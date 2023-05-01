use std::borrow::Cow;
use std::collections::HashSet;
use std::iter::once;
use std::path::Path;
use std::rc::Rc;

use derive_more::{Display, Error, From};

use crate::{debug, error, issue};
use crate::analyses::bindings::{FieldName, FieldNameStr, TypeNameStr, ValueBinding, ValueNameStr};
use crate::analyses::global_bindings::GlobalTypeBinding;
use crate::analyses::scopes::{ActiveScopeRef, ModuleCtx, ScopeChain};
use crate::analyses::types::{DeterminedReturnType, DeterminedType, FatType, Field, HasNullability, Nullability, OptionalType, ResolveCtx, RlReturnType, RlType, StructureType, Variance};
use crate::ast::NOMINALSCRIPT_PARSER;
use crate::ast::queries::{EXPORT_ID, FUNCTION, IMPORT, NOMINAL_TYPE, VALUE};
use crate::ast::tree_sitter::{TraversalState, TreeCreateError, TSCursor, TSNode, TSQueryCursor, TSTree};
use crate::ast::typed_nodes::{AstCatchParameter, AstFunctionDecl, AstImportStatement, AstNode, AstParameter, AstReturn, AstReturnType, AstThrow, AstType, AstTypeDecl, AstTypeIdent, AstValueDecl, AstValueIdent};
use crate::diagnostics::{FileDiagnostics, FileLogger, ProjectLogger, TypeLogger};
use crate::import_export::export::{Exports, Module};
use crate::import_export::import_ctx::ImportError;
use crate::import_export::ModulePath;
use crate::ProjectCtx;

#[derive(Debug, Clone, Display, From, Error)]
pub enum FatalTranspileError {
    TreeCreate(TreeCreateError),
}

/// Transpile (compile) a NominalScript file into TypeScript.
/// This will parse the exports. You can call [Module::finish] to finish transpiling.
///
/// If successful, the transpiled file (module) will be cached in `ctx`,
/// so calling again will just return the same result
pub(super) fn begin_transpile_file<'a>(
    script_path: &Path,
    ctx: ProjectCtx<'a>
) -> Result<&'a Module, Cow<'a, ImportError>> {
    let header = begin_transpile_file_no_log_err(script_path, ctx);
    if let Err(import_error) = header.as_ref() {
        let e = ProjectLogger::new(&ctx.diagnostics);
        error!(e, "failed to transpile '{}'", script_path.display();
            issue!("{}", import_error));
    }
    header
}

/// [begin_transpile_file] but doesn't log the import error
fn begin_transpile_file_no_log_err<'a>(
    script_path: &Path,
    ctx: ProjectCtx<'a>
) -> Result<&'a Module, Cow<'a, ImportError>> {
    ctx.import_ctx.resolve_auxillary_and_cache_transpile(script_path, |module_path| {
        begin_transpile_file_no_cache(script_path, module_path, ctx.diagnostics.file(module_path)).map_err(ImportError::from)
    })
}

/// [begin_transpile_file] but doesn't cache
pub(crate) fn begin_transpile_file_no_cache(
    script_path: &Path,
    module_path: &ModulePath,
    diagnostics: &FileDiagnostics
) -> Result<Module, FatalTranspileError> {
    let ast = NOMINALSCRIPT_PARSER.lock().parse_file(script_path)?;
    let mut module = Module::new(module_path.clone(), ast);
    module.with_module_data_mut(|_module_path, exports, ast, m| {
        begin_transpile_ast(m, ast, exports, diagnostics)
    });
    Ok(module)
}

enum Export<'tree> {
    Type { original_name: AstTypeIdent<'tree>, alias: AstTypeIdent<'tree> },
    Value { original_name: AstValueIdent<'tree>, alias: AstValueIdent<'tree> },
}

/// Transpiles the header AKA locally resolves declarations, resolves import paths, and adds exports
fn begin_transpile_ast<'tree>(
    m: &mut ModuleCtx<'tree>,
    ast: &'tree TSTree,
    exports: &mut Exports,
    diagnostics: &FileDiagnostics,
) {
    // TODO handle error nodes (don't just crash but ignore, then we can traverse error nodes and throw syntax errors)
    let e = FileLogger::new(diagnostics);
    let mut c = ast.walk();
    let mut qc = TSQueryCursor::new();
    let root_node = ast.root_node();
    let root_scope = m.scopes.root().downgrade();
    // Query all `nominal_type_declaraton`s and all imports.
    // Build map of nominal type names to their declarations
    // In global scope, build value map and map functions to their declarations.
    // Add imported decls (lazily forward resolved) to the nominal type names and global scope value map
    // Imports are lazily and narrowly resolved so that we allow inter-woven dependencies,
    // laziness tracks self-recursion so we explicitly fail on cycles
    for type_decl_match in qc.matches(&NOMINAL_TYPE, root_node) {
        let node = type_decl_match.capture(0).unwrap().node;
        let scope = m.scopes.containing(node, &mut c);
        let decl = AstTypeDecl::new(scope, node);
        scope.activate_ref().types.set(decl, &e);
    }
    for function_decl_match in qc.matches(&FUNCTION, root_node) {
        let node = function_decl_match.capture(0).unwrap().node;
        let scope = m.scopes.containing(node, &mut c);
        let decl = AstFunctionDecl::new(scope, node);
        scope.activate_ref().values.add_hoisted(decl, &e);
    }
    for value_decl_match in qc.matches(&VALUE, root_node) {
        let node = value_decl_match.capture(0).unwrap().node;
        let scope = m.scopes.containing(node, &mut c);
        let decl = AstValueDecl::new(scope, node);
        scope.activate_ref().values.add_sequential(decl, &e);
    }
    for import_stmt_match in qc.matches(&IMPORT, root_node) {
        let node = import_stmt_match.capture(0).unwrap().node;
        let scope = m.scopes.containing(node, &mut c);
        let import_path_idx = scope.activate_ref().next_import_path_idx();
        let import_stmt = AstImportStatement::new(scope, import_path_idx, node);
        scope.activate_ref().add_imported(import_stmt, &e);
    }
    // Query and prefill exports, and also add to scopes
    for export_id_match in qc.matches(&EXPORT_ID, root_node) {
        let value_export_id = export_id_match.capture_named("value_export_id").map(|c| AstValueIdent::new(c.node));
        let type_export_id = export_id_match.capture_named("nominal_export_id").map(|c| AstTypeIdent::new(c.node));
        let export_id_node = value_export_id.as_ref().map(|v| v.node).or_else(|| type_export_id.as_ref().map(|t| t.node))
            .expect("export_id_match should have either a value_export_id or a nominal_export_id");
        let export_alias_id_node = export_id_match.capture_named("export_alias_id").map(|c| c.node);

        let export = match (value_export_id, type_export_id) {
            (None, None) => panic!("export_id_match should have either value_export_id or nominal_export_id"),
            (Some(_), Some(_)) => panic!("export_id_match should have either value_export_id or nominal_export_id, not both"),
            (Some(value_export), None) => {
                let alias_id = export_alias_id_node.map_or_else(|| value_export.clone(), AstValueIdent::new);
                Export::Value { original_name: value_export, alias: alias_id }
            }
            (None, Some(type_export)) => {
                let alias_id = export_alias_id_node.map_or_else(|| type_export.clone(), AstTypeIdent::new);
                Export::Type { original_name: type_export, alias: alias_id }
            }
        };

        // Add to scope exports, and add to global exports if root scope
        let scope_ptr = m.scopes.containing(export_id_node, &mut c);
        let mut scope = scope_ptr.activate_ref();
        match export {
            Export::Type { original_name, alias } => {
                if scope == root_scope {
                    if let Some(decl) = scope.types.get(&original_name.name) {
                        exports.add_type(alias.name.clone(), decl.type_decl().box_clone());
                    }
                    // if no decl, add_exported logs an error
                }
                scope.types.add_exported(original_name, alias, &e);
            }
            Export::Value { original_name, alias } => {
                if scope == root_scope {
                    if let Some(decl) = scope.values.at_exact_pos(&original_name.name, original_name.node) {
                        exports.add_value(alias.name.clone(), decl.value_type().box_clone());
                    }
                    // if no decl, add_exported logs an error
                }
                scope.values.add_exported(original_name, alias, &e);
            }
        }
    }
    debug!(e, "** begin_transpile_ast done" => ast.root_node());
}

/// Finish transpiling an ast
pub(crate) fn finish_transpile<'tree>(
    ast: &'tree TSTree,
    m: &mut ModuleCtx<'tree>,
    ctx: &ResolveCtx<'_>,
) {
    // TODO: Add guards (in type analysis, but probably first in function decls we've already scanned. We have to wait until we're in the return block though)
    let e = ctx.logger();
    // Random operation cursor
    let mut c = ast.walk();
    let mut c2 = ast.walk();
    let mut skip_nodes = HashSet::new();
    // Run type analysis (and scope analysis and other dependent analyses)
    // Instead of querying here we do an inorder traversal because we need to do inorder
    // But Queries.SCOPE roughly matches what we're using
    let mut scopes = ScopeChain::new();
    let mut traversal_cursor = ast.walk();
    let mut traversal_state = TraversalState::Start;
    loop {
        let node = traversal_cursor.node();
        if let Some(scope) = m.scopes.denoted_by(node, &mut c) {
            if !traversal_state.is_up() {
                scopes.push(scope.clone().activate());
            } else {
                scopes.pop();
                if scopes.is_empty() {
                    debug_assert_eq!(node.kind(), "program");
                    break
                }
            }
        }
        let scope = scopes.top().unwrap().inactive_clone();

        // Nodes are traversed once or twice: once 'start' or 'down' or 'right',
        // once if they have children 'up'
        let mut skip_children = false;
        if skip_nodes.remove(&node) || node.is_marked() {
            skip_children = true
        } else if node.is_named() {
            debug!(e, "Visiting {:?} {}:\n  {:.40}", traversal_state, node.kind(), str::escape_debug(node.text()) => node);
            'outer: {
                match node.kind() {
                    "function_declaration" |
                    "generator_function_declaration" |
                    "function" |
                    "generator_function" |
                    "arrow_function" |
                    "method_definition" => {
                        let is_decl = node.kind() == "function_declaration" ||
                            node.kind() == "generator_function_declaration";
                        let is_arrow = node.kind() == "arrow_function";
                        // We want to reuse params if a function decl because we can infer
                        // param types in future calls. In an expression, we assign the type to the
                        // node to do this
                        let decl = match is_decl {
                            false => None,
                            true => {
                                let name_node = node.field_child("name").unwrap();
                                let name = ValueNameStr::of(name_node.text());
                                scopes.hoisted_in_top_scope(name).map(|x| x.down_to_fn_decl().expect("hoisted declaration isn't a function (TODO handle, probably just return None here)"))
                            }
                        };
                        let body = node.field_child("body").unwrap();
                        let has_fun_scope = m.scopes.setup_denoted_by(body, &mut c);
                        debug_assert!(has_fun_scope, "function body should have a scope");
                        let fun_scope_inactive = m.scopes.existing_denoted_by(body);
                        if !traversal_state.is_up() {
                            let mut fun_scope = fun_scope_inactive.activate_ref();
                            // TODO: Scope nominal type params as well
                            // Declare parameters in function scope and assign explicit types
                            fn set_params<'tree>(fun_scope: &mut ActiveScopeRef<'_, 'tree>, skip_nodes: &mut HashSet<TSNode<'tree>>, c2: &mut TSCursor<'tree>, params: impl IntoIterator<Item=Rc<AstParameter<'tree>>>) {
                                fun_scope.set_params(params.into_iter().map(|param| {
                                    skip_nodes.extend(param.node.named_children(c2)
                                        .filter(|param_child_node| Some(param_child_node) != param.value.as_ref()));
                                    param
                                }));
                            }
                            match (decl, node.field_child("parameters")) {
                                // Decl parameters
                                (Some(decl), _) => set_params(&mut fun_scope, &mut skip_nodes, &mut c2, decl.formal_params.clone()),
                                // Expression formal parameters
                                (None, Some(parameters_node)) => {
                                    set_params(&mut fun_scope, &mut skip_nodes, &mut c2, parameters_node.named_children(&mut c).map(|x| Rc::new(AstParameter::formal(&scope, x, is_arrow))))
                                },
                                // Arrow parameter
                                (None, None) => {
                                    debug_assert!(is_arrow);
                                    set_params(&mut fun_scope, &mut skip_nodes, &mut c2, once(Rc::new(AstParameter::single_arrow(node.field_child("parameter").unwrap()))))
                                }
                            };
                        } else {
                            // Infer return type, and check with the explicit return type if provided.
                            // If expression, also assign inferred func type to the node itself
                            let explicit_return_type_ast = node.field_child("nominal_return_type").map(|x| {
                                // x.mark();
                                AstReturnType::new(fun_scope_inactive, x.named_child(0).unwrap())
                            });
                            fn process_inferred_return_types<'tree>(explicit_return_type_ast: Option<AstReturnType<'tree>>, node: TSNode<'tree>, e: &FileLogger<'_>, ctx: &ResolveCtx<'_>, inferred_return_types: impl IntoIterator<Item=DeterminedReturnType<'tree>>) -> Option<RlReturnType> {
                                let mut inferred_return_types = inferred_return_types.into_iter();
                                match explicit_return_type_ast {
                                    None => {
                                        // Assign inferred return type
                                        let inferred_return_type = FatType::unify_all_returns(
                                            inferred_return_types.map(|inferred_return_type| inferred_return_type.type_.into_resolve(ctx).1),
                                            Variance::Bivariant,
                                            TypeLogger::ignore()
                                        );
                                        Some(RlReturnType::resolved(inferred_return_type))
                                    }
                                    Some(explicit_return_type_ast) => {
                                        // Check explicit return type
                                        let mut last_inferred_return_type_det = inferred_return_types.next().expect("function always at least has one return type (the implicit return)");
                                        while let Some(next_inferred_return_type_det) = inferred_return_types.next() {
                                            last_inferred_return_type_det.check_subtype(Some(explicit_return_type_ast.clone()), node, e, ctx);
                                            last_inferred_return_type_det = next_inferred_return_type_det;
                                        };
                                        // We can consume the last one
                                        last_inferred_return_type_det.check_subtype(Some(explicit_return_type_ast), node, e, ctx);
                                        None
                                    }
                                }
                            }
                            // It's easier to just check if there is a type assigned instead of checking
                            // if body itself is an expression, since there are many expression node
                            // types. If it's not than there will never be a type assigned
                            let custom_inferred_return_type = match m.typed_exprs.get(body) {
                                // We get seen return types in the scope
                                None => process_inferred_return_types(explicit_return_type_ast, node, &e, ctx, m.scopes.seen_return_types_of(fun_scope_inactive, &m.typed_exprs)),
                                // Body is expression and it's the return type
                                Some(inferred_return_type) => process_inferred_return_types(explicit_return_type_ast, node, &e, ctx, once(DeterminedReturnType::from(inferred_return_type.clone())))
                            };
                            match decl {
                                None => {
                                    // We need to set the expression's type
                                    let mut func_type = AstFunctionDecl::parse_common(&scope, node).3;
                                    if let Some(custom_inferred_return_type) = custom_inferred_return_type {
                                        func_type = func_type.with_return_type(custom_inferred_return_type, ctx)
                                    };
                                    m.typed_exprs.assign(node, DeterminedType {
                                        type_: func_type,
                                        defined_value: Some(node),
                                        explicit_type: None,
                                    });
                                }
                                Some(decl) => {
                                    if let Some(custom_inferred_return_type) = custom_inferred_return_type {
                                        // We need to set the decl's inferred return type
                                        decl.set_custom_inferred_return_type(custom_inferred_return_type, ctx);
                                    }
                                }
                            }
                        }
                    }
                    "catch_clause" => {
                        if !traversal_state.is_up() {
                            let catch_param = AstCatchParameter::new(node.field_child("parameter").unwrap());
                            let body = node.field_child("body").unwrap();
                            let mut body_scope = m.scopes.denoted_by(body, &mut c)
                                .expect("scope body is a statement_block, which should have a scope")
                                .activate_ref();

                            // Don't traverse binding ids
                            skip_nodes.insert(catch_param.node());
                            body_scope.values.set_catch_param(Rc::new(catch_param));
                        }
                    }
                    "variable_declarator" => {
                        let name_node = node.field_child("name").unwrap();
                        let name = ValueNameStr::of(name_node.text());
                        let decl = scopes.at_exact_pos(name, node).expect("decl should have been added at this variable declarator's position");
                        if !traversal_state.is_up() {
                            if let (Some(value), Some(type_)) = (decl.value, decl.type_.as_ref()) {
                                // Require value to be nominal type
                                m.typed_exprs.require(value, type_.clone())
                            }
                            // Don't traverse binding ids
                            skip_nodes.insert(name_node);
                            // We also don't traverse nominal type but it's unnecessary to add
                            // because that gets skipped anyways (deleted)
                        }
                        // Already assigned via decl, and the node itself is a statement
                        // m.typed_exprs.assign(node, nominal_type)
                    }
                    "return_statement" => {
                        if traversal_state.is_up() {
                            scopes.add_return(AstReturn {
                                node,
                                returned_value: node.named_child(0),
                            }, &e)
                        }
                    }
                    "throw_statement" => {
                        if traversal_state.is_up() {
                            scopes.add_throw(AstThrow {
                                node,
                                thrown_value: node.named_child(0),
                            }, &e)
                        }
                    }
                    "assignment_expression" => {
                        if traversal_state.is_up() {
                            let left = node.named_child(0).unwrap();
                            let right = node.named_child(1).unwrap();
                            let left_type = m.typed_exprs.get(left).cloned();
                            if let Some(left_type) = left_type.as_ref() {
                                m.typed_exprs.require(right, left_type.clone())
                            }
                            // Can't re-assign right type to the left id,
                            // because of issues with scope and loops/break,
                            // but we can assign it to the expression itself.
                            // Also we need to assign the type because assignments are expressions
                            // and thus can be inside other expressions
                            let assigned_type = left_type.or_else(|| m.typed_exprs.get(right).cloned());
                            if let Some(assigned_type) = assigned_type {
                                m.typed_exprs.assign(node, assigned_type)
                            }
                        }
                    }
                    // "augmented_assignent_expression":
                    // "binary_expression":
                    // "update_expression":
                    //   We will add support for declaring operations on nominal types, e.g.
                    //   `operation; Liters + Liters -> Liters`
                    //   `operation; Liters * Float -> Liters`
                    //   And then call `m.typed_operations.get(left, op, right)`
                    //   to ensure that such a declaration exists and get the return type
                    //   ...But not yet (TODO)
                    "ternary_expression" => {
                        if traversal_state.is_up() {
                            // let condition = node.named_child(0).unwrap();
                            let then = node.named_child(1).unwrap();
                            let else_ = node.named_child(2).unwrap();
                            let then_type = m.typed_exprs.get(then);
                            let else_type = m.typed_exprs.get(else_);
                            if let (Some(then_type), Some(else_type)) = (then_type, else_type) {
                                let unified = DeterminedType::check_not_disjoint_then_unify(Some(then_type.clone()), Some(else_type.clone()), node, &e, ctx);
                                if let Some(unified) = unified {
                                    m.typed_exprs.assign(node, unified)
                                }
                            }
                        }
                    }
                    "sequence_expression" => {
                        if traversal_state.is_up() {
                            let last = node.last_named_child().unwrap();
                            let last_type = m.typed_exprs.get(last);
                            if let Some(last_type) = last_type {
                                m.typed_exprs.assign(node, last_type.clone())
                            }
                        }
                    }
                    "member_expression" => {
                        if traversal_state.is_up() {
                            let object = node.named_child(0).unwrap();
                            let field_name_node = node.field_child("property").unwrap();
                            let field_name = FieldNameStr::of(field_name_node.text());
                            let is_nullable_access = node.child_of_kind("optional_chain", &mut c).is_some();
                            let object_type_det = m.typed_exprs.get(object);
                            if let Some(object_type_det) = object_type_det {
                                let field_type = object_type_det.member_type(field_name, is_nullable_access, node, &e, ctx);
                                m.typed_exprs.assign(node, field_type);
                            }
                        }
                    }
                    "subscript_expression" => {
                        if traversal_state.is_up() {
                            let array = node.named_child(0).unwrap();
                            let index = node.field_child("index").unwrap().text().parse::<usize>().ok();
                            let is_nullable_access = node.child_of_kind("optional_chain", &mut c).is_some();
                            let array_type_det = m.typed_exprs.get(array);
                            if let Some(array_type_det) = array_type_det {
                                let element_type = array_type_det.subscript_type(index, is_nullable_access, node, &e, ctx);
                                m.typed_exprs.assign(node, element_type);
                            }
                        }
                    }
                    "await_expression" => {
                        if traversal_state.is_up() {
                            let promise = node.named_child(0).unwrap();
                            let promise_type_det = m.typed_exprs.get(promise);
                            if let Some(promise_type_det) = promise_type_det {
                                let inner_type = promise_type_det.awaited_type(node, &e, ctx);
                                m.typed_exprs.assign(node, inner_type);
                            }
                        }
                    }
                    "call_expression" => {
                        if traversal_state.is_up() {
                            let func = node.named_child(0).unwrap();
                            let is_nullable_call = node.child_of_kind("optional_chain", &mut c).is_some();
                            let Some(args_node) = node.child_of_kind("arguments", &mut c) else {
                                error!(e, "Can't handle function which takes template string as args" => node);
                                break 'outer
                            };
                            if let Some(func_type) = m.typed_exprs.get(func) {
                                let return_type = func_type.call_type(
                                    // TODO: This arg and spread args
                                    None,
                                    args_node.named_children(&mut c).map(|arg_node| {
                                        (m.typed_exprs.get(arg_node), arg_node)
                                    }),
                                    is_nullable_call,
                                    node,
                                    &e,
                                    ctx
                                );
                                m.typed_exprs.assign(node, return_type);
                            }
                        }
                    }
                    "parenthesized_expression" => {
                        let expr = node.named_child(0).unwrap();
                        let type_ = node.field_child("nominal_type").map(|t| AstType::parse_annotation(&scope, t));
                        if !traversal_state.is_up() {
                            if let Some(type_) = type_ {
                                m.typed_exprs.require(expr, type_.clone());
                                m.typed_exprs.assign(node, type_);
                            }
                        } else if type_.is_none() {
                            let inferred_type = m.typed_exprs.get(expr);
                            if let Some(inferred_type) = inferred_type {
                                m.typed_exprs.assign(node, inferred_type.clone());
                            }
                        }
                    }
                    "this" |
                    "identifier" |
                    "shorthand_property_identifier" => {
                        skip_children = true;
                        let ident = ValueNameStr::of(node.text());
                        let def = scopes.at_pos(ident, node);
                        let Some(def) = def else {
                            error!(e, "Unresolved identifier: {}", ident => node);
                            break 'outer
                        };
                        let def_type = def.infer_type_det(Some(&m.typed_exprs), ctx);
                        m.typed_exprs.assign(node, def_type);
                    }
                    "true" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, GlobalTypeBinding::get(TypeNameStr::of("True")).expect("builtin True type should exist").type_det());
                    }
                    "false" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, GlobalTypeBinding::get(TypeNameStr::of("False")).expect("builtin False type should exist").type_det());
                    }
                    "null" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, DeterminedType::intrinsic(RlType::NULL.clone()));
                    }
                    "number" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, GlobalTypeBinding::get(TypeNameStr::of_number_literal(node.text())).expect("builtin number literal type should exist").type_det());
                    }
                    "string" |
                    "template_string" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, GlobalTypeBinding::get(TypeNameStr::of("String")).expect("builtin String type should exist").type_det());
                    }
                    "regex" => {
                        skip_children = true;
                        m.typed_exprs.assign(node, GlobalTypeBinding::get(TypeNameStr::of("Regex")).expect("builtin Regex type should exist").type_det());
                    }
                    "object" => {
                        if traversal_state.is_up() {
                            let field_nodes = node.named_children(&mut c);
                            let mut type_override = None;
                            let mut fields = Vec::with_capacity(field_nodes.size_hint().0);
                            for field_node in field_nodes {
                                match field_node.kind() {
                                    "pair" => {
                                        let key = field_node.named_child(0).unwrap();
                                        let value = field_node.named_child(1).unwrap();
                                        let value_type = m.typed_exprs.get(value).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        fields.push(Field { name: FieldName::new(key.text()), type_: OptionalType::required(value_type) })
                                    },
                                    "shorthand_property_identifier" => {
                                        let name = field_node.named_child(0).unwrap();
                                        let name_type = m.typed_exprs.get(name).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        fields.push(Field { name: FieldName::new(name.text()), type_: OptionalType::required(name_type) })
                                    }
                                    "method_definition" => {
                                        let name = field_node.named_child(0).unwrap();
                                        // Since the field node is a function declaration the type is directly assigned to it
                                        let type_ = m.typed_exprs.get(field_node).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        fields.push(Field { name: FieldName::new(name.text()), type_: OptionalType::required(type_) })
                                    }
                                    "spread_element" => {
                                        let expr = field_node.named_child(0).unwrap();
                                        let spread_type = m.typed_exprs.get(expr).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        let spread_is_nullable = matches!(spread_type.nullability(), Nullability::Nullable);
                                        if spread_is_nullable {
                                            error!(e, "Can't object-spread nullable type because it's a type error to object-spread null" => field_node);
                                        }
                                        match spread_type {
                                            FatType::Any => {
                                                type_override = Some(GlobalTypeBinding::get(TypeNameStr::of("Array")).expect("builtin Array type should exist").decl.resolve(ctx).clone().into_type());
                                                break
                                            }
                                            FatType::Never { nullability } => {
                                                if matches!(nullability, Nullability::NonNullable) {
                                                    type_override = Some(FatType::NEVER.clone());
                                                }
                                                break
                                            }
                                            // TODO: fix holes in general
                                            FatType::Hole { .. } => panic!("fix holes in general"),
                                            _ => {}
                                        }
                                        match spread_type.into_structure() {
                                            Some(StructureType::Object { field_types }) => {
                                                // spread_field_arena.extend(field_types.len());
                                                fields.extend(field_types.into_iter().map(|Field { name, mut type_ }| {
                                                    type_.make_optional_if(spread_is_nullable);
                                                    Field { name, type_ }
                                                }))
                                            }
                                            _ => {
                                                error!(e, "Can't object-spread non-object type" => field_node);
                                            }
                                        }
                                    }
                                    _ => panic!("Unexpected field node kind: {}", field_node.kind())
                                }
                            }
                            let type_ = type_override.unwrap_or_else(|| {
                                fields.dedup_by(|Field { name: name1, type_: _ }, Field { name: name2, type_: _ }| name1 == name2);
                                FatType::object(fields)
                            });
                            m.typed_exprs.assign(node, DeterminedType {
                                // ???: We can do better be taking the thin types to construct a thin object as well,
                                //    but does it actually improve the thin signature?
                                type_: RlType::resolved(type_),
                                defined_value: Some(node),
                                explicit_type: None,
                            });
                        }
                    }
                    "array" => {
                        if traversal_state.is_up() {
                            let elem_nodes = node.named_children(&mut c);
                            let mut type_override = None;
                            let mut elems = Vec::with_capacity(elem_nodes.size_hint().0);
                            let mut is_array = false;
                            for elem_node in elem_nodes {
                                match elem_node.kind() {
                                    "spread_element" => {
                                        let expr = elem_node.named_child(0).unwrap();
                                        let spread_type = m.typed_exprs.get(expr).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        let spread_is_nullable = matches!(spread_type.nullability(), Nullability::Nullable);
                                        if spread_is_nullable {
                                            error!(e, "Can't array-spread nullable type because it's a type error to array-spread null" => elem_node);
                                        }
                                        match spread_type {
                                            FatType::Any => {
                                                type_override = Some(GlobalTypeBinding::get(TypeNameStr::of("Array")).expect("builtin Array type should exist").decl.resolve(ctx).clone().into_type());
                                                break
                                            }
                                            FatType::Never { nullability } => {
                                                if matches!(nullability, Nullability::NonNullable) {
                                                    type_override = Some(FatType::NEVER.clone());
                                                }
                                                break
                                            }
                                            // TODO: fix holes in general
                                            FatType::Hole { .. } => panic!("fix holes in general"),
                                            _ => {}
                                        }
                                        match spread_type.into_structure() {
                                            Some(StructureType::Tuple { element_types }) => {
                                                elems.extend(element_types);
                                            }
                                            Some(StructureType::Array { element_type }) => {
                                                is_array = true;
                                                elems.push(OptionalType::optional(*element_type));
                                            }
                                            _ => {
                                                error!(e, "Can't array-spread non-array, non-tuple type" => elem_node);
                                            }
                                        }
                                    }
                                    _ => {
                                        let elem_type = m.typed_exprs.get(elem_node).map_or(FatType::Any, |t| t.type_.resolve(ctx).clone());
                                        elems.push(OptionalType::required(elem_type));
                                    },
                                }
                            }
                            let type_ = type_override.unwrap_or_else(|| {
                                if is_array {
                                    FatType::array(FatType::unify_all(
                                        elems.into_iter().map(|e| e.collapse_optionality_into_nullability()),
                                        Variance::Bivariant,
                                        TypeLogger::ignore()
                                    ))
                                } else {
                                    FatType::tuple(elems)
                                }
                            });
                            m.typed_exprs.assign(node, DeterminedType {
                                // ???: We can do better be taking the thin types to construct a thin object as well,
                                //    but does it actually improve the thin signature?
                                type_: RlType::resolved(type_),
                                defined_value: Some(node),
                                explicit_type: None,
                            });
                        }
                    }
                    "nominal_wrap_expression" |
                    "nominal_wrap_unchecked_expression" => {
                        if !traversal_state.is_up() {
                            let is_checked = node.kind() == "nominal_wrap_expression";
                            let nominal_type = AstType::new(&scope, node.named_child(0).unwrap());
                            let expr = node.named_child(1).unwrap();
                            if is_checked {
                                m.typed_exprs.runtime_require(expr, nominal_type.clone());
                            }
                            m.typed_exprs.assign(node, nominal_type);
                            node.mark_children_except(expr, &mut c);
                        }
                    }
                    "nominal_type_annotation" |
                    "nominal_type_identifier" |
                    "parenthesized_nominal_type" |
                    "generic_nominal_type" |
                    "object_nominal_type" |
                    "array_nominal_type" |
                    "tuple_nominal_type" |
                    "function_nominal_type" |
                    "nullable_nominal_type" => {
                        skip_children = true;
                        error!(e, "Unhandled nominal type (nominal type in currently unsupported position)" => node);
                    }
                    _ => {}
                }
            }
        }

        if skip_children {
            // Note that lastMove is immediately set afterwards and gotoInorder doesn't
            // treat it as 'nextMove', so we aren't necessarily going up; we are only
            // not going down because lastMove is actually passed to gotoInorder to not
            // get a 'down' -> ... -> 'up' -> 'down' infinite loop. Maybe it should be
            // made more clear and something besides lastMove should be used instead...
            traversal_state = TraversalState::Up;
        }
        traversal_state = traversal_cursor.goto_preorder(traversal_state);
    };
    debug!(e, "** finish_transpile traversal done" => ast.root_node());

    // Type analysis (and scope analysis etc.) are done.
    // But not we need to throw errors for bad types
    // (we wait until analysis is done because some of these errors are due to uninferred types,
    // and we may infer types at weird future times which make them disappear.
    // So it's easier to just wait until ALL analysis is done to raise these uninferred-errors and the general category)
    m.typed_exprs.check_all(&e, ctx);

    // Roadmap (partially outdated)
    // - Query all `function_declaration`s, `method_declaration`s, `function_signature`s, etc., in every scope build a map of the static functions' types. Remove their nominal type annotations, replace them with guarded versions and which call a generated unguarded version. Also build a map of them as ordinary global (lambda) constants to their nominal function types (already somewhat done, some of the others are somewhat done as well)
    // - Run type checking: remove and check every type annotation we can. Similar to the last step, we build a map of dynamic functions' (lambdas) types and generate guarded/unguarded versions (the difference is that we're also doing inference, so previous statements don't get the inferred types), and we map ordinary values to their types. All calls go to guarded functions by default since they have the original name; we replace checked calls with their unguarded versions if we're absolutely sure the types are ok. We also add inline guards to nominal type annotated expressions and initializers/assignments of nominal type annotated values when we can't infer their type, including annotated exceptions in `catch` clauses. Whenever we infer a type but it is wrong, we generate a type error but continue to generate with guards (which, unless we messed up somewhere, will always throw a runtime error). We remove all nominal type annotations from the analyzed declarations and checked identifiers/expressions, to mark they've been analyzed and because we need to strip them anyways. To summarize:
    //   - Each scope has 2 maps: functions and values. There is also a map of all (global) nominal type declarations
    //   - Imports have already been added to the global scope maps
    //   - We add analyzed declarations to a map in each scope
    //   - We remove type annotations of everything we analyze
    //   - We replace inferred and type-checked function and lambda calls with their unguarded versions
    //   - We add guards to nominal type annotated expressions and initializer/assignments of nominal type annotated values when we can't infer. Function calls which we can't infer already have guards
    //   - Whenever we infer a type but it is wrong, we generate a type error but continue to generate with guards
    // - Generate more type errors error for any nominal type annotation we don't handle
    // - Strip unused guarded / unguarded versions of functions which don't get exported
    //
    // In particular, remember to remove:
    // - FunctionDecl and ValueDecl nominal-type annotations
    // - Query nominal imports any exports (we already handle them and warn for child scopes)
    // and then log errors for anything we don't remove
    debug!(e, "** finish_transpile done" => ast.root_node());
}