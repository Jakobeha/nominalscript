use std::borrow::Cow;
use std::collections::HashSet;
use std::iter::once;
use std::path::Path;
use std::rc::Rc;

use derive_more::{Display, Error, From};

use crate::{error, issue};
use crate::analyses::bindings::{FieldNameStr, ValueBinding, ValueNameStr};
use crate::analyses::scopes::{ModuleCtx, ScopeChain};
use crate::analyses::types::{DeterminedReturnType, DeterminedType, FatType, ResolveCtx, ResolvedLazyTrait, ReturnType, RlReturnType, Variance};
use crate::ast::NOMINALSCRIPT_PARSER;
use crate::ast::queries::{EXPORT_ID, FUNCTION, IMPORT, NOMINAL_TYPE, VALUE};
use crate::ast::tree_sitter::{TraversalState, TreeCreateError, TSQueryCursor, TSTree};
use crate::ast::typed_nodes::{AstFunctionDecl, AstImportStatement, AstParameter, AstReturn, AstReturnType, AstThrow, AstType, AstTypeDecl, AstTypeIdent, AstValueDecl, AstValueIdent};
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
fn begin_transpile_file<'a>(
    script_path: &Path,
    ctx: &'a ProjectCtx<'_>
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
    ctx: &'a ProjectCtx<'_>
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
    let mut e = FileLogger::new(diagnostics);
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
}

/// Finish transpiling an ast
pub(crate) fn finish_transpile<'tree>(
    ast: &'tree TSTree,
    m: &mut ModuleCtx<'tree>,
    ctx: &ResolveCtx<'_>,
) {
    // TODO: Add guards (in type analysis, but probably first in function decls we've already scanned. We have to wait until we're in the return block though)
    let mut e = ctx.logger();
    // Random operation cursor
    let mut c = ast.walk();
    let mut c2 = ast.walk();
    let mut skip_nodes = HashSet::new();
    // Run type analysis (and scope analysis and other dependent analyses)
    // Instead of querying here we do an inorder traversal because we need to do inorder
    // But Queries.SCOPE roughly matches what we're using
    let mut scopes = ScopeChain::at_root(ast.root_node(), m.scopes.root().clone().activate());
    let mut traversal_cursor = ast.walk();
    let mut traversal_state = TraversalState::Start;
    while !traversal_state.is_end() {
        let node = traversal_cursor.node();
        if let Some(scope) = m.scopes.denoted_by(node, &mut c) {
            if traversal_state != TraversalState::Up {
                scopes.push(node, scope.clone().activate());
            } else {
                scopes.pop();
            }
        }
        let (_scope_node, scope) = scopes.top().unwrap();
        let scope = scope.inactive_clone();

        // Nodes are traversed once or twice: once 'start' or 'down' or 'right',
        // once if they have children 'up'
        let mut skip_children = false;
        if skip_nodes.remove(&node) || node.is_marked() {
            skip_children = true
        } else if node.is_named() {
            match node.kind() {
                "function_declaration" |
                "generator_function_declaration" |
                "function" |
                "generator_function" |
                "arrow_function" => {
                    let is_decl = node.kind() == "function_declaration" ||
                        node.kind() == "generator_function_declaration";
                    let is_arrow = node.kind() == "arrow_function";
                    // We want to reuse params if a function decl because we can infer
                    // param types in future calls. In an expression, we assign the type to the
                    // node to do this
                    let decl = match is_decl {
                        false => None,
                        true => {
                            let name = ValueNameStr::of(node.field_child("name").unwrap().text());
                            scopes.hoisted_in_top_scope(name).map(|x| x.down_to_fn_decl().expect("hoisted declaration isn't a function (TODO handle, probably just return None here)"))
                        }
                    };
                    let body = node.field_child("body").unwrap();
                    let fun_scope_inactive = m.scopes.denoted_by(body, &mut c).expect("function body should have a scope");
                    if !traversal_state.is_up() {
                        let mut fun_scope = fun_scope_inactive.activate_ref();
                        // TODO: Scope nominal type params as well
                        // Declare parameters in function scope and assign explicit types
                        let set_params = |params: impl IntoIterator<Item=Rc<AstParameter>>| fun_scope.set_params(params.into_iter().map(|param| {
                            skip_nodes.extend(param.node.named_children(&mut c2)
                                .filter(|param_child_node| Some(param_child_node) != param.value.as_ref()))
                        }));
                        match (decl, node.field_child("parameters")) {
                            // Decl parameters
                            (Some(decl), _) => set_params(decl.formal_params.clone()),
                            // Expression formal parameters
                            (None, Some(parameters_node)) => {
                                set_params(parameters_node.named_children(&mut c).map(|x| Rc::new(AstParameter::formal(&scope, x, is_arrow))))
                            },
                            // Arrow parameter
                            (None, None) => {
                                debug_assert!(is_arrow);
                                set_params(once(Rc::new(AstParameter::single_arrow(node.field_child("parameter").unwrap()))))
                            }
                        };
                    } else {
                        // Infer return type, and check with the explicit return type if provided.
                        // If expression, also assign inferred func type to the node itself
                        let explicit_return_type_ast = node.field_child("nominal_return_type").map(|x| {
                            x.mark();
                            AstReturnType::new(fun_scope_inactive, x.named_child(0).unwrap())
                        });
                        let process_inferred_return_types = |inferred_return_types: impl IntoIterator<Item = DeterminedReturnType<'tree>>| -> Option<RlReturnType> {
                            let mut inferred_return_types = inferred_return_types.into_iter();
                            match explicit_return_type_ast {
                                None => {
                                    // Assign inferred return type
                                    let inferred_return_type = FatType::unify_all(
                                        inferred_return_types.map(|inferred_return_type| inferred_return_type.type_.into_resolve(ctx).1),
                                        Variance::Bivariant,
                                        TypeLogger::ignore()
                                    );
                                    Some(RlReturnType::resolved(ReturnType::Type(inferred_return_type)))
                                }
                                Some(explicit_return_type_ast) => {
                                    // Check explicit return type
                                    let mut last_inferred_return_type_det = inferred_return_types.next();
                                    while let Some(next_inferred_return_type_det) = inferred_return_types.next() {
                                        last_inferred_return_type_det.check_subtype(Some(explicit_return_type_ast.clone()), node, &e, ctx);
                                        last_inferred_return_type_det = next_inferred_return_type_det;
                                    };
                                    // We can consume the last one
                                    last_inferred_return_type_det.check_subtype(Some(explicit_return_type_ast), node, &e, ctx);
                                    None
                                }
                            }
                        };
                        // It's easier to just check if there is a type assigned instead of checking
                        // if body itself is an expression, since there are many expression node
                        // types. If it's not than there will never be a type assigned
                        let custom_inferred_return_type = match m.typed_exprs.get(body) {
                            None => process_inferred_return_types(m.scopes.seen_return_types_of(fun_scope_inactive, &m.typed_exprs)),
                            Some(inferred_return_type) => process_inferred_return_types(once(DeterminedReturnType::from(inferred_return_type.clone())))
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
                "variable_declarator" => {
                    let name_node = node.field_child("name").unwrap();
                    let name = ValueNameStr::of(name_node);
                    let nominal_type = node.field_child("nominal_type")
                        .map(|x| AstType::of_annotation(&scope, x));
                    let value = node.field_child("value");
                    let decl = scopes.at_exact_pos(name, node).expect("decl should have been added at this variable declarator's position");
                    assert_eq!(decl.type_.map(|x| x.node), nominal_type.as_ref().map(|x| x.node));
                    if !traversal_state.is_up() {
                        if let (Some(value), Some(nominal_type)) = (value, nominal_type) {
                            // Require value to be nominal type
                            m.typed_exprs.require(value, nominal_type)
                        }
                        // Don't traverse binding ids
                        skip_nodes.insert(name_node)
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
                        let left_type = m.typed_exprs.get(left);
                        if let Some(left_type) = left_type {
                            m.typed_exprs.require(right, left_type)
                        }
                        // Can't re-assign right type to the left id,
                        // because of issues with scope and loops/break,
                        // but we can assign it to the expression itself.
                        // Also we need to assign the type because assignments are expressions
                        // and thus can be inside other expressions
                        let assigned_type = left_type.or_else(|| m.typed_exprs.get(right));
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
                            m.typed_exprs.assign(node, last_type)
                        }
                    }
                }
                "member_expression" => {
                    if traversal_state.is_up() {
                        let object = node.named_child(0).unwrap();
                        let field_name = FieldNameStr::of(node.field_child("property").unwrap().text());
                        let is_nullable_access = node.child_of_type("optional_chain").is_some();
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
                        let is_nullable_access = node.child_of_type("optional_chain").is_some();
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
                            let inner_type = promise_type_det.awaited_type(is_nullable_access, node, &e, ctx);
                            m.typed_exprs.assign(node, inner_type);
                        }
                    }
                }
                // TODO: Remaining cases
                _ => {}
            }
        }
    /*
          case 'call_expression': {
            if (lastMove === 'up') {
              const func = node.namedChild(0)!
              const funcIsNullable = node.childOfType('optional_chain') !== null
              const args = node.childOfType('arguments')
              if (args === null) {
                logError(
                  'Can\'t handle function which takes template string as args', node
                )
                break
              }
              const funcTypeRaw = scopes.inferType(func)

              if (funcTypeRaw !== null) {
                const funcType = await ctx.nominalTypeDeclMap.superStructure2(funcTypeRaw) ?? funcTypeRaw
                if (!NominalTypeShape.isFunction(funcType.shape)) {
                  logError(
                    'Can\'t call non-function', func,
                    funcType.ast !== null ? ['note: type is here', funcType.ast.node] : null
                  )
                } else {
                  if (funcType.shape.isNullable && !funcIsNullable) {
                    logError(
                      'Can\'t call nullable function without optional chain', func,
                      funcType.ast !== null ? ['note: type is here', funcType.ast.node] : null
                    )
                  }
                  const paramLenRange = NominalFunctionType.paramLenRange(funcType.shape)
                  if (args.namedChildren.length < paramLenRange.min) {
                    logError(
                      `Too few arguments to function, expected ${paramLenRange.min} but got ${args.namedChildren.length}`,
                      args
                    )
                  } else if (args.namedChildren.length > paramLenRange.max) {
                    logError(
                      `Too many arguments to function, expected ${paramLenRange.max} but got ${args.namedChildren.length}`,
                      args
                    )
                  }
                  for (let i = 0; i < args.namedChildren.length; i++) {
                    const arg = args.namedChildren[i]
                    const paramType = NominalFunctionType.paramType(funcType.shape, i)
                    if (paramType !== 'any' && paramType !== 'out-of-range') {
                      ctx.typedExprs.require(arg, {
                        shape: paramType,
                        ast: funcType.ast
                      })
                    }
                  }

                  if (funcType.shape.returnValue !== 'any') {
                    ctx.typedExprs.assign(node, {
                      shape: funcType.shape.returnValue,
                      ast: funcType.ast
                    })
                  }
                }
              }
            }
            break
          }
          case 'parenthesized_expression': {
            if (lastMove !== 'up') {
              const expr = node.namedChild(0)!
              const type = mapNullable(
                node.childOfType('nominal_type_annotation'),
                x => { x.delete(); return new NominalType(x.namedChild(0)!) }
              )
              if (type !== null) {
                ctx.typedExprs.require(expr, type.asInferred)
                ctx.typedExprs.assign(node, type.asInferred)
              }
            } else {
              const expr = node.namedChild(0)!
              const typeAnnotation = node.childOfType('nominal_type_annotation')
              if (typeAnnotation === null) {
                const inferredType = scopes.inferType(expr)
                if (inferredType !== null) {
                  ctx.typedExprs.assign(node, inferredType)
                }
              }
            }
            break
          }
          case 'this':
          case 'identifier':
          case 'shorthand_property_identifier': {
            const def = scopes.get(node.text, node)
            if (def === null) {
              logError(`Unresolved identifier '${node.text}'`, node)
              break
            }
            if (def instanceof AstBinding) {
              def.addUse(node)
              const type = await def.inferTypeFor(node)
              if (type === null) break
              ctx.typedExprs.assign(node, type)
            } else if (def instanceof GlobalBinding) {
              if (def.type === null) break
              ctx.typedExprs.assign(node, {
                shape: def.type,
                ast: { node }
              })
            } else {
              throw new Error(`Unexpected binding type ${def.constructor.name}`)
            }

            skipChildren = true // there are no children
            break
          }
          case 'true':
          case 'false': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.BOOLEAN,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'null': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.NULL,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'number': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.number(node.text),
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'string':
          case 'template_string': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.STRING,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'regex': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.REGEX,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'object': {
            if (lastMove === 'up') {
              let fields: Array<[string | number, NominalTypeShape]> | null = []
              for (const child of node.namedChildren) {
                switch (child.type) {
                  case 'pair': {
                    const key = child.namedChild(0)!
                    const value = child.namedChild(1)!
                    const type = scopes.inferType(value)?.shape ?? NominalTypeShape.ANY
                    fields.push([key.text, type])
                    break
                  }
                  case 'shorthand_property_identifier': {
                    const name = child.namedChild(0)!
                    const type = scopes.inferType(name)?.shape ?? NominalTypeShape.ANY
                    fields.push([name.text, type])
                    break
                  }
                  case 'spread_element': {
                    const expr = child.namedChild(0)!
                    const spreadType = scopes.inferType(expr)
                    if (spreadType === null) {
                      fields = null
                    } else {
                      const spreadObjType = (await ctx.nominalTypeDeclMap.superStructure2(spreadType))?.shape ?? null
                      if (spreadObjType === null || !NominalTypeShape.isObject(spreadObjType)) {
                        logError(
                          'Cannot spread non-object type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          spreadObjType === null ? null : [`note: actual structure is ${NominalTypeShape.print(spreadObjType)}`]
                        )
                        fields = null
                      } else if (spreadObjType.isNullable) {
                        logError(
                          'Cannot spread nullable object type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          [`note: actual structure is ${NominalTypeShape.print(spreadObjType)}`]
                        )
                        fields = null
                      } else {
                        fields.push(...spreadObjType.fields)
                      }
                    }
                    break
                  }
                  case 'method_definition': {
                    const name = child.namedChild(0)!
                    const type = scopes.inferType(child)?.shape ?? NominalTypeShape.FUNCTION
                    fields.push([name.text, type])
                    break
                  }
                  default:
                    throw new Error(`unhandled object literal child type: ${child.type}`)
                }
                if (fields === null) {
                  break
                }
              }
              fields = mapNullable(fields, x => ArrayUtil.dedupedBy(x, ([key, _]) => key))

              ctx.typedExprs.assign(node, {
                shape: fields === null
                  ? NominalTypeShape.OBJECT
                  : NominalTypeShape.object(fields),
                ast: { node }
              })
            }
            break
          }
          case 'array': {
            if (lastMove === 'up') {
              let elems: NominalTypeShape[] | NominalTypeShape | null = []
              const pushElems = (...newElems: NominalTypeShape[]): NominalTypeShape[] | NominalTypeShape => {
                if (elems! instanceof Array) {
                  elems.push(...newElems)
                  return elems
                } else {
                  return GeneratorUtil.last(NominalTypeShape.tryUnifyAll([elems, ...newElems]))!
                }
              }
              const pushArray = (newElem: NominalTypeShape): NominalTypeShape[] | NominalTypeShape => {
                if (elems! instanceof Array) {
                  return GeneratorUtil.last(NominalTypeShape.tryUnifyAll([...elems, newElem]))!
                } else {
                  return GeneratorUtil.last(NominalTypeShape.tryUnify(elems, newElem, '_', '_', false))!
                }
              }
              for (const child of node.namedChildren) {
                switch (child.type) {
                  case 'spread_element': {
                    const expr = child.namedChild(0)!
                    const spreadType = scopes.inferType(expr)
                    if (spreadType === null) {
                      elems = null
                    } else {
                      const spreadArrayType = (await ctx.nominalTypeDeclMap.superStructure2(spreadType))?.shape ?? null
                      if (spreadArrayType === null ||
                          (!NominalTypeShape.isArray(spreadArrayType) && !NominalTypeShape.isTuple(spreadArrayType))) {
                        logError(
                          'Cannot spread non-array and non-tuple type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          spreadArrayType === null ? null : [`note: actual structure is ${NominalTypeShape.print(spreadArrayType)}`]
                        )
                        elems = null
                      } else if (spreadArrayType.isNullable) {
                        logError(
                          'Cannot spread nullable array type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          [`note: actual structure is ${NominalTypeShape.print(spreadArrayType)}`]
                        )
                        elems = null
                      } else if (NominalTypeShape.isTuple(spreadArrayType)) {
                        elems = pushElems(...spreadArrayType.elems)
                      } else {
                        elems = pushArray(spreadArrayType.elem)
                      }
                    }
                    break
                  }
                  default: {
                    elems = pushElems(scopes.inferType(child)?.shape ?? NominalTypeShape.ANY)
                    break
                  }
                }
                if (elems === null) {
                  break
                }
              }

              ctx.typedExprs.assign(node, {
                shape: elems === null
                  ? NominalTypeShape.ARRAY
                  : elems instanceof Array
                    ? NominalTypeShape.tuple(elems)
                    : NominalTypeShape.array(elems),
                ast: { node }
              })
            }
            break
          }
          case 'nominal_wrap_expression':
          case 'nominal_wrap_unchecked_expression': {
            if (lastMove !== 'up') {
              const nominalType = new NominalType(node.namedChild(0)!)
              const expr = node.namedChild(1)!
              if (node.type === 'nominal_wrap_expression') {
                ctx.typedExprs.rtRequire(expr, nominalType.asInferred)
              }
              ctx.typedExprs.assign(node, nominalType.asInferred)
              node.replaceWithChild(expr)
            }
            break
          }
          case 'nominal_type_annotation':
          case 'nominal_type_identifier':
          case 'parenthesized_nominal_type':
          case 'generic_nominal_type':
          case 'object_nominal_type':
          case 'array_nominal_type':
          case 'tuple_nominal_type':
          case 'function_nominal_type':
          case 'nullable_nominal_type':
            logError(
              'Unhandled nominal type (nominal type in currently unsupported position)', node
            )
            skipChildren = true
            break
          default:
            break
        }
      } */

        if skip_children {
            // Note that lastMove is immediately set afterwards and gotoInorder doesn't
            // treat it as 'nextMove', so we aren't necessarily going up; we are only
            // not going down because lastMove is actually passed to gotoInorder to not
            // get a 'down' -> ... -> 'up' -> 'down' infinite loop. Maybe it should be
            // made more clear and something besides lastMove should be used instead...
            traversal_state = TraversalState::Up;
        }
        traversal_cursor.goto_preorder(traversal_state);
    };

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
}