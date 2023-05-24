use std::cell::Cell;
use std::ops::{Deref, DerefMut};
use indexmap::IndexMap;
use type_sitter_lib::UntypedNode;
use typed_arena_nomut::Arena;
use crate::{impl_has_ann_enum, impl_has_ann_record_struct};
use crate::misc::arena::{AssocArena, IdentityRef, IndexArena};
use crate::semantic::ann::Ann;
use crate::semantic::def::{OwnedTypeDef, OwnedValueDef};
use crate::semantic::expr::{Expr, OwnedExpr, OwnedType};
pub use toplevel::*;
use crate::analyses::bindings::{TypeNameStr, ValueNameStr};

mod toplevel;

/// e.g. toplevel scope, class scope, function scope
pub type Scope<'tree> = IdentityRef<'tree, OwnedScope<'tree>>;
#[derive(Debug)]
pub struct OwnedScope<'tree> {
    /// The scope node
    pub ann: Ann<'tree>,
    /// The parent scope
    pub parent: Option<Scope<'tree>>,
    /// The child scopes
    children: Arena<OwnedScope<'tree>>,
    /// Value definitions
    pub value_defs: AssocArena<&'tree ValueNameStr, OwnedValueDef<'tree>>,
    /// Type definitions
    pub type_defs: AssocArena<&'tree TypeNameStr, OwnedTypeDef<'tree>>,
    /// Value expressions
    pub exprs: IndexArena<OwnedExpr<'tree>>,
    /// Types
    pub types: IndexArena<OwnedType<'tree>>,
    /// Scope `return` or `throw` if present
    exit: Cell<Option<ScopeExit<'tree>>>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeExit<'tree> {
    Return {
        /// Source location
        ann: Ann<'tree>,
        /// Returned expression
        expr: Expr<'tree>
    },
    Throw {
        /// Source location
        ann: Ann<'tree>,
        /// Returned expression
        expr: Expr<'tree>
    }
}

impl_has_ann_record_struct!(OwnedScope);
impl_has_ann_enum!(ScopeExit { Return, Throw });

impl<'tree> OwnedScope<'tree> {
    /// Create a new, empty scope
    fn new(ann: Ann<'tree>, parent: Option<Scope<'tree>>) -> Self {
        Self {
            ann,
            parent,
            children: Arena::new(),
            value_defs: AssocArena::new(),
            type_defs: AssocArena::new(),
            exprs: IndexArena::new(),
            types: IndexArena::new(),
            exit: Cell::new(None),
        }
    }
}

impl<'tree> Scope<'tree> {
    /// Add a child scope
    pub fn add_child(self, ann: Ann<'tree>) -> Scope<'tree> {
        let child = OwnedScope::new(ann, Some(self));
        self.children.alloc(child).into()
    }

    /// Iterate child scopes
    pub fn children(&self) -> impl Iterator<Item=Scope<'tree>> {
        self.children.iter().map(|child| child.into())
    }

    /// Add an exit
    pub fn add_exit(&self, exit: ScopeExit<'tree>) {
        // TODO: assert that there is no exit yet.
        //   If there is one, log an error and set to the earlier exit
        self.exit.set(Some(exit))
    }

    /// Get the current exit
    pub fn exit(&self) -> Option<ScopeExit<'tree>> {
        self.exit.get()
    }
}

/// A local scope: contains all of the type bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
#[derive(Debug)]
pub struct TypeScope<'tree> {
    hoisted: HashMap<TypeName, Box<DynAstTypeBinding<'tree>>>,
    exported: HashMap<TypeName, ExportedId<'tree, TypeName>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedId<'tree, Name> {
    pub alias_node: TSNode<'tree>,
    pub name: Name
}

impl<'tree> Scope<'tree> {
    pub(super) fn new(node: TSNode<'tree>, parent: Option<&InactiveScopePtr<'tree>>) -> Self {
        Scope {
            node,
            did_set_params: false,
            imported_script_paths: Vec::new(),
            values: ValueScope::new(),
            types: TypeScope::new(),
            resolve_cache: ResolveCache::new(),
            parent: parent.map_or_else(WeakScopePtr::new, InactiveScopePtr::downgrade),
            is_being_mutated: false,
            _pinned: PhantomPinned
        }
    }

    pub fn set_params(&mut self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        assert!(!self.did_set_params, "set_params() can only be called once per scope");
        self.values.set_params(params)
    }

    pub fn next_import_path_idx(&self) -> usize {
        self.imported_script_paths.len()
    }

    pub fn add_imported(&mut self, import_stmt: AstImportStatement<'tree>, e: &FileLogger<'_>) {
        self.imported_script_paths.push(import_stmt.path);
        for imported_type in import_stmt.imported_types {
            self.types.add_imported(imported_type, e);
        }
        for imported_value in import_stmt.imported_values {
            self.values.add_imported(imported_value, e)
        }
    }

    /// Resolve the [ImportPathAst] and [AstValueImportSpecifier] for a value import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [ImportPathAst]. If you don't have this assumption this could return random unrelated values
    pub fn value_import(&self, idx: &ScopeValueImportIdx) -> (&ImportPathAst<'tree>, &DynAstValueBinding<'tree>) {
        let import_path = &self.imported_script_paths[idx.import_path_idx];
        (import_path, self.values.hoisted(&idx.imported_name).expect("value_import idx not in scope"))
    }

    /// Resolve the [ImportPathAst] and [AstTypeImportSpecifier] for a type import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [ImportPathAst]. If you don't have this assumption this could return random unrelated values
    pub fn type_import(&self, idx: &ScopeTypeImportIdx) -> (&ImportPathAst<'tree>, &DynAstTypeBinding<'tree>) {
        let import_path = &self.imported_script_paths[idx.import_path_idx];
        (import_path, self.types.get(&idx.imported_name).expect("value_import idx not in scope"))
    }

    /// Resolve the [ImportPathAst] and import-specifier [TSNode] for an import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [ImportPathAst]. If you don't have this assumption this could return random unrelated values
    pub fn import(&self, idx: &ScopeImportIdx<impl ScopeImportAlias>) -> (&ImportPathAst<'tree>, TSNode<'tree>) {
        ScopeImportAlias::_index_into_scope(idx, self)
    }
}

impl<'tree> ValueScope<'tree> {
    fn new() -> ValueScope<'tree> {
        ValueScope {
            params: IndexMap::new(),
            catch_param: OnceCell::new(),
            hoisted: HashMap::new(),
            sequential: HashMap::new(),
            exported: HashMap::new(),
            return_: None,
            throw: None,
        }
    }

    pub fn add_imported(&mut self, imported: AstValueImportSpecifier<'tree>, e: &FileLogger<'_>) {
        let alias = &imported.alias.name;
        if let Some(prev_decl) = self.hoisted(alias) {
            error!(e, "Value '{}' already in scope", alias => imported.node;
                issue!("a value with the same name has already been {}", match prev_decl.locality() {
                    Locality::Local => "declared",
                    Locality::Imported => "imported",
                    Locality::Global => "declared globally? (compiler bug)",
                } => prev_decl.node()));
        }
        self.add_hoisted(imported, e);
    }

    pub fn set_params(&mut self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        debug_assert!(self.params.is_empty(), "set_params() can only be called once per scope");
        self.params.extend(params.map(|param| (param.name().clone(), param)));
    }

    pub fn set_catch_param(&mut self, catch_param: Rc<AstCatchParameter<'tree>>) {
        self.catch_param.set(catch_param).expect("set_catch_param() can only be called once per scope");
    }

    pub fn add_hoisted(&mut self, decl: impl AstValueBinding<'tree> + 'tree, e: &FileLogger<'_>) {
        if let Some(prev_decl) = self.hoisted(decl.name()) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, e: &FileLogger<'_>) {
        if let Some(prev_decl) = self.at_pos(decl.name(), decl.node) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node;
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.sequential.entry(decl.name().clone()).or_default().push(Box::new(decl))
    }

    pub fn add_exported(&mut self, original_name: AstValueIdent<'tree>, alias: AstValueIdent<'tree>, e: &FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Value to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        let entry = self.exported.entry(alias.name);
        match entry {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(ExportedId {
                    name: original_name.name,
                    alias_node: alias.node
                });
            }
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                error!(e, "Duplicate value export '{}'", entry.key() => alias.node;
                    issue!("there are multiple exported values with this name in the same scope");
                    hint!("previous export" => entry.get().alias_node));
                entry.insert(ExportedId {
                    name: original_name.name,
                    alias_node: alias.node
                });
            }
        }
    }

    pub fn add_return(&mut self, return_: AstReturn<'tree>, e: &FileLogger<'_>) {
        if let Some(old_return) = self.return_.as_ref() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = self.throw.as_ref() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(self.return_.as_ref(), Some(old_return) if old_return.node.start_byte() > return_.node.start_byte()) {
            self.return_ = Some(return_)
        }
    }

    pub fn add_throw(&mut self, throw: AstThrow<'tree>, e: &FileLogger<'_>) {
        if let Some(old_return) = self.return_.as_ref() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = self.throw.as_ref() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(self.throw.as_ref(), Some(old_throw) if old_throw.node.start_byte() > throw.node.start_byte()) {
            self.throw = Some(throw)
        }
    }

    pub fn iter_params(&self) -> impl Iterator<Item=&AstParameter<'tree>> {
        self.params.values().map(|param| param.as_ref())
    }

    pub fn catch_param(&self) -> Option<&AstCatchParameter<'tree>> {
        self.catch_param.get().map(|param| param.as_ref())
    }

    pub fn hoisted<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynAstValueBinding<'tree>> where ValueName: Borrow<N> {
        self.hoisted.get(name).map(|x| x.as_ref() as &DynAstValueBinding<'tree>)
            .or_else(|| self.catch_param().filter(|x| name.equivalent(x.name())).map(|x| x as &DynAstValueBinding<'tree>))
            .or_else(|| self.params.get(name).map(|x| x.as_ref() as &DynAstValueBinding<'tree>))
    }

    pub fn has_any<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N) -> bool where ValueName: Borrow<N> {
        self.sequential.contains_key(name) ||
            self.hoisted.contains_key(name) ||
            matches!(self.catch_param(), Some(param) if name.equivalent(param.name())) ||
            self.params.contains_key(name)
    }

    pub fn last<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynAstValueBinding<'tree>> where ValueName: Borrow<N> {
        self.sequential.get(name).map(|sequential| {
            sequential.last().expect("sequential should never be Some(<empty vec>)").as_ref() as &DynAstValueBinding<'tree>
        }).or_else(|| self.hoisted(name))
    }

    pub fn has_at_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> bool where ValueName: Borrow<N> {
        self.at_pos(name, pos_node).is_some()
    }

    pub fn at_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> Option<&DynAstValueBinding<'tree>> where ValueName: Borrow<N> {
        return self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte())
                .map(|decl| decl.as_ref() as &DynAstValueBinding<'tree>)
        }).or_else(|| self.hoisted(name))
    }

    pub fn at_exact_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> Option<&AstValueDecl<'tree>> where ValueName: Borrow<N> {
        self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().end_byte() >= pos_node.end_byte() && decl.node().start_byte() <= pos_node.start_byte())
                .map(|decl| decl.as_ref() as &AstValueDecl<'tree>)
        })
    }

    pub fn exported<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, alias: &N) -> Option<&ExportedId<ValueName>> where ValueName: Borrow<N> {
        self.exported.get(alias)
    }

    pub fn return_type(&self, typed_exprs: &ExprTypeMap<'tree>) -> DeterminedReturnType<'tree> {
        self.explicit_return_type(typed_exprs).unwrap_or_else(DeterminedReturnType::implicit_void)
    }

    pub fn explicit_return_type(&self, typed_exprs: &ExprTypeMap<'tree>) -> Option<DeterminedReturnType<'tree>> {
        match (self.return_.as_ref(), self.throw.as_ref()) {
            (None, None) => None,
            (return_, Some(throw))
            if return_.is_none() || throw.node.start_byte() < return_.unwrap().node.start_byte() => Some(DeterminedReturnType {
                type_: RlReturnType::resolved_type(FatType::NEVER),
                return_node: Some(throw.node),
                explicit_type: None
            }),
            (None, Some(_)) => unreachable!("guard succeeds if return_.is_none()"),
            (Some(return_), _) => {
                let expr = return_.returned_value.and_then(|x| typed_exprs.get(x));
                let (type_, explicit_type) = match expr {
                    None => (RlType::ANY, None),
                    Some(expr) => (expr.type_.clone(), expr.explicit_type)
                };
                Some(DeterminedReturnType {
                    // SAFETY: These are the same function, they are bijective
                    type_: unsafe { type_.bimap(ReturnType::Type, ReturnType::Type) },
                    return_node: Some(return_.node),
                    explicit_type
                })
            }
        }
    }
}

impl<'tree> TypeScope<'tree> {
    fn new() -> TypeScope<'tree> {
        TypeScope {
            hoisted: HashMap::new(),
            exported: HashMap::new()
        }
    }

    pub fn add_imported(&mut self, imported: AstTypeImportSpecifier<'tree>, e: &FileLogger<'_>) {
        let alias = &imported.alias.name;
        if let Some(prev_decl) = self.get(alias) {
            error!(e, "Nominal type '{}' already in scope", alias => imported.node;
                issue!("a type with the same name has already been {}", match prev_decl.locality() {
                    Locality::Local => "declared",
                    Locality::Imported => "imported",
                    Locality::Global => "declared globally? (compiler bug)",
                } => prev_decl.node()));
        }
        self.set(imported, e)
    }

    pub fn set(&mut self, decl: impl AstTypeBinding<'tree> + 'tree, e: &FileLogger<'_>) {
        if let Some(prev_decl) = self.get(decl.name()) {
            error!(e, "Duplicate nominal type declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_exported(&mut self, original_name: AstTypeIdent<'tree>, alias: AstTypeIdent<'tree>, e: &FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Type to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        if let Some(prev_exported) = self.exported(&alias.name) {
            error!(e, "Duplicate type export '{}'", &alias.name => alias.node;
                issue!("there are multiple exported types with this name in the same scope");
                hint!("previous export" => prev_exported.alias_node));
        }
        self.exported.insert(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        });
    }

    pub fn has_any<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(&self, name: &N) -> bool where TypeName: Borrow<N> {
        self.hoisted.contains_key(name)
    }

    pub fn get<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynAstTypeBinding<'tree>> where TypeName: Borrow<N> {
        self.hoisted.get(name).map(|decl| decl.as_ref() as &DynAstTypeBinding<'tree>)
    }

    /// Lookup the identifier and substitute generic parameters to get its resolved inherited types
    pub fn inherited(&self, id: IdentType<FatType>, ctx: &ResolveCtx<'_>) -> Option<FatTypeInherited> {
        let decl_ast = self.get(&id.name)?;
        // let decl_node = decl_ast.node(); (TODO: use in type logger)
        let decl = decl_ast.type_decl().resolve(ctx);

        debug_assert!(decl.name == id.name);

        let mut inherited = decl.inherited.as_ref().clone();
        inherited.subst_parameters(&decl.type_params, &id.generic_args, &TypeLogger::ignore());
        Some(inherited)
    }

    pub fn exported<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(&self, alias: &N) -> Option<&ExportedId<TypeName>> where TypeName: Borrow<N> {
        self.exported.get(alias)
    }
}


impl<'tree> Scope<'tree> {
    /// Create an empty scope
    pub fn new(node: UntypedNode<'tree>, parent: Option<&'tree Scope<'tree>>) {
        Scope {
            node,
            parent,
            children: Vec::new(),
            values: ValueScope::new(),
            types: TypeScope::new()
        }
    }
}
