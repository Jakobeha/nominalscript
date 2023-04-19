use std::collections::HashMap;
use std::marker::PhantomPinned;
use std::rc::Rc;

use indexmap::{Equivalent, IndexMap};

use crate::{error, hint, issue};
use crate::analyses::bindings::{Locality, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::{ExprTypeMap, InactiveScopePtr, WeakScopePtr};
use crate::analyses::scopes::scope_imports::{ScopeImportAlias, ScopeImportIdx, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{DeterminedReturnType, FatType, FatTypeInherited, ResolveCache, ResolveCtx, ReturnType, RlReturnType, RlType, TypeIdent};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstImportPath, AstImportStatement, AstNode, AstParameter, AstReturn, AstThrow, AstTypeBinding, AstTypeIdent, AstTypeImportSpecifier, AstValueBinding, AstValueDecl, AstValueIdent, AstValueImportSpecifier, DynAstTypeBinding, DynAstValueBinding};
use crate::diagnostics::{FileLogger, TypeLogger};

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
#[derive(Debug)]
pub struct Scope<'tree> {
    did_set_params: bool,
    imported_script_paths: Vec<AstImportPath<'tree>>,
    pub values: ValueScope<'tree>,
    pub types: TypeScope<'tree>,
    pub(crate) resolve_cache: ResolveCache,
    pub parent: WeakScopePtr<'tree>,
    pub(super) is_being_mutated: bool,
    _pinned: PhantomPinned
}

/// A local scope: contains all of the value bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
#[derive(Debug)]
pub struct ValueScope<'tree> {
    params: IndexMap<ValueName, Rc<AstParameter<'tree>>>,
    hoisted: HashMap<ValueName, Box<DynAstValueBinding<'tree>>>,
    sequential: HashMap<ValueName, Vec<Box<AstValueDecl<'tree>>>>,
    exported: HashMap<ValueName, ExportedId<'tree, ValueName>>,
    return_: Option<AstReturn<'tree>>,
    throw: Option<AstThrow<'tree>>,
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
    pub(super) fn new(parent: Option<&InactiveScopePtr<'tree>>) -> Self {
        Scope {
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

    /// Resolve the [AstImportPath] and [AstValueImportSpecifier] for a value import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [AstImportPath]. If you don't have this assumption this could return random unrelated values
    pub fn value_import(&self, idx: &ScopeValueImportIdx) -> (&AstImportPath<'tree>, &DynAstValueBinding<'tree>) {
        let import_path = &self.imported_script_paths[idx.import_path_idx];
        (import_path, self.values.hoisted(&idx.imported_name).expect("value_import idx not in scope"))
    }

    /// Resolve the [AstImportPath] and [AstTypeImportSpecifier] for a type import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [AstImportPath]. If you don't have this assumption this could return random unrelated values
    pub fn type_import(&self, idx: &ScopeTypeImportIdx) -> (&AstImportPath<'tree>, &DynAstTypeBinding<'tree>) {
        let import_path = &self.imported_script_paths[idx.import_path_idx];
        (import_path, self.types.get(&idx.imported_name).expect("value_import idx not in scope"))
    }

    /// Resolve the [AstImportPath] and import-specifier [TSNode] for an import
    ///
    /// This expects idx to be from this scope as it also indexes `imported_script_paths` to get the
    /// [AstImportPath]. If you don't have this assumption this could return random unrelated values
    pub fn import(&self, idx: &ScopeImportIdx<impl ScopeImportAlias>) -> (&AstImportPath<'tree>, TSNode<'tree>) {
        ScopeImportAlias::_index_into_scope(idx, self)
    }
}

impl<'tree> ValueScope<'tree> {
    fn new() -> ValueScope<'tree> {
        ValueScope {
            params: IndexMap::new(),
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
        debug_assert!(self.params.is_empty());
        self.params.extend(params.map(|param| (param.name().clone(), param)));
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

    pub fn hoisted(&self, name: &(impl Equivalent<ValueName> + ?Sized)) -> Option<&DynAstValueBinding<'tree>> {
        self.hoisted.get(name).map(|x| x.as_ref() as &DynAstValueBinding<'tree>)
            .or_else(|| self.params.get(name).map(|x| x.as_ref() as &DynAstValueBinding<'tree>))
    }

    pub fn has_any(&self, name: &(impl Equivalent<ValueName> + ?Sized)) -> bool {
        self.sequential.contains_key(name) || self.hoisted.contains_key(name) || self.params.contains_key(name)
    }

    pub fn last(&self, name: &(impl Equivalent<ValueName> + ?Sized)) -> Option<&DynAstValueBinding<'tree>> {
        self.sequential.get(name).map(|sequential| {
            sequential.last().expect("sequential should never be Some(<empty vec>)").as_ref() as &DynAstValueBinding<'tree>
        }).or_else(|| self.hoisted(name))
    }

    pub fn has_at_pos(&self, name: &(impl Equivalent<ValueName> + ?Sized), pos_node: TSNode<'tree>) -> bool {
        self.at_pos(name, pos_node).is_some()
    }

    pub fn at_pos(&self, name: &(impl Equivalent<ValueName> + ?Sized), pos_node: TSNode<'tree>) -> Option<&DynAstValueBinding<'tree>> {
        return self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte())
                .map(|decl| decl.as_ref() as &DynAstValueBinding<'tree>)
        }).or_else(|| self.hoisted(name))
    }

    pub fn at_exact_pos(&self, name: &(impl Equivalent<ValueName> + ?Sized), pos_node: TSNode<'tree>) -> Option<&AstValueDecl<'tree>> {
        self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().end_byte() >= pos_node.end_byte() && decl.node().start_byte() <= pos_node.start_byte())
                .map(|decl| decl.as_ref() as &AstValueDecl<'tree>)
        })
    }

    pub fn exported(&self, alias: &(impl Equivalent<ValueName> + ?Sized)) -> Option<&ExportedId<ValueName>> {
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

    pub fn has_any(&self, name: &(impl Equivalent<TypeName> + ?Sized)) -> bool {
        self.hoisted.contains_key(name)
    }

    pub fn get(&self, name: &(impl Equivalent<TypeName> + ?Sized)) -> Option<&DynAstTypeBinding<'tree>> {
        self.hoisted.get(name).map(|decl| decl.as_ref() as &DynAstTypeBinding<'tree>)
    }

    /// Lookup the identifier and substitute generic parameters to get its resolved inherited types
    fn inherited(&self, id: TypeIdent<FatType>, ctx: &ResolveCtx<'_>) -> Option<FatTypeInherited> {
        let decl_ast = self.get(&id.name)?;
        // let decl_node = decl_ast.node(); (TODO: use in type logger)
        let decl = decl_ast.type_decl().resolve(ctx);

        debug_assert!(decl.name == id.name);

        let mut inherited = decl.inherited.as_ref().clone();
        inherited.subst_parameters(&decl.type_params, &id.generic_args, &TypeLogger::ignore());
        Some(inherited)
    }

    pub fn exported(&self, alias: &(impl Equivalent<TypeName> + ?Sized)) -> Option<&ExportedId<TypeName>> {
        self.exported.get(alias)
    }
}

/*
export class NominalTypeDeclMap {

  async * getSupertypes (type: NominalTypeShape, use: TSNode): AsyncGenerator<NominalTypeShape> {
    if (!NominalTypeShape.isIdent(type)) {
      return
    }
    const decl = await this.getShape(type.ident, use)
    if (decl.shape === null) {
      return
    }
    const typeArgs = NominalTypeShape.argsForParams(type.genericArgs, decl.shape.genericParams.length, decl.loc, use)
    for (const nominalSupertypeNotSubst of decl.shape.nominalSupertypes) {
      const nominalSupertypeSubst = NominalTypeShape.subst(
        nominalSupertypeNotSubst,
        decl.shape.genericParams,
        typeArgs
      )
      yield nominalSupertypeSubst
      yield * this.getSupertypes(nominalSupertypeSubst, use)
    }
  }

  /** Returns the supertype which is structural */
  async superStructure (type: NominalTypeShape, use: TSNode): Promise<NominalTypeShape | null> {
    if (NominalTypeShape.isStructural(type)) {
      return type
    }
    for await (const supertype of this.getSupertypes(type, use)) {
      if (NominalTypeShape.isStructural(supertype)) {
        return supertype
      }
    }
    return null
  }

  /** Returns the supertype which is structural */
  async superStructure2 (type: NominalTypeInferred): Promise<NominalTypeInferred | null> {
    return mapNullable(
      await this.superStructure(type.shape, type.ast.node),
      shape => ({ shape, ast: type.ast })
    )
  }

  /** Returns the supertype which is a Promise */
  async superPromise (type: NominalTypeShape, use: TSNode): Promise<NominalTypeShape | null> {
    if (NominalTypeShape.isPromise(type)) {
      return type
    }
    for await (const supertype of this.getSupertypes(type, use)) {
      if (NominalTypeShape.isPromise(supertype)) {
        return supertype
      }
    }
    return null
  }

  /** Returns the supertype which is a Promise */
  async superPromise2 (type: NominalTypeInferred): Promise<NominalTypeInferred | null> {
    return mapNullable(
      await this.superPromise(type.shape, type.ast.node),
      shape => ({ shape, ast: type.ast })
    )
  }
}*/