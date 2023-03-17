use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::marker::PhantomPinned;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::rc::{Rc, Weak};
use indexmap::IndexMap;
use once_cell::unsync::OnceCell;
use tree_sitter::LogType::Parse;
use crate::analyses::bindings::{Locality, TypeBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::scopes::scope_ptr::{WeakScopePtr, ScopePtr};
use crate::analyses::types::{DeterminedReturnType, FatType, FatTypeDecl, FileResolveCache, Nullability, ResolveCache, ReturnType, RlReturnType, RlType, ThinType, TypeIdent};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstImportPath, AstImportStatement, AstNode, AstParameter, AstReturn, AstThrow, AstTypeDecl, AstTypeIdent, AstTypeImportSpecifier, AstValueDecl, AstValueIdent, AstValueImportSpecifier};
use crate::diagnostics::{error, FileLogger, hint, issue};
use crate::import_export::export::{Exports, ImportPath, Module};

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
pub struct ValueScope<'tree> {
    params: IndexMap<ValueName, Box<AstParameter<'tree>>>,
    hoisted: HashMap<ValueName, Box<dyn ValueBinding<'tree>>>,
    sequential: HashMap<ValueName, Vec<Box<AstValueDecl<'tree>>>>,
    exported: HashMap<ValueName, ExportedId<'tree, ValueName>>,
    return_: Option<AstReturn<'tree>>,
    throw: Option<AstThrow<'tree>>,
}

/// A local scope: contains all of the type bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct TypeScope<'tree> {
    hoisted: HashMap<TypeName, Box<dyn TypeBinding<'tree>>>,
    exported: HashMap<TypeName, ExportedId<'tree, TypeName>>,
}

/// Index in a scope to an imported value or type identifier
pub struct ScopeImportIdx<Alias> {
    /// Index to the [AstImportPath]
    pub import_path_idx: usize,
    /// Imported name
    pub imported_name: Alias
}

/// Index in a scope to an imported value identifier
pub type ScopeValueImportIdx = ScopeImportIdx<ValueName>;
/// Index in a scope to an imported type identifier
pub type ScopeTypeImportIdx = ScopeImportIdx<TypeName>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExportedId<'tree, Name> {
    pub alias_node: TSNode<'tree>,
    pub name: Name
}

impl<'tree> Scope<'tree> {
    pub(super) fn new(parent: Option<&ScopePtr<'tree>>) -> Self {
        Scope {
            did_set_params: false,
            imported_script_paths: Vec::new(),
            values: ValueScope::new(),
            types: TypeScope::new(),
            resolve_cache: ResolveCache::new(),
            parent: parent.map_or_else(WeakScopePtr::new, ScopePtr::downgrade),
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

    pub fn add_imported(&mut self, import_stmt: AstImportStatement<'tree>) {
        self.imported_script_paths.push(import_stmt.import_path);
        for imported_type in import_stmt.imported_types {
            self.types.add_imported(imported_type, &mut e);
        }
        for imported_value in import_stmt.imported_values {
            self.values.add_imported(imported_value, &mut e)
        }
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

    pub fn add_imported(&mut self, imported: AstValueImportSpecifier<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn set_params(&mut self, params: impl Iterator<Item=AstParameter<'tree>>) {
        debug_assert!(self.params.is_empty());
        self.params.extend(params.map(|param| (param.name().clone(), Box::new(param))));
    }

    pub fn add_hoisted(&mut self, decl: impl ValueBinding<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.hoisted(decl.name()) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.at_pos(decl.name(), decl.node) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node;
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.sequential.entry(decl.name().clone()).or_default().push(Box::new(decl))
    }

    pub fn add_exported(&mut self, original_name: AstValueIdent, alias: AstValueIdent, e: &mut FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Value to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        if let Some(prev_exported) = self.exported.insert(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        }) {
            error!(e, "Duplicate value export '{}'", &alias.name => alias.node;
                issue!("there are multiple exported values with this name in the same scope");
                hint!("previous export" => prev_exported.alias_node));
        }
    }

    pub fn add_return(&mut self, return_: AstReturn<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn add_throw(&mut self, throw: AstThrow<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn hoisted(&self, name: &ValueName) -> Option<&dyn ValueBinding<'tree>> {
        self.hoisted.get(name).or_else(|| self.params.get(name))
    }

    pub fn has_any(&self, name: &ValueName) -> bool {
        self.sequential.contains_key(name) || self.hoisted.contains_key(name) || self.params.contains_key(name)
    }

    pub fn last(&self, name: &ValueName) -> Option<&dyn ValueBinding<'tree>> {
        // `as_deref`s convert &Box<dyn ValueBinding> to &dyn ValueBinding
        self.sequential.get(name).map(|sequential| {
            sequential.last().expect("sequential should never be Some(<empty vec>)")
        }).or_else(|| self.hoisted(name)).as_deref()
    }

    pub fn has_at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> bool {
        self.get(name, pos_node).is_some()
    }

    pub fn at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&dyn ValueBinding<'tree>> {
        return self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte())
        }).or_else(|| self.hoisted(name)).as_deref()
    }

    pub fn at_exact_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&dyn ValueBinding<'tree>> {
        self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte() &&
                decl.node().end_byte() >= pos_node.end_byte())
        }).as_deref()
    }

    pub fn exported(&self, alias: &ValueName) -> Option<&ExportedId<ValueName>> {
        self.exported.get(alias)
    }

    pub fn return_type(&self, typed_exprs: &ExprTypeMap<'tree>) -> DeterminedReturnType<'tree> {
        match (self.return_.get(), self.throw.get()) {
            (None, None) => DeterminedReturnType {
                type_: RlReturnType::resolved_void(),
                return_node: None,
                explicit_type: None,
            },
            (return_, Some(throw))
            if return_.is_none() || throw.node.start_byte() < return_.unwrap().node.start_byte() => DeterminedReturnType {
                type_: RlReturnType::resolved_type(FatType::never()),
                return_node: Some(throw.node),
                explicit_type
            },
            (None, Some(_)) => unreachable!("guard succeeds if return_.is_none()"),
            (Some(return_), _) => {
                let expr = return_.returned_value.and_then(|x| typed_exprs.get(x));
                let (type_, explicit_type) = match expr {
                    None => (RlType::any(), None),
                    Some(expr) => (expr.type_.clone(), expr.explicit_type)
                };
                DeterminedReturnType {
                    // SAFETY: These are the same function, they are bijective
                    type_: unsafe { type_.trimap(ReturnType::Type, ReturnType::Type, |x| x) },
                    return_node: Some(return_.node),
                    explicit_type
                }
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

    pub fn add_imported(&mut self, imported: AstTypeImportSpecifier<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn set(&mut self, decl: impl TypeBinding<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.get(decl.name()) {
            error!(e, "Duplicate nominal type declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_exported(&mut self, original_name: AstTypeIdent, alias: AstTypeIdent, e: &mut FileLogger<'_>) {
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

    pub fn has_any(&self, name: &TypeName) -> bool {
        self.hoisted.contains_key(name)
    }

    pub fn get(&self, name: &TypeName) -> Option<&dyn TypeBinding<'tree>> {
        self.hoisted.get(name)
    }

    pub fn immediate_supertypes(&self, type_: ThinType) -> FatType {
        let ThinType::Nominal { id, nullability } = type_ else {
            Vec::new()
        };
        self.ident_supertypes(id, nullability)
    }

    fn ident_supertypes(&self, id: TypeIdent<ThinType>, nullability: Nullability) -> FatType {
        // TODO this is wrong
        let mut supertypes = Vec::new();
        let mut queue = VecDeque::new();
        queue.push_back(name);
        while let Some(name) = queue.pop_front() {
            if let Some(decl) = this.hoisted.get(name) {
                supertypes.push(decl.clone());
                queue.extend(decl.supertypes().iter().map(|x| x.name()));
            }
        }
        supertypes
    }

    pub fn exported(&self, alias: &TypeName) -> Option<&ExportedId<TypeName>> {
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