use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::marker::PhantomPinned;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::rc::{Rc, Weak};
use elsa::{FrozenIndexMap, FrozenMap, FrozenVec};
use indexmap::IndexMap;
use once_cell::unsync::OnceCell;
use tree_sitter::LogType::Parse;
use crate::analyses::bindings::{Locality, TypeBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::types::{FatType, FatTypeDecl, DeterminedReturnType, Nullability, ReturnType, ThinType, RlReturnType, RlType, TypeIdent, FileResolveCache, ResolveCache};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstImportStatement, AstNode, AstParameter, AstReturn, AstThrow, AstTypeDecl, AstTypeIdent, AstTypeImportSpecifier, AstValueDecl, AstValueIdent, AstValueImportSpecifier};
use crate::diagnostics::{error, issue, hint, FileLogger};
use crate::import_export::export::{Exports, Module};

/// Raw pointer to scope which can be derefenced if we know the scope exists
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RawScopePtr(*const Scope<'static>);

/// Pointer to a scope
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ScopePtr<'tree>(Pin<Rc<Scope<'tree>>>);

/// Raw pointer to pinned script path which can be dereferenced if we know the scope containing the imported path exists
pub(crate) struct RawScriptPathPtr(*const (PhantomPinned, Path));

/// Script path which is pinned for [ResolvedLazy]
pub(crate) struct ScriptPathPtr(Pin<Box<(PhantomPinned, Path)>>);

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
#[derive(Debug)]
pub struct Scope<'tree> {
    did_set_params: bool,
    /// These must be stored in the scope because pointers in [ResolvedLazy] values reference them
    imported_script_paths: RefCell<Vec<ScriptPathPtr>>,
    pub values: ValueScope<'tree>,
    pub types: TypeScope<'tree>,
    pub(crate) resolve_cache: ResolveCache,
    pub parent: Weak<Scope<'tree>>,
    _pinned: PhantomPinned
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExportedId<'tree, Name> {
    pub alias_node: TSNode<'tree>,
    pub name: Name
}

/// A local scope: contains all of the value bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct ValueScope<'tree> {
    params: FrozenIndexMap<ValueName, Box<AstParameter<'tree>>>,
    hoisted: FrozenMap<ValueName, Box<dyn ValueBinding<'tree>>>,
    sequential: FrozenMap<ValueName, FrozenVec<Box<AstValueDecl<'tree>>>>,
    exported: RefCell<HashMap<ValueName, ExportedId<'tree, ValueName>>>,
    return_: Cell<Option<AstReturn<'tree>>>,
    throw: Cell<Option<AstThrow<'tree>>>,
}

/// A local scope: contains all of the type bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct TypeScope<'tree> {
    hoisted: FrozenMap<TypeName, Box<dyn TypeBinding<'tree>>>,
    exported: RefCell<HashMap<TypeName, ExportedId<'tree, TypeName>>>,
}

impl RawScopePtr {
    pub const fn null() -> RawScopePtr {
        RawScopePtr(std::ptr::null())
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    /// SAFETY: You must statically know that the scope being pointed to is alive.
    /// This creates a new reference to that scope, incrementing the reference count.
    ///
    /// Note that [as_raw] doesn't affect the reference count, so if you call [as_raw] and then
    /// drop the pointer (decrements the count) and there are no active references, you can't call
    /// `from_raw` without UB.
    ///
    /// Returns `None` (not UB) if this is null.
    pub(crate) unsafe fn upgrade<'tree>(self) -> Option<ScopePtr<'tree>> {
        if self.is_null() {
            return None
        }
        unsafe {
            // SAFETY: Rc::into_raw is equivalent to Rc::as_ptr and then mem::forget.
            // We don't call forget (letting the reference from as_raw be dropped)
            // but increment the reference count beforehand (since Rc::from_raw won't).
            // So this is equivalent to Rc::from_raw and Rc::into_raw.
            // And the pin is ok because Rc::pin trivially calls Pin::new_unchecked
            // (Rc is intrinsically Pinned since it's a pointer)
            let ptr = self.0 as *const Scope<'tree>;
            Rc::increment_strong_count(ptr);
            Some(ScopePtr(Pin::new_unchecked(Rc::from_raw(ptr))))
        }
    }
}

impl<'tree> ScopePtr<'tree> {
    pub(super) fn new(parent: Option<&ScopePtr<'tree>>) -> Self {
        ScopePtr(Rc::pin(Scope::new(parent)))
    }

    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn as_raw(this: &Self) -> RawScopePtr {
        RawScopePtr(Self::as_ptr(this) as *const Scope<'static>)
    }

    pub fn downgrade(this: &Self) -> Weak<Scope<'tree>> {
        // SAFETY: we are not mutating the Rc but creating an (unpinned) clone
        Rc::downgrade(unsafe { ScopePtr::unpinned(this) })
    }

    #[allow(clippy::wrong_self_convention)]
    fn as_ptr(this: &Self) -> *const Scope<'tree> {
        // SAFETY: we are not mutating the Rc but creating an (unpinned) pointer
        unsafe { Rc::as_ptr(Self::unpinned(this)) }
    }

    /// SAFETY: You must ensure that the inner reference remains pinned
    unsafe fn unpinned(this: &Self) -> &Rc<Scope<'tree>> {
        // SAFETY: Pin<Rc<_>> = Rc<_> but pinned
        std::mem::transmute::<&Pin<Rc<Scope<'tree>>>, &Rc<Scope<'tree>>>(&this.0)
    }
}

impl<'tree> Deref for ScopePtr<'tree> {
    type Target = Scope<'tree>;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl RawScriptPathPtr {
    pub const fn null() -> RawScriptPathPtr {
        RawScriptPathPtr(std::ptr::null())
    }

    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    /// SAFETY: You must statically know that the path being pointed to is alive.
    ///
    /// Returns `None` (not UB) if this is null.
    pub(crate) unsafe fn upgrade<'a>(self) -> Option<&'a Path> {
        unsafe {
            // SAFETY: (PhantomPinned, Path) == Path and we already "know" it's alive from the above
            let ptr = self.0 as *const Path;
            ptr.as_ref()
        }
    }
}

impl ScriptPathPtr {
    pub(crate) fn new(path: PathBuf) -> Self {
        // SAFETY: Path == (PhantomPinned, Path)
        ScriptPathPtr(Box::into_pin(unsafe { std::mem::transmute::<Box<Path>, Box<(PhantomPinned, Path)>>(path.into_boxed_path()) }))
    }

    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn as_raw(this: &Self) -> RawScriptPathPtr {
        // SAFETY: We are not mutating the pinned data but only converting to a reference
        // (and also the pin doesn't really matter here)
        RawScriptPathPtr(unsafe { Pin::into_inner(this.0.as_ref()) } as *const (PhantomPinned, Path))
    }
}

impl Deref for ScriptPathPtr {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl AsRef<Path> for ScriptPathPtr {
    fn as_ref(&self) -> &Path {
        // SAFETY: (PhantomPinned, Path) == Path
        let inner = unsafe { std::mem::transmute::<Pin<&(PhantomPinned, Path)>, Pin<&Path>>(this.0.as_ref()) };
        Pin::into_inner(inner)
    }
}

impl<'tree> Scope<'tree> {
    fn new(parent: Option<&ScopePtr<'tree>>) -> Self {
        Scope {
            did_set_params: false,
            imported_script_paths: RefCell::new(Vec::new()),
            values: ValueScope::new(),
            types: TypeScope::new(),
            resolve_cache: ResolveCache::new(),
            parent: parent.map_or_else(Weak::new, ScopePtr::downgrade),
            _pinned: PhantomPinned
        }
    }

    pub fn set_params(&self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        assert!(!self.did_set_params, "set_params() can only be called once per scope");
        self.values.set_params(params)
    }

    pub fn add_imported(&self, import_stmt: AstImportStatement<'tree>) {
        self.imported_script_paths.borrow_mut().push(import_stmt.script_path);
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
            params: FrozenIndexMap::new(),
            hoisted: FrozenMap::new(),
            sequential: FrozenMap::new(),
            exported: RefCell::new(HashMap::new()),
            return_: Cell::new(None),
            throw: Cell::new(None),
        }
    }

    pub fn add_imported(&self, imported: AstValueImportSpecifier<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn set_params(&self, params: impl Iterator<Item=AstParameter<'tree>>) {
        debug_assert!(self.params.is_empty());
        for param in params {
            self.params.insert(param.name().clone(), Box::new(param));
        }
    }

    pub fn add_hoisted(&self, decl: impl ValueBinding<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.hoisted(decl.name()) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_sequential(&self, decl: AstValueDecl<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.at_pos(decl.name(), decl.node) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node;
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.sequential.entry(decl.name().clone()).or_default().push(Box::new(decl))
    }

    pub fn add_exported(&self, original_name: AstValueIdent, alias: AstValueIdent, e: &mut FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Value to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        if let Some(prev_exported) = self.exported.borrow_mut().insert(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        }) {
            error!(e, "Duplicate value export '{}'", &alias.name => alias.node;
                issue!("there are multiple exported values with this name in the same scope");
                hint!("previous export" => prev_exported.alias_node));
        }
    }

    pub fn add_return(&self, return_: AstReturn<'tree>, e: &mut FileLogger<'_>) {
        if let Some(old_return) = self.return_.get() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = self.throw.get() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(self.return_.get(), Some(old_return) if old_return.node.start_byte() > return_.node.start_byte()) {
            self.return_.set(Some(return_))
        }
    }

    pub fn add_throw(&self, throw: AstThrow<'tree>, e: &mut FileLogger<'_>) {
        if let Some(old_return) = self.return_.get() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = self.throw.get() {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(self.throw.get(), Some(old_throw) if old_throw.node.start_byte() > throw.node.start_byte()) {
            self.throw.set(Some(throw))
        }
    }

    pub fn iter_params(&self) -> impl Iterator<Item=&AstParameter<'tree>> {
        struct IterScopeParams<'a, 'tree> {
            index: usize,
            map: &'a FrozenIndexMap<ValueName, Box<AstParameter<'tree>>>
        }
        impl<'a, 'tree> IterScopeParams<'a, 'tree> {
            fn new(map: &'a FrozenIndexMap<ValueName, Box<AstParameter<'tree>>>) -> IterScopeParams<'a, 'tree> {
                IterScopeParams {
                    index: 0,
                    map
                }
            }
        }
        impl<'a, 'tree> Iterator for IterScopeParams<'a, 'tree> {
            type Item = &'a AstParameter<'tree>;
            fn next(&mut self) -> Option<Self::Item> {
                let index = self.index;
                self.index += 1;
                self.map.get_index(index).map(|(_, v)| v)
            }
        }

        IterScopeParams::new(&self.params)
    }

    pub fn hoisted(&self, name: &ValueName) -> Option<&dyn ValueBinding<'tree>> {
        self.hoisted.get(name).or_else(|| self.params.get(name))
    }

    pub fn has_any(&self, name: &ValueName) -> bool {
        self.sequential.get(name).is_some() || self.hoisted.get(name).is_some() || self.params.get(name).is_some()
    }

    pub fn last(&self, name: &ValueName) -> Option<&dyn ValueBinding<'tree>> {
        self.sequential.get(name).and_then(|sequential| {
            sequential.last().expect("sequential should never be Some(<empty vec>)")
        }).or_else(|| self.hoisted(name))
    }

    pub fn has_at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> bool {
        self.get(name, pos_node).is_some()
    }

    pub fn at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&dyn ValueBinding<'tree>> {
        return self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte())
        }).or_else(|| self.hoisted(name))
    }

    pub fn at_exact_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&dyn ValueBinding<'tree>> {
        self.sequential.get(name).and_then(|sequential| {
            sequential.iter().rfind(|decl| decl.node().start_byte() <= pos_node.start_byte() &&
                decl.node().end_byte() >= pos_node.end_byte())
        })
    }

    pub fn exported(&self, alias: &ValueName) -> Option<ExportedId<ValueName>> {
        self.exported.borrow().get(alias).cloned()
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
            hoisted: FrozenMap::new(),
            exported: RefCell::new(HashMap::new())
        }
    }

    pub fn add_imported(&self, imported: AstTypeImportSpecifier<'tree>, e: &mut FileLogger<'_>) {
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

    pub fn set(&self, decl: impl TypeBinding<'tree>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.get(decl.name()) {
            error!(e, "Duplicate nominal type declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        self.hoisted.insert(decl.name().clone(), Box::new(decl));
    }

    pub fn add_exported(&self, original_name: AstTypeIdent, alias: AstTypeIdent, e: &mut FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Type to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        if let Some(prev_exported) = self.exported(&alias.name) {
            error!(e, "Duplicate type export '{}'", &alias.name => alias.node;
                issue!("there are multiple exported types with this name in the same scope");
                hint!("previous export" => prev_exported.alias_node));
        }
        self.exported.borrow_mut().insert(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        });
    }

    pub fn has_any(&self, name: &TypeName) -> bool {
        self.hoisted.get(name).is_some()
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
        let this = self.0.borrow();
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

    pub fn exported(&self, alias: &TypeName) -> Option<ExportedId<TypeName>> {
        self.exported.borrow().get(alias).cloned()
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