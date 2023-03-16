use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use tree_sitter::LogType::Parse;
use crate::analyses::bindings::{Locality, TypeBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::types::{FatType, FatTypeDecl, DeterminedReturnType, Nullability, ReturnType, ThinType, RlReturnType, RlType, TypeIdent};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstImportStatement, AstNode, AstParameter, AstReturn, AstThrow, AstTypeDecl, AstTypeIdent, AstTypeImportSpecifier, AstValueDecl, AstValueIdent, AstValueImportSpecifier};
use crate::diagnostics::{error, issue, hint, FileLogger};
use crate::import_export::export::{Exports, TranspileOutHeader};

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
pub struct Scope<'tree> {
    did_set_params: bool,
    pub values: ValueScope<'tree>,
    pub types: TypeScope<'tree>,
}

/// A local scope: contains all of the value bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct ValueScope<'tree>(RefCell<_ValueScope<'tree>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExportedId<'tree, Name> {
    pub alias_node: TSNode<'tree>,
    pub name: Name
}

struct _ValueScope<'tree> {
    params: Vec<Rc<AstParameter<'tree>>>,
    hoisted: HashMap<ValueName, Rc<dyn ValueBinding<'tree>>>,
    sequential: HashMap<ValueName, Vec<Rc<AstValueDecl<'tree>>>>,
    exported: HashMap<ValueName, ExportedId<'tree, ValueName>>,
    return_: Option<AstReturn<'tree>>,
    throw: Option<AstThrow<'tree>>,
}

/// A local scope: contains all of the type bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct TypeScope<'tree>(RefCell<_TypeScope<'tree>>);

struct _TypeScope<'tree> {
    hoisted: HashMap<TypeName, Rc<dyn TypeBinding<'tree>>>,
    exported: HashMap<TypeName, ExportedId<'tree, TypeName>>,
}

impl<'tree> Scope<'tree> {
    pub(super) fn new() -> Scope<'tree> {
        Scope {
            did_set_params: false,
            values: ValueScope::new(),
            types: TypeScope::new(),
        }
    }

    pub fn set_params(&self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        assert!(!self.did_set_params, "set_params() can only be called once per scope");
        self.values.set_params(params)
    }

    pub fn add_imported(&self, import_stmt: AstImportStatement<'tree>) {
        for imported_type in import_stmt.imported_types {
            self.types.add_imported(Rc::new(imported_type), &mut e);
        }
        for imported_value in import_stmt.imported_values {
            self.values.add_imported(Rc::new(imported_value), &mut e)
        }
    }
}

impl<'tree> ValueScope<'tree> {
    fn new() -> ValueScope<'tree> {
        ValueScope(RefCell::new(_ValueScope {
            params: Vec::new(),
            hoisted: HashMap::new(),
            sequential: HashMap::new(),
            exported: HashMap::new(),
            return_: None,
            throw: None,
        }))
    }

    pub fn set_params(&self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        let mut this = self.0.borrow_mut();
        debug_assert!(this.params.is_empty());
        this.params = params.collect();
        for param in &this.params {
            this.hoisted.insert(param.name().clone(), param.clone());
        }
    }

    pub fn add_imported(&self, imported: Rc<AstValueImportSpecifier<'tree>>, e: &mut FileLogger<'_>) {
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

    pub fn add_hoisted(&self, decl: Rc<dyn ValueBinding<'tree>>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.hoisted(decl.name()) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        let mut this = self.0.borrow_mut();
        this.hoisted.insert(decl.name().clone(), decl);
    }

    pub fn add_sequential(&self, decl: Rc<AstValueDecl<'tree>>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.at_pos(decl.name(), decl.node) {
            error!(e, "Duplicate value declaration for '{}'", decl.name() => decl.node;
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        let mut this = self.0.borrow_mut();
        this.sequential.entry(decl.name().clone()).or_default().push(decl)
    }

    pub fn add_exported(&self, original_name: AstValueIdent, alias: AstValueIdent, e: &mut FileLogger<'_>) {
        if !self.has_any(&original_name.name) {
            error!(e, "Value to be exported is not in scope '{}'", &original_name.name => original_name.node);
        }
        if let Some(prev_exported) = self.exported(&alias.name) {
            error!(e, "Duplicate value export '{}'", &alias.name => alias.node;
                issue!("there are multiple exported values with this name in the same scope");
                hint!("previous export" => prev_exported.alias_node));
        }
        let mut this = self.0.borrow_mut();
        this.exported.set(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        });
    }

    pub fn add_return(&self, return_: AstReturn<'tree>, e: &mut FileLogger<'_>) {
        let mut this = self.0.borrow_mut();
        if let Some(old_return) = this.return_ {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = this.throw {
            error!(e,
                "cannot return/throw multiple times in the same scope" => return_.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(this.return_, Some(old_return) if old_return.node.start_byte() > return_.node.start_byte()) {
            this.return_ = Some(return_)
        }
    }

    pub fn add_throw(&self, throw: AstThrow<'tree>, e: &mut FileLogger<'_>) {
        let mut this = self.0.borrow_mut();
        if let Some(old_return) = this.return_ {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first return" => old_return.node)
            )
        }
        if let Some(old_throw) = this.throw {
            error!(e,
                "cannot return/throw multiple times in the same scope" => throw.node;
                hint!("first throw" => old_throw.node)
            )
        }
        if !matches!(this.throw, Some(old_throw) if old_throw.node.start_byte() > throw.node.start_byte()) {
            this.throw = Some(throw)
        }
    }

    pub fn params(&self) -> Ref<'_, [Rc<AstParameter<'tree>>]> {
        Ref::map(self.0.borrow(), |this| this.params.as_ref())
    }

    pub fn hoisted(&self, name: &ValueName) -> Option<Rc<dyn ValueBinding<'tree>>> {
        self.0.borrow().hoisted.get(name).cloned()
    }

    pub fn has_any(&self, name: &ValueName) -> bool {
        let this = self.0.borrow();
        this.sequential.contains_key(name) || this.hoisted.contains_key(name)
    }

    pub fn last(&self, name: &ValueName) -> Option<Rc<dyn ValueBinding<'tree>>> {
        let this = self.0.borrow();
        if let Some(sequential) = this.sequential.get(name) {
            return Some(sequential.last()
                .expect("sequential should never be Some(<empty vec>)")
                .clone())
        }
        this.hoisted.get(name).cloned()
    }

    pub fn has_at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> bool {
        self.get(name, pos_node).is_some()
    }

    pub fn at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<Rc<dyn ValueBinding<'tree>>> {
        let this = self.0.borrow();
        return this.sequential.get(name).and_then(|sequential| {
            sequential.iter()
                .rfind(|decl| decl.node().start_byte() <= pos_node.start_byte())
        }).or_else(|| this.hoisted.get(name)).cloned()
    }

    pub fn at_exact_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<Rc<dyn ValueBinding<'tree>>> {
        let this = self.0.borrow();
        this.sequential.get(name).and_then(|sequential| {
            sequential.iter()
                .rfind(|decl| decl.node().start_byte() <= pos_node.start_byte()
                    && decl.node().end_byte() >= pos_node.end_byte())
        }).cloned()
    }

    pub fn exported(&self, alias: &ValueName) -> Option<ExportedId<ValueName>> {
        let this = self.0.borrow();
        this.exported.get(alias).cloned()
    }

    pub fn return_type(&self, typed_exprs: &ExprTypeMap<'tree>) -> DeterminedReturnType<'tree> {
        let this = self.0.borrow();
        match (this.return_.as_ref(), this.throw.as_ref()) {
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
                    type_: unsafe { type_.bimap(ReturnType::Type, ReturnType::Type) },
                    return_node: Some(return_.node),
                    explicit_type
                }
            }
        }
    }
}

impl<'tree> TypeScope<'tree> {
    fn new() -> TypeScope<'tree> {
        TypeScope(RefCell::new(_TypeScope {
            hoisted: HashMap::new(),
            exported: HashMap::new()
        }))
    }

    pub fn add_imported(&self, imported: Rc<AstTypeImportSpecifier<'tree>>, e: &mut FileLogger<'_>) {
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

    pub fn set(&self, decl: Rc<dyn TypeBinding<'tree>>, e: &mut FileLogger<'_>) {
        if let Some(prev_decl) = self.get(decl.name()) {
            error!(e, "Duplicate nominal type declaration for '{}'", decl.name() => decl.node();
                issue!("they are in the same scope");
                hint!("previous declaration" => prev_decl.node()));
        }
        let mut this = self.0.borrow_mut();
        this.hoisted.insert(decl.name().clone(), decl);
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
        let mut this = self.0.borrow_mut();
        this.exported.insert(alias.name, ExportedId {
            name: original_name.name,
            alias_node: alias.node
        });
    }

    pub fn has_any(&self, name: &TypeName) -> bool {
        let this = self.0.borrow();
        this.hoisted.contains_key(name)
    }

    pub fn get(&self, name: &TypeName) -> Option<Rc<dyn TypeBinding<'tree>>> {
        let this = self.0.borrow();
        this.hoisted.get(name).cloned()
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
        let this = self.0.borrow();
        this.exported.get(alias).cloned()
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