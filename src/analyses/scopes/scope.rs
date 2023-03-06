use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use crate::analyses::bindings::{LocalTypeBinding, LocalValueBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::types::{FatType, FatTypeDecl, InferredReturnType, Nullability, ReturnType, ThinType};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::{AstNode, AstParameter, AstReturn, AstThrow, AstTypeDecl, AstValueDecl};

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
pub struct Scope<'tree> {
    did_set_params: bool,
    values: ValueScope<'tree>,
    types: TypeScope<'tree>,
}

/// A local scope: contains all of the value bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct ValueScope<'tree>(RefCell<_ValueScope<'tree>>);

struct _ValueScope<'tree> {
    params: Vec<Rc<AstParameter<'tree>>>,
    hoisted: HashMap<ValueName, Rc<dyn LocalValueBinding<'tree>>>,
    sequential: HashMap<ValueName, Vec<Rc<AstValueDecl<'tree>>>>,
    return_: Option<AstReturn<'tree>>,
    throw: Option<AstThrow<'tree>>,
}

/// A local scope: contains all of the type bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct TypeScope<'tree>(RefCell<_TypeScope<'tree>>);

struct _TypeScope<'tree> {
    hoisted: HashMap<TypeName, Rc<AstTypeDecl<'tree>>>,
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
}

impl<'tree> ValueScope<'tree> {
    fn new() -> ValueScope<'tree> {
        ValueScope(RefCell::new(_ValueScope {
            params: Vec::new(),
            hoisted: HashMap::new(),
            sequential: HashMap::new(),
            return_: None,
            throw: None,
        }))
    }

    pub fn params(&self) -> Ref<'_, [Rc<AstParameter<'tree>>]> {
        Ref::map(self.0.borrow(), |this| this.params.as_ref())
    }

    pub fn set_params(&self, params: impl Iterator<Item=Rc<AstParameter<'tree>>>) {
        let mut this = self.0.borrow_mut();
        debug_assert!(this.params.is_empty());
        this.params = params.collect();
        for param in &this.params {
            this.hoisted.insert(param.name().clone(), param.clone());
        }
    }

    pub fn add_hoisted(&self, decl: Rc<dyn LocalValueBinding<'tree>>) {
        let mut this = self.0.borrow_mut();
        this.hoisted.add_hoisted(decl)
    }

    pub fn hoisted(&self, name: &ValueName) -> Option<Rc<dyn LocalValueBinding<'tree>>> {
        self.0.borrow().hoisted.get(name).cloned()
    }

    pub fn add_sequential(&self, decl: Rc<AstValueDecl<'tree>>) {
        let mut this = self.0.borrow_mut();
        this.sequential.entry(decl.name().clone()).or_default().push(decl)
    }

    pub fn has_any(&self, name: &ValueName) -> bool {
        let this = self.0.borrow();
        this.sequential.contains_key(name) || this.hoisted.contains_key(name)
    }

    pub fn get_last(&self, name: &ValueName) -> Option<Rc<dyn ValueBinding<'tree>>> {
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

    pub fn return_type(&self, typed_exprs: &ExprTypeMap<'tree>) -> InferredReturnType<'tree> {
        let this = self.0.borrow();
        match (this.return_.as_ref(), this.throw.as_ref()) {
            (None, None) => InferredReturnType {
                type_: ReturnType::Void,
                return_node: None,
                explicit_type: None,
            },
            (return_, Some(throw))
            if return_.is_none() || throw.node.start_byte() < return_.unwrap().node.start_byte() => InferredReturnType {
                type_: ReturnType::Type(FatType::never()),
                return_node: Some(throw.node),
                explicit_type
            },
            (Some(return_), _) => {
                let expr = return_.returned_value.and_then(|x| typed_exprs.get(x));
                let (type_, explicit_type) = match expr {
                    None => (FatType::Any, None),
                    Some(expr) => (expr.type_, expr.explicit_type)
                };
                InferredReturnType {
                    type_: ReturnType::Type(type_),
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
        }))
    }

    pub fn has_any(&self, name: &TypeName) -> bool {
        let this = self.0.borrow();
        this.hoisted.contains_key(name)
    }

    pub fn get(&self, name: &TypeName) -> Option<Rc<AstTypeDecl<'tree>>> {
        let this = self.0.borrow();
        this.hoisted.get(name).cloned()
    }

    pub fn set(&self, decl: AstTypeDecl<'tree>) {
        let mut this = self.0.borrow_mut();
        this.hoisted.insert(decl.name().clone(), Rc::new(decl));
    }

    pub fn immediate_supertypes(&self, type_: ThinType) -> FatType {
        let ThinType::Nominal { id, nullability } = type_ else {
            Vec::new()
        };
        self.ident_supertypes(id, ty)
    }

    fn ident_supertypes(&self, type_: ThinType) -> Vec<Rc<AstTypeDecl<'tree>>> {

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