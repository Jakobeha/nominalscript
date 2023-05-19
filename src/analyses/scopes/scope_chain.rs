use std::borrow::Borrow;
use std::fmt::Display;
use std::hash::Hash;
use indexmap::Equivalent;
use crate::analyses::bindings::{DynValueBinding, ValueName};
use crate::analyses::global_bindings::GlobalValueBinding;
use crate::analyses::scopes::{ActiveScopePtr, ExprTypeMap};
use crate::analyses::types::{DynRlType, RlType};
use crate::concrete::tree_sitter::TSNode;
use crate::concrete::typed_nodes::{AstReturn, AstThrow, AstValueDecl, DynAstValueBinding};
use crate::diagnostics::FileLogger;
use crate::error;

pub struct ScopeChain<'tree> {
    scopes: Vec<ActiveScopePtr<'tree>>,
}

impl<'tree> ScopeChain<'tree> {
    pub fn new() -> Self {
        ScopeChain { scopes: Vec::new() }
    }

    pub fn push(&mut self, scope: ActiveScopePtr<'tree>) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Option<ActiveScopePtr<'tree>> {
        self.scopes.pop()
    }

    pub fn top(&self) -> Option<&ActiveScopePtr<'tree>> {
        self.scopes.last()
    }

    fn top_mut(&mut self) -> Option<&mut ActiveScopePtr<'tree>> {
        self.scopes.last_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, e: &FileLogger<'_>) {
        self.top_mut().expect("ScopeChain is empty").values.add_sequential(decl, e);
    }

    pub fn add_return(&mut self, return_: AstReturn<'tree>, e: &FileLogger<'_>) {
        self.top_mut().expect("ScopeChain is empty").values.add_return(return_, e);
    }

    pub fn add_throw(&mut self, throw: AstThrow<'tree>, e: &FileLogger<'_>) {
        self.top_mut().expect("ScopeChain is empty").values.add_throw(throw, e);
    }

    pub fn hoisted_in_top_scope<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynAstValueBinding<'tree>> where ValueName: Borrow<N> {
        self.top().expect("ScopeChain is empty").values.hoisted(name)
    }

    pub fn has_at_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> bool where ValueName: Borrow<N> {
        let mut top_to_bottom = self.iter_top_to_bottom();
        if let Some(top) = top_to_bottom.next() {
            if top.values.has_at_pos(name, pos_node) {
                return true
            }
        }
        top_to_bottom.any(|scope| scope.values.has_any(name)) ||
            GlobalValueBinding::has(name)
    }

    pub fn at_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> Option<&DynValueBinding<'tree>> where ValueName: Borrow<N> {
        let mut top_to_bottom = self.iter_top_to_bottom();
        if let Some(top) = top_to_bottom.next() {
            if let Some(decl) = top.values.at_pos(name, pos_node) {
                return Some(decl.up())
            }
        }
        top_to_bottom.find_map(|scope| scope.values.last(name).map(|decl| decl.up()))
            .or_else(|| GlobalValueBinding::get(name).map(|decl| decl as &DynValueBinding<'tree>))
    }

    pub fn at_exact_pos<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N, pos_node: TSNode<'tree>) -> Option<&AstValueDecl<'tree>> where ValueName: Borrow<N> {
        self.top().expect("ScopeChain is empty").values.at_exact_pos(name, pos_node)
    }

    /// Finds up type of an identifier by looking up its binding and returning its inferred type.
    ///
    /// A declaration's inferred type is its annotation if it has one, otherwise its initial value's
    /// type if it has an initial value, otherwise `Any`.
    ///
    /// A parameter's inferred type is its annotation if it has one, otherwise its initial value's
    /// type if it has an initial value, otherwise *a type hole* which is saved in the parameter
    /// for subsequent calls (backwards inference).
    ///
    /// If the identifier has no binding, then logs an error and returns `NEVER`.
    pub fn lookup<'a, N: Equivalent<ValueName> + Eq + Hash + Display + ?Sized>(
        use_id: &N,
        use_node: TSNode<'tree>,
        scope: &'a ScopeChain<'tree>,
        typed_exprs: &'a ExprTypeMap<'tree>,
        e: &FileLogger<'_>,
    ) -> &'a DynRlType where ValueName: Borrow<N> {
        scope
            .at_pos(use_id, use_node)
            .map(|def| def.infer_type(Some(typed_exprs)))
            .unwrap_or_else(|| {
                error!(e, "undeclared identifier `{}`", use_id => use_node);
                RlType::never_ref()
            })
    }

    fn iter_top_to_bottom(&self) -> impl Iterator<Item=&ActiveScopePtr<'tree>> + '_ {
        self.scopes.iter().rev()
    }
}