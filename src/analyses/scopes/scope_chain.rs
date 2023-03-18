use crate::analyses::bindings::{GlobalValueBinding, ValueBinding, ValueName};
use crate::analyses::scopes::{ExprTypeMap, ModuleCtx, scope_parent_of, ScopePtr, WeakScopePtr};
use crate::analyses::types::RlType;
use crate::ast::tree_sitter::{TSCursor, TSNode};
use crate::ast::typed_nodes::{AstReturn, AstThrow, AstValueBinding, AstValueDecl};
use crate::diagnostics::{FileLogger};
use crate::error;

pub struct ScopeChain<'tree> {
    scopes: Vec<(TSNode<'tree>, WeakScopePtr<'tree>)>,
}

impl<'tree> ScopeChain<'tree> {
    //noinspection RsUnreachableCode (this inspection is bugged for IntelliJ with let-else statements)
    pub fn new(ctx: &mut ModuleCtx<'tree>, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Self {
        let mut scopes = Vec::new();
        let mut scope_parent = scope_parent;
        loop {
            let scope = ctx.scopes.get(scope_parent, c).downgrade();
            scopes.push((scope_parent, scope));
            let Some(next_scope_parent) = scope_parent_of(scope_parent, c) else {
                break
            };
            scope_parent = next_scope_parent;
        }
        scopes.reverse();
        Self { scopes }
    }

    pub fn push(&mut self, ctx: &mut ModuleCtx<'tree>, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) {
        let scope = ctx.scopes.get(scope_parent, c).downgrade();
        self.scopes.push((scope_parent, scope));
    }

    pub fn pop(&mut self) -> Option<(TSNode<'tree>, ScopePtr<'tree>)> {
        self.scopes.pop().map(|(node, scope)|
            (node, scope.upgrade().expect("ScopeChain has dangling WeakScopePtr")))
    }

    fn top(&self) -> Option<(TSNode<'tree>, ScopePtr<'tree>)> {
        self.scopes.last().map(|(node, scope)|
            (*node, scope.upgrade().expect("ScopeChain has dangling WeakScopePtr")))
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, e: &mut FileLogger<'_>) {
        self.top().expect("ScopeChain is empty").1.borrow_mut().values_mut().add_sequential(decl, e);
    }

    pub fn add_return(&mut self, return_: AstReturn<'tree>, e: &mut FileLogger<'_>) {
        self.top().expect("ScopeChain is empty").1.borrow_mut().values_mut().add_return(return_, e);
    }

    pub fn add_throw(&mut self, throw: AstThrow<'tree>, e: &mut FileLogger<'_>) {
        self.top().expect("ScopeChain is empty").1.borrow_mut().values_mut().add_throw(throw, e);
    }

    pub fn hoisted_in_top_scope(&self, name: &ValueName) -> Option<&dyn AstValueBinding<'tree>> {
        self.top().expect("ScopeChain is empty").1.values.hoisted(name)
    }

    pub fn has_at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> bool {
        let mut top_to_bottom = self.iter_top_to_bottom();
        if let Some((_, top)) = top_to_bottom.next() {
            if top.values.has_at_pos(name, pos_node) {
                return true
            }
        }
        top_to_bottom.any(|(_, scope)| scope.values.has_any(name)) ||
            GlobalValueBinding::has(name)
    }

    pub fn at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&dyn ValueBinding> {
        let mut top_to_bottom = self.iter_top_to_bottom();
        if let Some((_, top)) = top_to_bottom.next() {
            if let Some(decl) = top.values.at_pos(name, pos_node) {
                return Some(decl)
            }
        }
        top_to_bottom.find_map(|(_, scope)| scope.values.last(name).map(|decl| decl as &dyn ValueBinding))
            .or_else(|| GlobalValueBinding::get(name).map(|decl| decl as &dyn ValueBinding))
    }

    pub fn at_exact_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<&AstValueDecl<'tree>> {
        self.top().expect("ScopeChain is empty").1.values.at_exact_pos(name, pos_node)
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
    pub fn lookup<'a>(
        use_id: &ValueName,
        use_node: TSNode<'_>,
        scope: &'a ScopeChain,
        typed_exprs: &'a ExprTypeMap,
        e: &mut FileLogger<'_>,
    ) -> &'a RlType {
        scope
            .at_pos(use_id, use_node)
            .map(|def| def.infer_type(Some(typed_exprs)))
            .unwrap_or_else(|| {
                error!(e, "undeclared identifier `{}`", use_id => use_node);
                &RlType::NEVER
            })
    }

    fn iter_top_to_bottom(&self) -> impl Iterator<Item=(TSNode<'tree>, ScopePtr<'tree>)> + '_ {
        self.scopes.iter().rev().map(|(node, scope)|
            (*node, scope.upgrade().expect("ScopeChain has dangling WeakScopePtr")))
    }
}