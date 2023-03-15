use std::rc::Rc;
use crate::analyses::bindings::{GlobalValueBinding, LocalValueBinding, ValueBinding, ValueName};
use crate::analyses::scopes::{ExprTypeMap, ModuleCtx, Scope, scope_parent_of};
use crate::analyses::types::RlType;
use crate::ast::tree_sitter::{TSCursor, TSNode};
use crate::ast::typed_nodes::AstValueDecl;
use crate::diagnostics::{error, FileLogger};

pub struct ScopeChain<'tree> {
    scopes: Vec<(TSNode<'tree>, Rc<Scope<'tree>>)>,
}

impl<'tree> ScopeChain {
    //noinspection RsUnreachableCode (this inspection is bugged for IntelliJ with let-else statements)
    pub fn new(ctx: &ModuleCtx<'tree>, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Self {
        let mut scopes = Vec::new();
        let mut scope_parent = scope_parent;
        loop {
            let scope = ctx.scopes.get(scope_parent, c);
            scopes.push((scope_parent, scope));
            let Some(next_scope_parent) = scope_parent_of(scope_parent, c) else {
                break
            };
            scope_parent = next_scope_parent;
        }
        scopes.reverse();
        Self { scopes }
    }

    pub fn push(&mut self, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) {
        let scope = ctx.scopes.get(scope_parent, c);
        self.scopes.push((scope_parent, scope));
    }

    pub fn pop(&mut self) -> Option<(TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.pop()
    }

    fn top(&self) -> Option<&(TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.last()
    }

    fn top_mut(&mut self) -> Option<&mut (TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.last_mut()
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, c: &mut TSCursor<'tree>) {
        self.top_mut().expect("ScopeChain is empty").1.add_sequential(decl, c);
    }

    pub fn add_return(&mut self, stmt: TSNode<'tree>, value: Option<TSNode<'tree>>) {
        self.top_mut().expect("ScopeChain is empty").1.add_return(stmt, value);
    }

    pub fn add_throw(&mut self, stmt: TSNode<'tree>, value: Option<TSNode<'tree>>) {
        self.top_mut().expect("ScopeChain is empty").1.add_throw(stmt, value);
    }

    pub fn hoisted_in_top_scope(&self, name: &ValueName) -> Option<Rc<dyn LocalValueBinding<'tree>>> {
        self.top().expect("ScopeChain is empty").1.hoisted(name)
    }

    pub fn has_at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> bool {
        let mut top_to_bottom = self.scopes.iter().rev();
        if let Some((_, top)) = top_to_bottom.next() {
            if top.has_at_pos(name, pos_node) {
                return true
            }
        }
        top_to_bottom.any(|(_, scope)| scope.has_any(name)) ||
            GlobalValueBinding::has(name)
    }

    pub fn at_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<Rc<dyn ValueBinding<'tree>>> {
        let mut top_to_bottom = self.scopes.iter().rev();
        if let Some((_, top)) = top_to_bottom.next() {
            if let Some(decl) = top.at_pos(name, pos_node) {
                return Some(decl)
            }
        }
        top_to_bottom.find_map(|(_, scope)| scope.last(name))
            .or_else(|| GlobalValueBinding::get)
    }

    pub fn at_exact_pos(&self, name: &ValueName, pos_node: TSNode<'tree>) -> Option<Rc<dyn ValueBinding<'tree>>> {
        self.top().expect("ScopeChain is empty").1.at_exact_pos(name, pos_node)
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
        scope.at_pos(use_id, use_node)
            .and_then(|def| def.infer_type(typed_exprs))
            .unwrap_or_else(|| {
                error!(e, "undeclared identifier `{}`", use_id => use_node);
                &RlType::NEVER
            })
    }
}