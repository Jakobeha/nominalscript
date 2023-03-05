use crate::analyses::scopes::{ExprTypeMap, ModuleScopes};
use crate::ast::tree_sitter::TSTree;

pub struct ModuleCtx<'tree> {
    tree: &'tree TSTree,
    pub scopes: ModuleScopes<'tree>,
    pub typed_exprs: ExprTypeMap<'tree>,
}

impl<'tree> ModuleCtx<'tree> {
    pub fn new(tree: &'tree TSTree) -> ModuleCtx<'tree> {
        ModuleCtx {
            tree,
            scopes: ModuleScopes::new(),
            typed_exprs: ExprTypeMap::new()
        }
    }
}