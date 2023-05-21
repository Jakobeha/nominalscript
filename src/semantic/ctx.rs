use std::collections::HashMap;
use crate::semantic::ann::Ann;
use crate::semantic::expr::{Expr, Type};

/// Project context = expression types, node annotations, and logger
pub struct ProjectCtx<'tree> {
    /// Expression types
    pub expr_types: ExprTypes<'tree>,
    /// Logger
    pub logger: Logger<'tree>
}

/// Maps expressions to types
pub struct ExprTypes<'tree> {

}

/// Maps semantic nodes to annotations
pub struct NodeAnnotations<'tree> {
    exprs: HashMap<Expr<'tree>, Ann<'tree>>,
    types: HashMap<Type<'tree>, Ann<'tree>>
}