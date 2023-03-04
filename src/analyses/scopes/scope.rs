use std::collections::HashMap;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::ast::tree_sitter::TSNode;

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// `T` is value for value scopes, and `FatType` for type scopes.
pub struct ValueScope<'tree> {
    scope_node: TSNode<'tree>,
    did_set_params: bool,
    hoisted: HashMap<ValueName, ValueBinding<'tree>>,
    sequential: HashMap<ValueName, Vec<ValueBinding<'tree>>>,
    return_: Option<Return<'tree>>,
    throw: Option<Throw<'tree>>,
}

pub struct TypeScope<'tree> {
    scope_node: TSNode<'tree>,
    did_set_params: bool,
    hoisted: HashMap<TypeName, TypeBinding<'tree>>,
}

impl<'tree> ValueScope<'tree> {
    pub fn new(scope_node: TSNode<'tree>) -> ValueScope<'tree> {
        ValueScope {
            scope_node,
            did_set_params: false,
            hoisted: HashMap::new(),
            sequential: HashMap::new(),
            return_: None,
            throw: None,
        }
    }
}