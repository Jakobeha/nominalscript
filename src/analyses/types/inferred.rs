use crate::analyses::types::{FatType, ReturnType};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstType;

pub struct InferredType<'tree> {
    pub type_: FatType,
    pub inferred_from: TSNode<'tree>,
    /// If this type was not "inferred" but explicitly provided, this will be set.
    /// If this is set, `inferred_from` will usually be the type's node (but not enforced).
    pub explicit_type: Option<AstType<'tree>>,
}

pub struct InferredReturnType<'tree> {
    pub type_: ReturnType<FatType>,
    pub return_node: Option<TSNode<'tree>>,
    pub explicit_type: Option<AstType<'tree>>,
}

impl<'tree> InferredType<'tree> {
    pub fn new(type_: FatType, inferred_from: TSNode<'tree>) -> Self {
        Self { type_, inferred_from, explicit_type: None }
    }
}

impl<'a, 'tree> From<&'a AstType<'tree>> for InferredType<'tree> {
    fn from(type_: &'a AstType<'tree>) -> Self {
        Self {
            type_: type_.type_.clone(),
            inferred_from: type_.node,
            explicit_type: Some(type_.clone())
        }
    }
}