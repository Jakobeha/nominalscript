use type_sitter_lib::{TypedNodeGAT, TypedTree};
use type_sitter_lib::tree_sitter_wrapper::Tree;
pub use crate::type_sitter::nominalscript::nodes::*;

/// Typed nominalscript syntax tree
pub type ProgramTree = TypedTree<ProgramGAT>;
/// GAT of [Program]
pub struct ProgramGAT;

impl TypedNodeGAT for ProgramGAT {
    type This<'tree> = Program<'tree>;
}