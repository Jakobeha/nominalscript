use type_sitter_lib::{TypedNodeGAT, TypedTree};
pub use crate::type_sitter::nominalscript::nodes::*;

/// Typed nominalscript syntax tree
pub type ProgramTree = TypedTree<ProgramGAT>;
/// GAT of [Program]
#[derive(Debug)]
pub struct ProgramGAT;

impl TypedNodeGAT for ProgramGAT {
    type This<'tree> = Program<'tree>;
}