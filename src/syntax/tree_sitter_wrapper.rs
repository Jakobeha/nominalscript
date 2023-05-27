pub struct Custom {

}

// TODO: Create macro in type_sitter_lib::tree_sitter_wrapper to make this a lot easier
pub type Tree = type_sitter_lib::tree_sitter_wrapper::Tree<Custom>;
pub type Node<'tree> = type_sitter_lib::tree_sitter_wrapper::Node<'tree, Custom>;
pub type TreeCursor<'tree> = type_sitter_lib::tree_sitter_wrapper::TreeCursor<'tree, Custom>;
