use lazy_static::lazy_static;

/// Wrapper for arbitrary tree-sitter nodes, queries, and other datatypes
pub mod tree_sitter;
/// Tree-sitter queries for NominalScript
pub mod queries;
/// Typed nodes for NominalScript (not all nodes get their own type, but some do)
pub mod typed_nodes;


lazy_static! {
    pub static ref NOMINALSCRIPT_PARSER: TSParser =
        TSParser::new(TREE_SITTER_NOMINALSCRIPT).expect("failed to load NominalScript parser");
}