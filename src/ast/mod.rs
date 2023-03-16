use lazy_static::lazy_static;
use crate::ast::tree_sitter::{TSParser, TSRange};
use std::path::PathBuf;
use tree_sitter_nominalscript::language_nominalscript;
use crate::misc::NiceMutex;
use crate::import_export::export::ImportPath;

/// Wrapper for arbitrary tree-sitter nodes, queries, and other datatypes
pub mod tree_sitter;
/// Tree-sitter queries for NominalScript
pub mod queries;
/// Typed nodes for NominalScript (not all nodes get their own type, but some do)
pub mod typed_nodes;

lazy_static! {
    pub static ref NOMINALSCRIPT_PARSER: NiceMutex<TSParser> =
        NiceMutex::new(TSParser::new(language_nominalscript()).expect("failed to load NominalScript parser"));
}