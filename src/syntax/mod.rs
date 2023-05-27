use lazy_static::lazy_static;

use tree_sitter_nominalscript::language_nominalscript;

use crate::concrete::tree_sitter::TSParser;
use crate::misc::NiceMutex;
pub use package::*;

/// Nominalscript syntax nodes
pub mod nodes;
/// Nominalscript syntax queries
pub mod queries;
mod package;
/// tree-sitter-wrapper for types nodes, with custom `Tree` data
mod tree_sitter_wrapper;

lazy_static! {
    pub static ref PARSER: NiceMutex<TSParser> =
        NiceMutex::new(TSParser::new(language_nominalscript()).expect("failed to load NominalScript parser"));
}