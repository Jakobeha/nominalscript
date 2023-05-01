use lazy_static::lazy_static;

use tree_sitter_nominalscript::language_nominalscript;

use crate::ast::tree_sitter::TSParser;
use crate::misc::NiceMutex;

/// Wrapper/shim around the tree-sitter library providing misc functionality
pub mod tree_sitter;
/// Tree-sitter queries for NominalScript
pub mod queries;
/// General-purpose typed node wrapper macros, and typed node wrappers for NominalScript
/// (???: migrate into a separate module or crate?)
pub mod typed_nodes;
/// Annotations: source info
pub mod ann;

lazy_static! {
    pub static ref NOMINALSCRIPT_PARSER: NiceMutex<TSParser> =
        NiceMutex::new(TSParser::new(language_nominalscript()).expect("failed to load NominalScript parser"));
}