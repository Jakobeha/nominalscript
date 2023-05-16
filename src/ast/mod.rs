use lazy_static::lazy_static;

use tree_sitter_nominalscript::language_nominalscript;

use crate::ast::tree_sitter::TSParser;
use crate::misc::NiceMutex;

/// Annotations: source info (general)
pub mod ann;
/// Parse "thin" semantic nodes for NominalScript (TODO update)
pub mod typed_nodes;

lazy_static! {
    pub static ref NOMINALSCRIPT_PARSER: NiceMutex<TSParser> =
        NiceMutex::new(TSParser::new(language_nominalscript()).expect("failed to load NominalScript parser"));
}