use lazy_static::lazy_static;

use tree_sitter_nominalscript::language_nominalscript;

use yak_sitter::Parser;
use crate::misc::NiceMutex;
pub use package::*;

/// Nominalscript syntax nodes
pub mod nodes;
/// Nominalscript syntax queries
pub mod queries;
mod package;

lazy_static! {
    pub static ref PARSER: NiceMutex<Parser> =
        NiceMutex::new(Parser::new(language_nominalscript()).expect("failed to load NominalScript parser"));
}