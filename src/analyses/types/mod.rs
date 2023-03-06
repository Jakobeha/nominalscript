/// Thin (pre-resolution, what is parsed/printed) types
mod thin;
/// Fat (post-resolution, includes supertypes) types + the unification algorithm
mod fat;
/// Inferred type = type with AST and other info from where/how it was inferred
mod inferred;
/// Locations in types
mod loc;

pub use thin::*;
pub use fat::*;
pub use inferred::*;
pub use loc::*;