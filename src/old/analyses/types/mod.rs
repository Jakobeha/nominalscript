/// Thin (pre-resolution, what is parsed/printed) types
mod thin;
/// Fat (post-resolution, includes supertypes) types + the unification algorithm
mod fat;
/// Abstracted data-structues shared by thin and fat types
mod generic;
/// Type resolution + resolved lazy, which combines thin and fat type to handle cyclic dependencies,
/// resolution caching, etc.
mod resolve;
/// Determined type = type with AST and other info from where/how it was determined, for diagnostics
mod determined;
/// Locations in types
mod loc;

pub use thin::*;
pub use fat::*;
pub use generic::*;
pub use resolve::*;
pub use determined::*;
pub use loc::*;