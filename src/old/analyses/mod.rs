/// Thin (pre-resolution, what is parsed/printed) and fat (post-resolution, includes supertypes) types + type analysis
pub mod types;
/// Semantic "thin" types and values which are parsed from tree-sitter
pub mod semantic;
/// Scopes and contexts + scope analysis
pub mod scopes;
/// Datatypes for identifiers and bindings and global bindings
pub mod bindings;
