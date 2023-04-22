/// Thin (pre-resolution, what is parsed/printed) and fat (post-resolution, includes supertypes) types + type analysis
pub mod types;
/// Scopes and contexts + scope analysis
pub mod scopes;
/// Datatypes for bindings
pub mod bindings;
/// Global bindings, and loads the global bindings script
pub mod global_bindings;