pub use package::*;

/// Annotations = source (syntax) info for semantic nodes
pub mod ann;
/// Scope, toplevel scope, etc.
pub mod scope;
/// Value and type declarations
pub mod def;
/// Value and type references
pub mod r#use;
/// Value and type expressions
pub mod expr;
/// Project context = expression types, node annotations, and logger
pub mod ctx;
mod package;
