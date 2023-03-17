/// Imports (outgoing edges) which let you data in the scope refer to other files
mod scope_imports;
/// A single scope (lexical or non-lexical)
mod scope;
/// Reference-counted pointer to a scope with the capability to erase and unsafely re-assign the lifetime
mod scope_ptr;
/// A scope chain which lets you actually lookup values (name -> expression (ast node))
mod scope_chain;
/// Contains all scopes of value or nominal type
mod module_scopes;
/// Maps expressions (ast nodes) to their types
mod expr_type_map;
/// Contains the value and nominal type module scopes, and map of expressions to types
mod module_ctx;

pub use scope_imports::*;
pub use scope::*;
pub use scope_ptr::*;
pub use scope_chain::*;
pub use module_scopes::*;
pub use expr_type_map::*;
pub use module_ctx::*;
