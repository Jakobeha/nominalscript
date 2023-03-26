use crate::import_export::import_resolver::ResolvedFatPath;

/// Export datatypes and the output datatype
pub mod export;
/// Caches imports
pub(crate) mod import_cache;
/// Import context which caches and resolves imports (combines [import_cache] and [import_resolver])
pub mod import_ctx;
/// Import resolution strategy and module root path (stateless import context)
pub mod import_resolver;

/// References a module. This is an alias for [ResolvedFatPath]
///
/// **Note:** "Module" in this case refers to what can be imported - "internal modules" are not
/// considered modules in the codebase. This may change in the future to make it more clear
pub type ModulePath = ResolvedFatPath;