/// Export datatypes and the output datatype
pub mod export;
/// Caches imports
pub(crate) mod import_cache;
/// Import context which caches and resolves imports (combines [import_cache] and [import_resolver])
pub mod import_ctx;
/// Import resolution strategy and module root path (stateless import context)
pub mod import_resolver;
/// Wrapper for [FrozenVec] for maps with module id keys
pub(crate) mod module_id_map;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);