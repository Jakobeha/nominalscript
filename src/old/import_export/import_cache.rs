use std::fmt::{Debug, Formatter};
use std::path::{Path, PathBuf};

use elsa::FrozenMap;

use crate::import_export::export::{ImportPath, Module};
use crate::import_export::import_ctx::ImportError;
use crate::import_export::import_resolver::{ResolvedFatPath, ResolveFailure};
use crate::import_export::ModulePath;

/// Caches imports
pub(crate) struct ImportCache {
    import_to_fat_path: FrozenMap<PathBuf, Box<FrozenMap<ImportPath, Box<ResolvedFatPath>>>>,
    module_transpile_out: FrozenMap<ModulePath, Box<Result<Module, ImportError>>>
}

impl ImportCache {
    pub(crate) fn new() -> Self {
        Self {
            import_to_fat_path: FrozenMap::new(),
            module_transpile_out: FrozenMap::new()
        }
    }

    pub(super) fn cache_resolve_module(
        &self,
        (importer_path, module_path): (&Path, &ImportPath),
        resolve: impl FnOnce() -> Result<ResolvedFatPath, ResolveFailure>
    ) -> &ResolvedFatPath {
        let module_path_to_fat_path = self.import_to_fat_path.get(importer_path)
            .unwrap_or_else(|| self.import_to_fat_path.insert(importer_path.to_path_buf(), Box::new(FrozenMap::new())));
        module_path_to_fat_path.get(module_path)
            .unwrap_or_else(|| module_path_to_fat_path.insert(module_path.clone(), Box::new(resolve().unwrap_or_default())))
    }

    /// If the path has already been partially or fully transpiled, returns the cached result.
    /// Otherwis calls `transpile`.
    ///
    /// *Panics* if `fat_path` is null
    pub(super) fn cache_transpile(
        &self,
        fat_path: &ResolvedFatPath,
        transpile: impl FnOnce() -> Result<Module, ImportError>
    ) -> Result<&Module, &ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        self.module_transpile_out.get(fat_path)
            .unwrap_or_else(|| self.module_transpile_out.insert(fat_path.clone(), Box::new(transpile())))
            .as_ref()
    }

    /// Same as [cache_transpile] but takes an owned `fat_path` instead of a [Ref].
    pub(super) fn cache_transpile2(
        &self,
        fat_path: ResolvedFatPath,
        transpile: impl FnOnce(&ResolvedFatPath) -> Result<Module, ImportError>
    ) -> Result<&Module, &ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        self.module_transpile_out.get(&fat_path)
            .unwrap_or_else(|| {
                let transpiled = transpile(&fat_path);
                self.module_transpile_out.insert(fat_path, Box::new(transpiled))
            })
            .as_ref()
    }
}

impl Debug for ImportCache {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImportCache")
            .field("import_to_fat_path", &"...")
            .field("module_transpile_out", &"...")
            .finish()
    }
}
