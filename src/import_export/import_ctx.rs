use std::collections::HashMap;
use std::path::{Path, PathBuf};

use derive_more::{Display, Error};

use crate::compile::FatalTranspileError;
use crate::import_export::export::{ImportPath, Module};
use crate::import_export::import_resolver::{ImportResolver, ResolvedFatPath, ResolveFailure};
use crate::misc::Oob;

/// Caches and resolves imports.
#[derive(Debug)]
pub struct ProjectImportCtx<'a> {
    /// Caches imports, does change when calling transpile
    cache: &'a mut ImportCache,
    /// Stateless relative to program (doesn't change when calling transpile)
    resolver: &'a ImportResolver
}

/// Caches and resolves imports for a particular file.
///
/// Necessary because the same module path in different files may point to difference modules
#[derive(Debug)]
pub struct FileImportCtx<'a> {
    pub project_ctx: ProjectImportCtx<'a>,
    pub(crate) importer_path: &'a Path
}

#[derive(Debug, Display, Error)]
pub enum ImportError {
    #[display(fmt = "could not locate module at path {}", module_path)]
    CouldNotResolve { #[error(not(source))] module_path: String },
    #[display(fmt = "cannot transpile located module because it it not nominalscript and has no nominalscript declaration ({})", fat_path)]
    NoNominalScript { #[error(not(source))] fat_path: ResolvedFatPath },
    #[display(fmt = "file not found at path {}", "path.display()")]
    CouldNotResolvePath { path: PathBuf, resolve_failure: ResolveFailure },
    #[display(fmt = "cannot transpile file at path because it's not nominalscript: {}", "path.display()")]
    NotNominalScriptPath { #[error(not(source))] path: PathBuf },
    #[display(fmt = "could not load file at path {}: {}", "path.display()", error)]
    CouldNotLoad { path: PathBuf, #[error(source)] error: std::io::Error },
    FailedToTranspile { #[error(source)] error: FatalTranspileError }
}

impl From<FatalTranspileError> for ImportError {
    fn from(error: FatalTranspileError) -> Self {
        ImportError::FailedToTranspile { error }
    }
}

/// Caches imports
#[derive(Debug)]
pub(crate) struct ImportCache {
    module_to_fat_path: HashMap<PathBuf, HashMap<ImportPath, ResolvedFatPath>>,
    fat_path_to_transpile_out: HashMap<ResolvedFatPath, Result<Module, ImportError>>
}

impl<'a> ProjectImportCtx<'a> {
    pub(crate) fn new(cache: &'a mut ImportCache, resolver: &'a ImportResolver) -> Self {
        Self { cache, resolver }
    }

    pub(crate) fn shorten_lifetime(&self) -> ProjectImportCtx {
        ProjectImportCtx { cache: self.cache, resolver: self.resolver }
    }

    pub(crate) fn file<'b>(&'b mut self, importer_path: &'b Path) -> FileImportCtx<'b> {
        FileImportCtx { project_ctx: ProjectImportCtx::new(&mut self.cache, &self.resolver), importer_path }
    }

    /// Resolves the module path.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    fn resolve_and_cache_transpile(
        &mut self,
        importer_path: &Path,
        module_path: &ImportPath,
        transpile: impl FnOnce(&Path, ProjectImportCtx<'_>) -> Result<Module, ImportError>
    ) -> Result<(&Path, &Module), Oob<'_, ImportError>> {
        let fat_path = self.resolve_and_cache_fat_path(importer_path, module_path);
        if fat_path.is_null() {
            return Err(Oob::Owned(ImportError::CouldNotResolve { module_path: module_path.to_string() }));
        }
        self.cache.cache_transpile(
            fat_path,
            |fat_path, cache| {
                let Some(nominalscript_path) = fat_path.nominalscript_path.as_ref() else {
                    return Err(ImportError::NoNominalScript { fat_path: fat_path.clone() });
                };
                transpile(nominalscript_path, ProjectImportCtx::new(cache, &self.resolver))
            }
        ).as_ref()
            .map(|module| (fat_path.nominalscript_path.as_deref().unwrap(), module))
            .map_err(Oob::Borrowed)
    }

    /// Resolves declarations / nominal exports and also checks that `scriptPath` is correct.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_auxillary_and_cache_transpile(
        &mut self,
        script_path: &Path,
        transpile: impl FnOnce(ProjectImportCtx<'_>) -> Result<Module, ImportError>
    ) -> Result<&Module, Oob<'_, ImportError>> {
        let fat_path = self.resolver
            .fat_script_path(script_path)
            .map_err(|resolve_failure| Oob::Owned(ImportError::CouldNotResolvePath { path: script_path.to_path_buf(), resolve_failure }))?;
        if fat_path.nominalscript_path.is_none() {
            return Err(Oob::Owned(ImportError::NotNominalScriptPath { path: script_path.to_path_buf() }));
        }
        self.cache.cache_transpile2(
            fat_path,
            |_fat_path, cache| transpile(ProjectImportCtx::new(cache, &self.resolver))
        ).as_ref().map_err(Oob::Borrowed)
    }

    pub fn resolve_and_cache_script_path(&mut self, importer_path: &Path, module_path: &ImportPath) -> Result<&Path, ImportError> {
        let fat_path = self.resolve_and_cache_fat_path(importer_path, module_path);
        fat_path.nominalscript_path.as_deref()
            .ok_or_else(|| ImportError::NoNominalScript { fat_path: fat_path.clone() })
    }


    fn resolve_and_cache_fat_path(&mut self, importer_path: &Path, module_path: &ImportPath) -> &ResolvedFatPath {
        self.cache.cache_resolve_module(
            (importer_path, module_path),
            || self.resolver.locate(&module_path, Some(importer_path))
        )
    }
}

impl<'a> FileImportCtx<'a> {
    pub(crate) fn shorten_lifetime(&mut self) -> FileImportCtx<'_> {
        FileImportCtx {
            project_ctx: self.project_ctx.shorten_lifetime(),
            importer_path: self.importer_path
        }
    }

    pub(crate) fn other_file<'b>(&'b mut self, path: &'b Path) -> FileImportCtx<'b> {
        self.project_ctx.file(path)
    }

    pub(crate) fn with_other_file<R>(&mut self, path: &Path, fun: impl FnOnce(&mut Self) -> R) -> R {
        let original_path = self.importer_path;
        self.importer_path = path;
        let result = fun(&mut other);
        self.importer_path = original_path;
        result
    }

    /// Resolves the module path.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_and_cache_transpile(
        &mut self,
        module_path: &ImportPath,
        transpile: impl FnOnce(&Path, ProjectImportCtx<'_>) -> Result<Module, ImportError>
    ) -> Result<(&Path, &Module), Oob<'_, ImportError>> {
        self.project_ctx.resolve_and_cache_transpile(self.importer_path, module_path, transpile)
    }

    pub fn resolve_and_cache_script_path(&mut self, module_path: &ImportPath) -> Result<&Path, ImportError> {
        self.project_ctx.resolve_and_cache_script_path(self.importer_path, module_path)
    }
}


impl ImportCache {
    pub(crate) fn new() -> Self {
        Self {
            module_to_fat_path: HashMap::new(),
            fat_path_to_transpile_out: HashMap::new()
        }
    }

    fn cache_resolve_module(
        &mut self,
        (importer_path, module_path): (&Path, &ImportPath),
        resolve: impl FnOnce() -> Result<ResolvedFatPath, ResolveFailure>
    ) -> &ResolvedFatPath {
        if let Some(fat_path) = self.module_to_fat_path.get(importer_path).and_then(|m| m.get(module_path)) {
            return fat_path
        }
        let fat_path = resolve().unwrap_or_default();
        let module_to_fat_path = self.module_to_fat_path
            .entry(importer_path.to_path_buf())
            .or_default();
        let std::collections::hash_map::Entry::Vacant(entry) = module_to_fat_path.entry(module_path.clone()) else {
            unreachable!("we just checked that this entry doesn't exist")
        };
        entry.insert(fat_path)
    }

    /// If the path has already been partially or fully transpiled, returns the cached result.
    /// Otherwis calls `transpile`.
    ///
    /// *Panics* if `fat_path` is null
    fn cache_transpile(
        &mut self,
        fat_path: &ResolvedFatPath,
        transpile: impl FnOnce(&ResolvedFatPath, &mut ImportCache) -> Result<Module, ImportError>
    ) -> &Result<Module, ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Some(transpile_result) = self.fat_path_to_transpile_out.get(fat_path) {
            return transpile_result
        }
        let transpiled = transpile(&fat_path, self);
        let std::collections::hash_map::Entry::Vacant(entry) = self.fat_path_to_transpile_out.entry(fat_path.clone()) else {
            unreachable!("we just checked that this entry doesn't exist")
        };
        entry.insert(transpiled)
    }

    /// Same as [cache_transpile] but takes an owned `fat_path` instead of a [Ref].
    fn cache_transpile2(
        &mut self,
        fat_path: ResolvedFatPath,
        transpile: impl FnOnce(&ResolvedFatPath, &mut ImportCache) -> Result<Module, ImportError>
    ) -> &Result<Module, ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Some(transpile_result) = self.fat_path_to_transpile_out.get(&fat_path) {
            return transpile_result
        }
        let transpiled = transpile(&fat_path, self);
        let std::collections::hash_map::Entry::Vacant(entry) = self.fat_path_to_transpile_out.entry(fat_path) else {
            unreachable!("we just checked that the entry doesn't exist")
        };
        entry.insert(transpiled)
    }
}
