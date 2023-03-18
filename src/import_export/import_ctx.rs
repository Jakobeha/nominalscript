use std::cell::Ref;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use derive_more::{Display, Error};

use crate::compile::FatalTranspileError;
use crate::import_export::export::{ImportPath, Module};
use crate::import_export::import_cache::ImportCache;
use crate::import_export::import_resolver::{ImportResolver, ResolvedFatPath, ResolveFailure};

/// Caches and resolves imports.
#[derive(Debug)]
pub struct ProjectImportCtx<'a> {
    /// Caches imports, does change when calling transpile
    cache: &'a ImportCache,
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

impl<'a> ProjectImportCtx<'a> {
    pub(crate) fn new(cache: &'a ImportCache, resolver: &'a ImportResolver) -> Self {
        Self { cache, resolver }
    }

    pub(crate) fn file<'b>(&'b self, importer_path: &'b Path) -> FileImportCtx<'b> {
        FileImportCtx { project_ctx: ProjectImportCtx::new(&self.cache, &self.resolver), importer_path }
    }

    /// Resolves the module path.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    fn resolve_and_cache_transpile(
        &self,
        importer_path: &Path,
        module_path: &ImportPath,
        transpile: impl FnOnce(&Path) -> Result<Module, ImportError>
    ) -> Result<(&Path, Rc<Module>), Rc<ImportError>> {
        let fat_path = self.resolve_and_cache_fat_path(importer_path, module_path);
        if fat_path.is_null() {
            return Err(Rc::new(ImportError::CouldNotResolve { module_path: module_path.to_string() }));
        }
        let nominalscript_path = fat_path.nominalscript_path.clone();
        self.cache.cache_transpile(
            fat_path,
            || {
                let Some(nominalscript_path) = nominalscript_path.as_ref() else {
                    return Err(ImportError::NoNominalScript { fat_path: fat_path.clone() });
                };
                transpile(nominalscript_path)
            }
        ).map(|module| (nominalscript_path.expect("nullity should be checked in transpile or cached"), module))
    }

    /// Resolves declarations / nominal exports and also checks that `scriptPath` is correct.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_auxillary_and_cache_transpile(
        &self,
        script_path: &Path,
        transpile: impl FnOnce() -> Result<Module, ImportError>
    ) -> Result<Rc<Module>, Rc<ImportError>> {
        let fat_path = self.resolver
            .fat_script_path(script_path)
            .map_err(|resolve_failure| Rc::new(ImportError::CouldNotResolvePath { path: script_path.to_path_buf(), resolve_failure }))?;
        if fat_path.nominalscript_path.is_none() {
            return Err(Rc::new(ImportError::NotNominalScriptPath { path: script_path.to_path_buf() }));
        }
        self.cache.cache_transpile2(fat_path, transpile)
    }

    fn resolve_and_cache_fat_path(&self, importer_path: &Path, module_path: &ImportPath) -> Ref<'_, ResolvedFatPath> {
        self.cache.cache_resolve_module(
            (importer_path, module_path),
            || self.resolver.locate(&module_path, Some(importer_path))
        )
    }
}

impl<'a> FileImportCtx<'a> {
    pub(crate) fn other_file<'b>(&'b self, path: &'b Path) -> FileImportCtx<'b> {
        self.project_ctx.file(path)
    }

    /// Resolves the module path.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_and_cache_transpile(
        &self,
        module_path: &ImportPath,
        transpile: impl FnOnce(&Path) -> Result<Module, ImportError>
    ) -> Result<(&Path, Rc<Module>), Rc<ImportError>> {
        self.project_ctx.resolve_and_cache_transpile(self.importer_path, module_path, transpile)
    }
}

impl From<FatalTranspileError> for ImportError {
    fn from(error: FatalTranspileError) -> Self {
        ImportError::FailedToTranspile { error }
    }
}

impl Clone for ImportError {
    fn clone(&self) -> Self {
        match self {
            ImportError::CouldNotResolve { module_path } => ImportError::CouldNotResolve { module_path: module_path.clone() },
            ImportError::NoNominalScript { fat_path } => ImportError::NoNominalScript { fat_path: fat_path.clone() },
            ImportError::CouldNotResolvePath { path, resolve_failure } => ImportError::CouldNotResolvePath { path: path.clone(), resolve_failure: resolve_failure.clone() },
            ImportError::NotNominalScriptPath { path } => ImportError::NotNominalScriptPath { path: path.clone() },
            ImportError::CouldNotLoad { path, error } => ImportError::CouldNotLoad { path: path.clone(), error: std::io::Error::from(error.kind()) },
            ImportError::FailedToTranspile { error } => ImportError::FailedToTranspile { error: error.clone() }
        }
    }
}