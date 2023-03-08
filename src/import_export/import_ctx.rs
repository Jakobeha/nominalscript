use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use derive_more::{Display, Error};
use crate::import_export::export::{ModulePath, TranspileOutHeader};
use crate::import_export::import_resolver::{ImportResolver, ImportResolverCreateError, ResolvedFatPath, ResolveFailure};

/// Caches and resolves imports.
/// Typically construct via `ImportCtx.regular` (which is async because it reads tsconfig and other I/O),
/// but you can call `new` for fine-grained control over module resolution
pub struct ImportCtx {
    /// Caches imports, does change when calling transpile
    cache: RefCell<ImportCache>,
    /// Stateless relative to program (doesn't change when calling transpile)
    resolver: ImportResolver
}

#[derive(Debug, Clone, Display, Error, PartialEq, Eq)]
pub enum ImportError {
    #[display(fmt = "could not locate module at path {}", module_path)]
    CouldNotResolve { #[error(not(source))] module_path: String },
    #[display(fmt = "cannot transpile located module because it it not nominalscript and has no nominalscript declaration ({})", fat_path)]
    NoNominalScript { #[error(not(source))] fat_path: ResolvedFatPath },
    #[display(fmt = "file not found at path {}", "path.display()")]
    CouldNotResolvePath { path: PathBuf, resolve_failure: ResolveFailure },
    #[display(fmt = "cannot transpile file at path because it's not nominalscript", "path.display()")]
    NotNominalScriptPath { #[error(not(source))] path: PathBuf },
    #[display(fmt = "could not load file at path {}: {}", "path.display()", error)]
    CouldNotLoad { path: PathBuf, #[error(source)] error: std::io::Error },
}

/// Caches imports
struct ImportCache {
    module_to_fat_path: HashMap<ModulePath, ResolvedFatPath>,
    fat_path_to_transpile_out: HashMap<ResolvedFatPath, Result<Rc<TranspileOutHeader>, ImportError>>
}

impl ImportCtx {
    pub fn regular(module_path: PathBuf) -> Result<Self, ImportResolverCreateError> {
        Ok(Self::new(ImportResolver::regular(module_path)?))
    }

    pub fn new(resolver: ImportResolver) -> Self {
        Self {
            cache: RefCell::new(ImportCache::new()),
            resolver
        }
    }

    /// Resolves the module path.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_and_cache_transpile(
        &self,
        module_path: &ModulePath,
        importer_path: Option<&Path>,
        transpile: impl FnOnce(&Path) -> Result<TranspileOutHeader, ImportError>
    ) -> Result<Rc<TranspileOutHeader>, ImportError> {
        let fat_path = ImportCache::cache_resolve_module(
            &self.cache,
            module_path,
            || self.resolver.locate(&module_path, importer_path)
        );
        if fat_path.is_null() {
            return Err(ImportError::CouldNotResolve { module_path: module_path.to_string() });
        }
        ImportCache::cache_transpile(
            &self.cache,
            fat_path,
            |fat_path| {
                let Some(nominalscript_path) = fat_path.nominalscript_path.as_ref() else {
                    return Err(ImportError::NoNominalScript { fat_path: fat_path.clone() });
                };
                transpile(nominalscript_path)
            }
        )
    }

    /// Resolves declarations / nominal exports and also checks that `scriptPath` is correct.
    /// If already partially or fully transpiled, returns the cached result.
    /// Otherwise, calls `transpile` with the actual script path
    pub fn resolve_auxillary_and_cache_transpile(
        &self,
        script_path: &Path,
        transpile: impl FnOnce() -> Result<TranspileOutHeader, ImportError>
    ) -> Result<Rc<TranspileOutHeader>, ImportError> {
        let fat_path = self.resolver
            .fat_script_path(script_path)
            .map_err(|resolve_failure| ImportError::CouldNotResolvePath { path: script_path.to_path_buf(), resolve_failure })?;
        if fat_path.nominalscript_path.is_none() {
            return Err(ImportError::NotNominalScriptPath { path: script_path.to_path_buf() });
        }
        ImportCache::cache_transpile2(
            &self.cache,
            fat_path,
            |_fat_path| transpile()
        )
    }
}

impl ImportCache {
    fn new() -> Self {
        Self {
            module_to_fat_path: HashMap::new(),
            fat_path_to_transpile_out: HashMap::new()
        }
    }

    fn cache_resolve_module(
        this: &RefCell<Self>,
        module: &ModulePath,
        resolve: impl FnOnce() -> Result<ResolvedFatPath, ResolveFailure>
    ) -> Ref<'_, ResolvedFatPath> {
        if let Ok(fat_path) = Ref::filter_map(this.borrow(), |this| this.module_to_fat_path.get(module)) {
            fat_path
        } else {
            let fat_path = resolve().unwrap_or_default();
            this.borrow_mut().module_to_fat_path.insert(module.clone(), fat_path);
            Ref::map(this.borrow(), |this| this.module_to_fat_path.get(module).unwrap())
        }
    }

    /// If the path has already been partially or fully transpiled, returns the cached result.
    /// Otherwis calls `transpile`.
    ///
    /// *Panics* if `fat_path` is null
    fn cache_transpile(
        this: &RefCell<Self>,
        fat_path: Ref<'_, ResolvedFatPath>,
        transpile: impl FnOnce(&ResolvedFatPath) -> Result<TranspileOutHeader, ImportError>
    ) -> Result<Rc<TranspileOutHeader>, ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Ok(transpile_result) = Ref::filter_map(this.borrow(), |this| this.fat_path_to_transpile_out.get(&*fat_path)) {
            transpile_result.clone()
        } else {
            // Drop fat_path before transpile, so we can borrow_mut
            let fat_path = fat_path.clone();
            let header = transpile(&fat_path).map(Rc::new);
            this.borrow_mut().fat_path_to_transpile_out.insert(fat_path, header.clone());
            header
        }
    }

    /// Same as [cache_transpile] but takes an owned `fat_path` instead of a [Ref].
    fn cache_transpile2(
        this: &RefCell<Self>,
        fat_path: ResolvedFatPath,
        transpile: impl FnOnce(&ResolvedFatPath) -> Result<TranspileOutHeader, ImportError>
    ) -> Result<Rc<TranspileOutHeader>, ImportError> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Ok(transpile_result) = Ref::filter_map(this.borrow(), |this| this.fat_path_to_transpile_out.get(&fat_path)) {
            transpile_result.clone()
        } else {
            let header = transpile(&fat_path).map(Rc::new);
            this.borrow_mut().fat_path_to_transpile_out.insert(fat_path, header.clone());
            header
        }
    }
}