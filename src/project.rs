use std::borrow::Cow;
use std::path::{Path, PathBuf};
use derive_more::{Display, From};
use crate::compile::begin_transpile_file;
use crate::diagnostics::ProjectDiagnostics;
use crate::import_export::export::{AlreadyTranspiled, Module};
use crate::import_export::import_cache::ImportCache;
use crate::import_export::import_ctx::{ImportError, ProjectImportCtx};
use crate::import_export::import_resolver::{ImportResolver, ImportResolverCreateError};

/// Project datastructure which contains everything.
///
/// In reality you will use a [ProjectCtx] which contains references to this data,
/// since you will be passing it through `FnOnce` in import resolution.
#[derive(Debug)]
pub struct Project {
    /// Caches imports, does change when calling transpile
    pub(crate) import_cache: ImportCache,
    /// Stateless relative to program (doesn't change when calling transpile)
    pub(crate) import_resolver: ImportResolver,
    pub diagnostics: ProjectDiagnostics,
}

/// Project environment = reference to project data
#[derive(Debug, Clone, Copy)]
pub struct ProjectCtx<'a> {
    pub import_ctx: ProjectImportCtx<'a>,
    pub diagnostics: &'a ProjectDiagnostics,
}

/// Error when transpiling
#[derive(Debug, Clone, Display, From)]
pub enum TranspileError<'a> {
    /// Error importing e.g. resolving the file
    ImportError(Cow<'a, ImportError>),
    /// Already transpiled (if you want to you must manually remember the output associated with the path)
    AlreadyTranspiled(AlreadyTranspiled),
}

impl Project {
    pub fn regular(package_path: PathBuf, print_diagnostics_immediately: bool) -> Result<Self, ImportResolverCreateError> {
        Ok(Self::new(ImportResolver::regular(package_path)?, print_diagnostics_immediately))
    }

    pub fn new(import_resolver: ImportResolver, print_diagnostics_immediately: bool) -> Self {
        Self {
            import_cache: ImportCache::new(),
            import_resolver,
            diagnostics: ProjectDiagnostics::new(print_diagnostics_immediately)
        }
    }

    pub fn ctx(&self) -> ProjectCtx<'_> {
        ProjectCtx {
            import_ctx: ProjectImportCtx::new(&self.import_cache, &self.import_resolver),
            diagnostics: &self.diagnostics,
        }
    }

    /// Transpile (compile) a NominalScript file into TypeScript.
    /// This will parse the exports. You can call [Module::finish] to finish transpiling.
    ///
    /// If successful, the transpiled file (module) will be cached in `ctx`,
    /// so calling again will just return the same result
    pub fn begin_transpile_file(
        &self,
        script_path: &Path,
    ) -> Result<&Module, Cow<ImportError>> {
        begin_transpile_file(script_path, self.ctx())
    }

    /// Fully transpile (compile) a NominalScript file into TypeScript.
    ///
    /// If successful, the transpiled file (module) will be cached in `ctx`,
    /// and calling again will return [TranspileError::AlreadyTranspiled]
    pub fn transpile_file(
        &self,
        script_path: &Path,
    ) -> Result<String, TranspileError<'_>> {
        let ctx = self.ctx();
        Ok(begin_transpile_file(script_path, ctx)?.finish(ctx)?)
    }
}