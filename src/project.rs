use std::path::PathBuf;
use crate::diagnostics::{FileDiagnostics, ProjectDiagnostics};
use crate::import_export::import_cache::ImportCache;
use crate::import_export::import_ctx::{FileImportCtx, ProjectImportCtx};
use crate::import_export::import_resolver::{ImportResolver, ImportResolverCreateError};
use crate::import_export::ModulePath;

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
#[derive(Debug)]
pub struct ProjectCtx<'a> {
    pub import_ctx: ProjectImportCtx<'a>,
    pub diagnostics: &'a ProjectDiagnostics,
}

/// File environment = reference to file data
#[derive(Debug)]
pub struct FileCtx<'a> {
    pub import_ctx: FileImportCtx<'a>,
    pub diagnostics: &'a FileDiagnostics,
    pub project_diagnostics: &'a ProjectDiagnostics,
}

impl Project {
    pub fn regular(package_path: PathBuf) -> Result<Self, ImportResolverCreateError> {
        Ok(Self::new(ImportResolver::regular(package_path)?))
    }

    pub fn new(import_resolver: ImportResolver) -> Self {
        Self {
            import_cache: ImportCache::new(),
            import_resolver,
            diagnostics: ProjectDiagnostics::new(),
        }
    }

    pub fn ctx(&self) -> ProjectCtx<'_> {
        ProjectCtx {
            import_ctx: ProjectImportCtx::new(&self.import_cache, &self.import_resolver),
            diagnostics: &self.diagnostics,
        }
    }
}

impl<'a> ProjectCtx<'a> {
    pub fn file<'b>(&'b self, importer_path: &'b ModulePath) -> FileCtx<'b> {
        FileCtx {
            import_ctx: self.import_ctx.file(importer_path),
            diagnostics: self.diagnostics.file(importer_path),
            project_diagnostics: self.diagnostics,
        }
    }
}