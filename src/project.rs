use std::path::{Path, PathBuf};
use crate::analyses::types::{FileResolveCache, ProjectResolveCache};
use crate::diagnostics::{FileDiagnostics, ProjectDiagnostics};
use crate::import_export::import_ctx::{FileImportCtx, ImportCache, ProjectImportCtx};
use crate::import_export::import_resolver::{ImportResolver, ImportResolverCreateError};

/// Project datastructure which contains everything.
///
/// In reality you will use a [ProjectCtx] which contains references to this data,
/// since you will be passing it through `FnOnce` in import resolution.
#[derive(Debug)]
pub struct Project {
    /// Caches imports, does change when calling transpile
    pub import_cache: ImportCache,
    /// Stateless relative to program (doesn't change when calling transpile)
    pub import_resolver: ImportResolver,
    pub diagnostics: ProjectDiagnostics,
    pub resolve_cache: ProjectResolveCache
}

/// Project environment = reference to project data
#[derive(Debug)]
pub struct ProjectCtx<'a> {
    pub import_ctx: ProjectImportCtx<'a>,
    pub diagnostics: &'a mut ProjectDiagnostics,
    pub resolve_cache: &'a mut ProjectResolveCache
}

/// File environment = reference to file data
#[derive(Debug)]
pub struct FileCtx<'a> {
    pub import_ctx: FileImportCtx<'a>,
    pub diagnostics: &'a mut FileDiagnostics,
    pub resolve_cache: &'a mut FileResolveCache
}

impl Project {
    pub fn regular(module_path: PathBuf) -> Result<Self, ImportResolverCreateError> {
        Ok(Self::new(ImportResolver::regular(module_path)?))
    }

    pub fn new(import_resolver: ImportResolver) -> Self {
        Self {
            import_cache: ImportCache::new(),
            import_resolver,
            diagnostics: ProjectDiagnostics::new(),
            resolve_cache: ProjectResolveCache::new()
        }
    }

    pub fn ctx(&mut self) -> ProjectCtx<'_> {
        ProjectCtx {
            import_ctx: ProjectImportCtx::new(&mut self.import_cache, &self.import_resolver),
            diagnostics: &mut self.diagnostics,
            resolve_cache: &mut self.resolve_cache
        }
    }
}

impl<'a> ProjectCtx<'a> {
    pub fn file(&mut self, importer_path: &Path) -> FileCtx<'a> {
        FileCtx {
            import_ctx: self.import_ctx.file(importer_path),
            diagnostics: &mut self.diagnostics.file(importer_path),
            resolve_cache: &mut self.resolve_cache.file(importer_path)
        }
    }
}