use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use crate::import_export::export::{ImportPath, Module};
use crate::import_export::import_ctx::ImportError;
use crate::import_export::import_resolver::{ResolvedFatPath, ResolveFailure};
use crate::import_export::module_id_map::ModuleIdMap;
use crate::import_export::ModuleId;

/// Caches imports
#[derive(Debug)]
pub(crate) struct ImportCache {
    next_module_id: Cell<ModuleId>,
    module_to_id: RefCell<HashMap<PathBuf, HashMap<ImportPath, ModuleId>>>,
    id_to_fat_path: ModuleIdMap<ResolvedFatPath>,
    fat_path_to_transpile_out: ModuleIdMap<Result<Module, ImportError>>
}


impl ImportCache {
    pub(crate) fn new() -> Self {
        Self {
            next_module_id: Cell::new(ModuleId(0)),
            module_to_id: RefCell::new(HashMap::new()),
            id_to_fat_path: ModuleIdMap::new(),
            fat_path_to_transpile_out: ModuleIdMap::new()
        }
    }

    fn next_module_id(&self) -> ModuleId {
        let id = self.next_module_id.get();
        self.next_module_id.set(ModuleId(id.0 + 1));
        id
    }

    pub(super) fn cache_resolve_module(
        &self,
        (importer_path, module_path): (&Path, &ImportPath),
        resolve: impl FnOnce() -> Result<ResolvedFatPath, ResolveFailure>
    ) -> Ref<'_, ResolvedFatPath> {
        if let Some(fat_path) = Some(self.module_to_fat_path.borrow())
            .and_then(|x| Ref::filter_map(x, |x| x.get(importer_path)).ok())
            .and_then(|m| Ref::filter_map(m, |m| m.get(module_path)).ok()) {
            return fat_path
        }
        let fat_path = resolve().unwrap_or_default();
        self.module_to_fat_path.borrow_mut()
            .entry(importer_path.to_path_buf()).or_default()
            .insert(module_path.clone(), fat_path);
        Ref::map(self.module_to_fat_path.borrow(), |x| {
            x.get(importer_path).unwrap().get(module_path).unwrap()
        })
    }

    /// If the path has already been partially or fully transpiled, returns the cached result.
    /// Otherwis calls `transpile`.
    ///
    /// *Panics* if `fat_path` is null
    pub(super) fn cache_transpile(
        &self,
        fat_path: Ref<'_, ResolvedFatPath>,
        transpile: impl FnOnce() -> Result<Module, ImportError>
    ) -> Result<Rc<Module>, Rc<ImportError>> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Some(transpile_result) = Ref::filter_map(
            self.fat_path_to_transpile_out.borrow(),
            |x| x.get(&*fat_path)
        ).ok() {
            return transpile_result.clone()
        }
        // Drop in case we affect module_to_fat_path in transpile
        // (we don't, but if we change it will be annoying to debug)
        drop(fat_path);
        let transpiled = transpile().map(Rc::new).map_err(Rc::new);
        let None = self.fat_path_to_transpile_out.borrow_mut().insert(fat_path.clone(), transpiled.clone()) else {
            unreachable!("we just checked that this entry doesn't exist")
        };
        transpiled
    }

    /// Same as [cache_transpile] but takes an owned `fat_path` instead of a [Ref].
    pub(super) fn cache_transpile2(
        &self,
        fat_path: ResolvedFatPath,
        transpile: impl FnOnce() -> Result<Module, ImportError>
    ) -> Result<Rc<Module>, Rc<ImportError>> {
        assert!(!fat_path.is_null(), "can't cache-transpile null fat path");
        if let Some(transpile_result) = Ref::filter_map(
            self.fat_path_to_transpile_out.borrow(),
            |x| x.get(&fat_path)
        ).ok() {
            return transpile_result.clone()
        }
        let transpiled = transpile().map(Rc::new).map_err(Rc::new);
        let None = self.fat_path_to_transpile_out.borrow_mut().insert(fat_path, transpiled.clone()) else {
            unreachable!("we just checked that this entry doesn't exist")
        };
        transpiled
    }
}
