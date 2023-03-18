use elsa::FrozenVec;
use stable_deref_trait::StableDeref;
use crate::import_export::import_cache::ModuleId;
use crate::import_export::ModuleId;

/// Wrapper for [FrozenVec] for maps with module id keys
pub struct ModuleIdMap<V: StableDeref> {
    backing: FrozenVec<V>
}

impl<V: StableDeref> ModuleIdMap<V> {
    pub fn new() -> Self {
        Self {
            backing: FrozenVec::new()
        }
    }

    pub fn push(&self, key: ModuleId, value: V) {
        self.backing.push(value)
    }

    pub fn push_get(&self, key: ModuleId, value: V) -> &V {
        self.backing.push_get(key, value)
    }

    pub fn get(&self, key: &ModuleId) -> Option<&V> {
        self.backing.get(key.0)
    }

    pub fn get_mut(&self, key: &ModuleId) -> Option<&mut V> {
        self.backing.get_mut(key.0)
    }
}