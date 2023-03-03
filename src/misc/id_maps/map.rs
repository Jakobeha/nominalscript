use std::marker::PhantomData;
use std::mem::transmute;
use crate::misc::id_maps::{Id, IdSet, RoaringMap};

/// `HashMap<L, R>`, but faster
#[derive(Debug, Default, Clone, PartialEq)]
#[repr(transparent)]
pub struct IdMap<K: Id, V>(RoaringMap<V>, PhantomData<K>);

impl<K: Id, V> IdMap<K, V> {
    pub fn new() -> Self {
        IdMap(RoaringMap::new(), PhantomData)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        IdMap(RoaringMap::with_capacity(capacity), PhantomData)
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.0.insert(key.into(), value)
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.0.get(key.into())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.0.get_mut(key.into())
    }

    pub fn contains_key(&self, key: K) -> bool {
        self.0.contains_key(key.into())
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        self.0.remove(key.into())
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> + '_ {
        self.0.iter().map(|(k, v)| (K::from(k), v))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> + '_ {
        self.0.iter_mut().map(|(k, v)| (K::from(k), v))
    }

    pub fn keys(&self) -> &IdSet<K> {
        // SAFETY: transparent repr
        unsafe { transmute(self.0.keys()) }
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.0.values()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> + '_ {
        self.0.values_mut()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<K: Id, V> FromIterator<(K, V)> for IdMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

impl<K: Id, V> Extend<(K, V)> for IdMap<K, V> {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}