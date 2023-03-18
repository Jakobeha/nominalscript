use std::cell::{Cell, UnsafeCell};
use std::collections::HashMap;
use std::hash::Hash;
use elsa::FrozenMap;

/// Iterator for [FrozenMap], since it has no builtin iteration for some reason
pub struct FrozenMapIter<'a, K: 'a, V: 'a> {
    map: &'a _FrozenMap<K, V>,
    iter: std::collections::hash_map::Iter<'a, K, V>
}

/// Has the same repr as FrozenMap
struct _FrozenMap<K, V> {
    map: UnsafeCell<HashMap<K, V>>,
    /// Eq/Hash implementations can have side-effects, and using Rc it is possible
    /// for FrozenMap::insert to be called on a key that itself contains the same
    /// `FrozenMap`, whose `eq` implementation also calls FrozenMap::insert
    ///
    /// We use this `in_use` flag to guard against any reentrancy.
    in_use: Cell<bool>,
}

impl<'a, K: Eq + Hash, V> FrozenMapIter<'a, K, V> {
    pub fn new(map: &'a FrozenMap<K, V>) -> Self {
        let map: &'a _FrozenMap<K, V> = map.into();
        assert!(!map.in_use.get());
        map.in_use.set(true);
        // SAFETY: The map is only in use by us
        let map_ref = unsafe { &*(map.map.get() as *const HashMap<K, V>) };
        Self {
            map,
            iter: map_ref.iter()
        }
    }
}

impl<'a, K: 'a, V: 'a> Iterator for FrozenMapIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<'a, K, V> Drop for FrozenMapIter<'a, K, V> {
    fn drop(&mut self) {
        let map: &'a _FrozenMap<K, V> = self.map;
        assert!(map.in_use.get());
        map.in_use.set(false);
    }
}

impl<'a, K, V> From<&'a FrozenMap<K, V>> for &'a _FrozenMap<K, V> {
    fn from(map: &'a FrozenMap<K, V>) -> Self {
        // SAFETY: FrozenMap has the same fields as _FrozenMap
        //     (not repr(C) so "technically" not safe, but in practice ok.
        //      This is the best we can do because FrozenMap completely blocks the capability to
        //      iterate on a reference otherwise)
        unsafe { std::mem::transmute(map) }
    }
}