use intmap::IntMap;
use roaring::RoaringTreemap;

/// Combination of [IntMap] and [RoaringTreemap] which is an int-map with fast key-set access.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct RoaringMap<V> {
    map: IntMap<V>,
    keys: RoaringTreemap,
}

impl<V> RoaringMap<V> {
    pub fn new() -> Self {
        Self { map: IntMap::new(), keys: RoaringTreemap::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { map: IntMap::with_capacity(capacity), keys: RoaringTreemap::new() }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.map.reserve(additional)
    }

    pub fn insert(&mut self, key: u64, value: V) -> Option<V> {
        self.keys.insert(key);
        self.map.insert(key, value)
    }

    pub fn get(&self, key: u64) -> Option<&V> {
        if self.keys.contains(key) {
            // No way to get_unchecked
            Some(unsafe { self.map.get(key).unwrap_unchecked() })
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, key: u64) -> Option<&mut V> {
        if self.keys.contains(key) {
            // No way to get_unchecked
            Some(unsafe { self.map.get_mut(key).unwrap_unchecked() })
        } else {
            None
        }
    }

    pub fn contains_key(&self, key: u64) -> bool {
        self.keys.contains(key)
    }

    pub fn remove(&mut self, key: u64) -> Option<V> {
        if self.keys.remove(key) {
            self.map.remove(key)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.map.clear();
        self.keys.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = (u64, &V)> + '_ {
        self.map.iter().map(|(&key, value)| (key, value))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (u64, &mut V)> + '_ {
        self.map.iter_mut().map(|(&key, value)| (key, value))
    }

    pub fn keys(&self) -> &RoaringTreemap {
        &self.keys
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> + '_ {
        self.map.values_mut()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<V> IntoIterator for RoaringMap<V> {
    type Item = (u64, V);
    type IntoIter = <IntMap<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<V> FromIterator<(u64, V)> for RoaringMap<V> {
    fn from_iter<T: IntoIterator<Item = (u64, V)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

impl<V> Extend<(u64, V)> for RoaringMap<V> {
    fn extend<T: IntoIterator<Item = (u64, V)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}