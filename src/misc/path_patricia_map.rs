//! A patricia tree map with [Path] keys, with the restriction that only UTF8 paths are allowed
//! except on unix and WASI.
use std::ffi::{OsStr, OsString};
#[cfg(unix)]
use std::os::unix::ffi::{OsStrExt, OsStringExt};
#[cfg(wasi)]
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::path::{Path, PathBuf};

use patricia_tree::{GenericPatriciaMap, PatriciaMap};
use patricia_tree::map::{Values, ValuesMut};

/// Patricia tree based map with [Path] as key.
///
/// On unix and WASI we can accept arbitrary paths, on other platforms we can't accept paths which
/// aren't valid UTF-8. For compatibility, both methods will enforce UTF8 for insert operations, but
/// but the unix and WASI versions also have `insert_arbitrary`.
#[derive(Debug, Clone)]
pub struct PathPatriciaMap<V>(PatriciaMap<V>);

/// An iterator for [PathPatriciaMap].
#[derive(Debug)]
pub struct IntoIter<V>(patricia_tree::map::IntoIter<Vec<u8>, V>);

/// Error encountered when attempting to insert a path which is not UTF-8, and not through
/// `insert_arbitrary`
#[derive(Debug)]
pub struct NonUtf8PathError<V> {
    /// The value which was attempted to be inserted.
    pub value: V
}

impl<V> PathPatriciaMap<V> {
    /// Makes a new empty `PathPatriciaMap` instance.
    #[inline]
    pub fn new() -> Self {
        Self(GenericPatriciaMap::new())
    }

    /// Clears this map, removing all values.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns the number of elements in this map.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if this map contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Inserts a key-value pair into this map.
    ///
    /// **Important:** Fails if the path isn't valid UTF-8.
    ///
    /// If the map did not have this key present, `None` is returned.
    /// If the map did have this key present, the value is updated, and the old value is returned.
    pub fn insert<Q: AsRef<Path>>(&mut self, key: Q, value: V) -> Result<Option<V>, NonUtf8PathError<V>> {
        match key.as_ref().to_str() {
            None => Err(NonUtf8PathError { value }),
            Some(s) => Ok(self.0.insert(s.as_bytes(), value)),
        }
    }
}

impl<V> Default for PathPatriciaMap<V> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(any(unix, wasi))]
impl<V> PathPatriciaMap<V> {
    /// Inserts a key-value pair into this map. Unlike `insert`, this *does not* require the key to
    /// be valid UTF-8.
    ///
    /// If the map did not have this key present, `None` is returned.
    /// If the map did have this key present, the value is updated, and the old value is returned.
    #[inline]
    pub fn insert_arbitrary<Q: AsRef<Path>>(&mut self, key: Q, value: V) -> Option<V> {
        self.0.insert(key.as_ref().as_os_str().as_bytes(), value)
    }

    /// Returns `true` if this map contains a value for the specified key.
    #[inline]
    pub fn contains_key<Q: AsRef<Path>>(&self, key: Q) -> bool {
        self.0.contains_key(key.as_ref().as_os_str().as_bytes())
    }

    /// Returns a reference to the value corresponding to the key.
    #[inline]
    pub fn get<Q: AsRef<Path>>(&self, key: Q) -> Option<&V> {
        self.0.get(key.as_ref().as_os_str().as_bytes())
    }

    /// Returns a mutable reference to the value corresponding to the key.
    #[inline]
    pub fn get_mut<Q: AsRef<Path>>(&mut self, key: Q) -> Option<&mut V> {
        self.0.get_mut(key.as_ref().as_os_str().as_bytes())
    }

    /// Finds the longest common prefix of `key` and the keys in this map,
    /// and returns a reference to the entry whose key matches the prefix.
    #[inline]
    pub fn get_longest_common_prefix<'a, Q>(&self, key: &'a Q) -> Option<(&'a Path, &V)>
        where
            Q: ?Sized + AsRef<Path>,
    {
        self.0.get_longest_common_prefix(key.as_ref().as_os_str().as_bytes())
            .map(|(k, v)| (Path::new(OsStr::from_bytes(k)), v))
    }

    /// Finds the longest common prefix of `key` and the keys in this map,
    /// and returns a reference to the entry whose key matches the prefix.
    #[inline]
    pub fn get_longest_common_prefix_mut<'a, Q>(&mut self, key: &'a Q) -> Option<(&'a Path, &mut V)>
        where
            Q: ?Sized + AsRef<Path>,
    {
        self.0.get_longest_common_prefix_mut(key.as_ref().as_os_str().as_bytes())
            .map(|(k, v)| (Path::new(OsStr::from_bytes(k)), v))
    }

    /// Removes a key from this map, returning the value at the key if the key was previously in it.
    #[inline]
    pub fn remove<Q: AsRef<Path>>(&mut self, key: Q) -> Option<V> {
        self.0.remove(key.as_ref().as_os_str().as_bytes())
    }

    /// Returns an iterator that collects all entries in the map up to a certain key.
    #[inline]
    pub fn common_prefixes<'a, 'b>(&'a self, key: &'b Path) -> impl Iterator<Item=(&'b Path, &'a V)>
        where
            'a: 'b,
    {
        self.0.common_prefixes(key.as_os_str().as_bytes())
            .map(|(k, v)| (Path::new(OsStr::from_bytes(k)), v))
    }

    /// Returns an iterator that collects all values of entries in the map up to a certain key.
    #[inline]
    pub fn common_prefix_values<'a>(&'a self, key: &Path) -> impl 'a + Iterator<Item = &'a V> {
        self.0.common_prefix_values(key.as_os_str().as_bytes())
    }

    /// Splits the map into two at the given prefix.
    ///
    /// The returned map contains all the entries of which keys are prefixed by `prefix`.
    #[inline]
    pub fn split_by_prefix<Q: AsRef<Path>>(&mut self, prefix: Q) -> Self {
        Self(self.0.split_by_prefix(prefix.as_ref().as_os_str().as_bytes()))
    }

    /// Gets an iterator over the entries of this map, sorted by key.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=(PathBuf, &V)> {
        self.0.iter().map(|(k, v)| {
            (PathBuf::from(OsString::from_vec(k)), v)
        })
    }

    /// Gets a mutable iterator over the entries of this map, soretd by key.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item=(PathBuf, &mut V)> {
        self.0.iter_mut().map(|(k, v)| {
            (PathBuf::from(OsString::from_vec(k)), v)
        })
    }

    /// Gets an iterator over the keys of this map, in sorted order.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item=PathBuf> + '_ {
        self.0.keys().map(|k| PathBuf::from(OsString::from_vec(k)))
    }

    /// Gets an iterator over the values of this map, in order by key.
    #[inline]
    pub fn values(&self) -> Values<V> {
        self.0.values()
    }

    /// Gets a mutable iterator over the values of this map, in order by key.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<V> {
        self.0.values_mut()
    }

    /// Gets an iterator over the entries having the given prefix of this map, sorted by key.
    #[inline]
    pub fn iter_prefix<'a, 'b>(
        &'a self,
        prefix: &'b Path,
    ) -> impl 'a + Iterator<Item = (PathBuf, &'a V)>
        where
            'b: 'a,
    {
        self.0.iter_prefix(prefix.as_os_str().as_bytes()).map(move |(k, v)| {
            (PathBuf::from(OsString::from_vec(k)), v)
        })
    }

    /// Gets a mutable iterator over the entries having the given prefix of this map, sorted by key.
    #[inline]
    pub fn iter_prefix_mut<'a, 'b>(
        &'a mut self,
        prefix: &'b Path,
    ) -> impl 'a + Iterator<Item = (PathBuf, &'a mut V)>
        where
            'b: 'a,
    {
        self.0.iter_prefix_mut(prefix.as_os_str().as_bytes()).map(move |(k, v)| {
            (PathBuf::from(OsString::from_vec(k)), v)
        })
    }
}

#[cfg(not(any(unix, wasi)))]
impl<V> PathPatriciaMap<V> {
    /// Returns `true` if this map contains a value for the specified key.
    #[inline]
    pub fn contains_key<Q: AsRef<Path>>(&self, key: Q) -> bool {
        match key.as_ref().to_str() {
            None => false,
            Some(s) => self.0.contains_key(s.as_bytes()),
        }
    }

    /// Returns a reference to the value corresponding to the key.
    #[inline]
    pub fn get<Q: AsRef<Path>>(&self, key: Q) -> Option<&V> {
        match key.as_ref().to_str() {
            None => None,
            Some(s) => self.0.get(s.as_bytes()),
        }
    }

    /// Returns a mutable reference to the value corresponding to the key.
    #[inline]
    pub fn get_mut<Q: AsRef<Path>>(&mut self, key: Q) -> Option<&mut V> {
        match key.as_ref().to_str() {
            None => None,
            Some(s) => self.0.get_mut(s.as_bytes()),
        }
    }

    /// Finds the longest common prefix of `key` and the keys in this map,
    /// and returns a reference to the entry whose key matches the prefix.
    #[inline]
    pub fn get_longest_common_prefix<'a, Q>(&self, key: &'a Q) -> Option<(&'a Path, &V)>
        where
            Q: ?Sized + AsRef<Path>,
    {
        match key.as_ref().to_str() {
            None => None,
            Some(s) => {
                self.0.get_longest_common_prefix(s.as_bytes()).map(|(k, v)| {
                    // SAFETY: Only stores valid UTF-8 paths
                    (Path::new(unsafe { str::from_utf8_unchecked(k) }), v)
                })
            },
        }
    }

    /// Removes a key from this map, returning the value at the key if the key was previously in it.
    #[inline]
    pub fn remove<Q: AsRef<Path>>(&mut self, key: Q) -> Option<V> {
        match key.as_ref().to_str() {
            None => None,
            Some(s) => self.0.remove(s.as_bytes()),
        }
    }

    /// Returns an iterator that collects all entries in the map up to a certain key.
    #[inline]
    pub fn common_prefixes<'a, 'b>(&'a self, key: &'b Path) -> impl Iterator<Item=(&'b Path, &'a V)>
        where
            'a: 'b,
    {
        key.to_str().into_iter().flat_map(move |s| {
            self.0.common_prefixes(s.as_bytes()).map(|(k, v)| {
                // SAFETY: Only stores valid UTF-8 paths
                (Path::new(unsafe { str::from_utf8_unchecked(k) }), v)
            })
        })
    }

    /// Returns an iterator that collects all values of entries in the map up to a certain key.
    #[inline]
    pub fn common_prefix_values<'a>(&'a self, key: &Path) -> impl 'a + Iterator<Item = &'a V> {
        key.to_str().into_iter().flat_map(move |s| {
            self.0.common_prefix_values(s.as_bytes())
        })
    }

    /// Splits the map into two at the given prefix.
    ///
    /// The returned map contains all the entries of which keys are prefixed by `prefix`.
    #[inline]
    pub fn split_by_prefix<Q: AsRef<Path>>(&mut self, prefix: Q) -> Self {
        match prefix.as_ref().to_str() {
            None => Self::new(),
            Some(s) => Self(self.0.split_by_prefix(s.as_bytes())),
        }
    }

    /// Gets an iterator over the entries of this map, sorted by key.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=(PathBuf, &V)> {
        self.0.iter().map(|(k, v)| {
            // SAFETY: Only stores valid UTF-8 paths.
            (PathBuf::from(unsafe { String::from_utf8_unchecked(k) }), v)
        })
    }

    /// Gets a mutable iterator over the entries of this map, soretd by key.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item=(PathBuf, &mut V)> {
        self.0.iter_mut().map(|(k, v)| {
            // SAFETY: Only stores valid UTF-8 paths.
            (PathBuf::from(unsafe { String::from_utf8_unchecked(k) }), v)
        })
    }

    /// Gets an iterator over the keys of this map, in sorted order.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item=PathBuf> {
        self.0.keys().map(|k| {
            // SAFETY: Only stores valid UTF-8 paths.
            PathBuf::from(unsafe { String::from_utf8_unchecked(k) })
        })
    }

    /// Gets an iterator over the values of this map, in order by key.
    #[inline]
    pub fn values(&self) -> Values<V> {
        self.0.values()
    }

    /// Gets a mutable iterator over the values of this map, in order by key.
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<V> {
        self.0.values_mut()
    }

    /// Gets an iterator over the entries having the given prefix of this map, sorted by key.
    #[inline]
    pub fn iter_prefix<'a, 'b>(
        &'a self,
        prefix: &'b Path,
    ) -> impl 'a + Iterator<Item = (PathBuf, &'a V)>
        where
            'b: 'a,
    {
        prefix.to_str().into_iter().flat_map(move |s| {
            self.0.iter_prefix(s.as_bytes()).map(|(k, v)| {
                // SAFETY: Only stores valid UTF-8 paths.
                (PathBuf::from(unsafe { String::from_utf8_unchecked(k) }), v)
            })
        })
    }

    /// Gets a mutable iterator over the entries having the given prefix of this map, sorted by key.
    #[inline]
    pub fn iter_prefix_mut<'a, 'b>(
        &'a mut self,
        prefix: &'b Path,
    ) -> impl 'a + Iterator<Item = (PathBuf, &'a mut V)>
        where
            'b: 'a,
    {
        prefix.to_str().into_iter().flat_map(move |s| {
            self.0.iter_prefix_mut(s.as_bytes()).map(|(k, v)| {
                // SAFETY: Only stores valid UTF-8 paths.
                (PathBuf::from(unsafe { String::from_utf8_unchecked(k) }), v)
            })
        })
    }
}

impl<V> IntoIterator for PathPatriciaMap<V> {
    type Item = (PathBuf, V);
    type IntoIter = IntoIter<V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter())
    }
}

impl<V> Iterator for IntoIter<V> {
    type Item = (PathBuf, V);

    #[cfg(any(unix, wasi))]
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| {
            (PathBuf::from(OsString::from_vec(k)), v)
        })
    }

    #[cfg(not(any(unix, wasi)))]
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| {
            // SAFETY: Only stores valid UTF-8 paths.
            (PathBuf::from(unsafe { String::from_utf8_unchecked(k) }), v)
        })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
