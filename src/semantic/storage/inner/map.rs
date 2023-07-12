use std::borrow::Borrow;
use std::fmt::Display;

use derivative::Derivative;

use btree_plus_store::{BTreeMap, BTreeStore};

use crate::semantic::storage::{HasStore, Id};
use crate::semantic::storage::ann::Ann;
use crate::semantic::storage::inner::InnerSet;

/// Map of semantic names to nodes inside another semantic node; an [InnerSet] where each node is
/// uniqued by its name.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct InnerMap<'tree, K: ?Sized, V> {
    /// Underlying semantic set of nodes
    set: InnerSet<'tree, V>,
    /// Map of names to nodes
    by_name: BTreeMap<'tree, &'tree K, Id<'tree, V>>,
}

/// Error when a node is inserted into an [InnerMap] with a name that already exists
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct DuplicateNode<'tree, K: ?Sized, V> {
    pub name: &'tree K,
    pub old: V,
}

impl<'tree, K: ?Sized, V> InnerMap<'tree, K, V> {
    /// Create a new, empty map
    #[inline]
    pub fn new_in(store: impl HasStore<'tree, BTreeStore<V, ()>> + HasStore<'tree, BTreeStore<&'tree K, V>>) -> Self {
        Self {
            set: InnerSet::new_in(store),
            by_name: BTreeMap::new_in(store.inner_store()),
        }
    }

    /// Check if we have the node
    #[inline]
    pub fn contains(&self, node: &V) -> bool {
        self.set.contains(node)
    }

    /// Check if we have the name
    #[inline]
    pub fn contains_name<Q: Ord + ?Sized>(&self, name: &Q) -> bool where K: Borrow<Q> {
        self.by_name.contains_key(name)
    }

    /// Insert a node into the map. *Warns* if the node was already in the map. If a different node
    /// with the same name is present, replaces and returns an error with that node.
    #[inline]
    pub fn insert(&mut self, name: &'tree K, node: V) -> Result<(), DuplicateNode<K, V>> where V: Copy {
        self.set.insert(node);
        match self.by_name.insert(name, node) {
            Some(old) if old != node => Err(DuplicateNode { name, old }),
            _ => Ok(()),
        }
    }

    /// Remove the node with the name in the map. *Warns* if the name was not in the map.
    #[inline]
    pub fn remove<Q: Display + Ord + ?Sized>(&mut self, name: &Q) {
        if let Some(node) = self.by_name.remove(name) {
            self.set.remove(&node);
        } else {
            log::warn!("tried to remove non-existent name '{}'", name)
        }
    }

    /// Get a reference to the node at the given annotation, or `None` if it doesn't exist
    #[inline]
    pub fn by_ann(&self, ann: &Ann<'tree>) -> Option<&V> where V: Borrow<Ann<'tree>> {
        self.set.by_ann(ann)
    }

    /// Get a reference to the node with the given name, or `None` if it doesn't exist
    #[inline]
    pub fn by_name<Q: Ord + ?Sized>(&self, name: &Q) -> Option<&V> where K: Borrow<Q> {
        self.by_name.get(name)
    }

    /// Iterate over the nodes in the map
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=&V> {
        self.set.iter()
    }
}

impl<'a, 'tree: 'a, K, V> IntoIterator for &'a InnerMap<'tree, K, V> {
    type Item = &'a V;
    type IntoIter = btree_plus_store::set::Iter<'a, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.set.iter()
    }
}