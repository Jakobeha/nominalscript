use std::borrow::Borrow;
use std::fmt::Debug;

use derivative::Derivative;

use btree_plus_store::{BTreeSet, BTreeStore};

use crate::semantic::storage::HasStore;
use crate::semantic::storage::ann::Ann;

/// Set of semantic nodes inside another semantic node. The set only contains the identifiers, the
/// nodes' data is stored in the [RootSet]; furthermore, the set also allocates memory in an
/// external, root store.
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct InnerSet<'tree, T> {
    /// Underlying set with an external store
    inner: BTreeSet<'tree, T>,
}

impl<'tree, T> InnerSet<'tree, T> {
    /// Create a new, empty set
    #[inline]
    pub fn new_in(store: impl HasStore<'tree, BTreeStore<T, ()>>) -> Self {
        Self {
            inner: BTreeSet::new_in(store.inner_store()),
        }
    }

    /// Check if we have the node
    #[inline]
    pub fn contains(&self, node: &T) -> bool {
        self.inner.contains(node)
    }

    /// Insert a node into the set. *Warns* if the node was already in the set.
    #[inline]
    pub fn insert(&mut self, node: T) where T: Debug {
        if !self.inner.insert(node) {
            log::warn!("inserted node {:?} into set twice", node);
        }
    }

    /// Remove a node from the set. *Warns* if the node was not in the set.
    #[inline]
    pub fn remove(&mut self, node: &T) where T: Debug {
        if !self.inner.remove(node) {
            log::warn!("removed node {:?} from set twice", node);
        }
    }

    /// Get a reference to the node at the given annotation, or `None` if it doesn't exist
    #[inline]
    pub fn by_ann(&self, ann: &Ann<'tree>) -> Option<&T> where T: Borrow<Ann<'tree>> {
        self.inner.get(ann)
    }

    /// Iterate over the nodes in the set
    #[inline]
    pub fn iter(&self) -> btree_plus_store::set::Iter<'_, T> {
        self.inner.iter()
    }
}

impl<'a, 'tree: 'a, T> IntoIterator for &'a InnerSet<'tree, T> {
    type Item = &'a T;
    type IntoIter = btree_plus_store::set::Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}