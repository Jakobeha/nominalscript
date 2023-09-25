use std::borrow::Borrow;
use std::fmt::Debug;

use btree_plus_store::{BTreeMap, BTreeStore};

use crate::storage::HasStore;
use crate::storage::ann::Ann;

/// Ordered set of semantic nodes inside another semantic node. The set only contains the
/// identifiers, the nodes' data is stored in the [RootSet]; furthermore, the set also allocates
/// memory in an external, root store. See [InnerSet] for more, the only difference is that this is
/// ordered, and it still can't contain duplicates.
#[derive(Debug)]
pub struct InnerVec<'tree, T> {
    /// Unordered set, for fast `contains` and `position`
    set: BTreeMap<'tree, T, usize>,
    /// Ordered map, for fast iteration and indexed lookup
    order: BTreeMap<'tree, usize, T>,
}

impl<'tree, T> InnerVec<'tree, T> {
    /// Create a new, empty set
    #[inline]
    pub fn new_in(store: &(impl HasStore<'tree, BTreeStore<usize, T>> + HasStore<'tree, BTreeStore<T, usize>>)) -> Self {
        Self {
            set: BTreeMap::new_in(store.inner_store()),
            order: BTreeMap::new_in(store.inner_store()),
        }
    }

    /// Check if we have the node
    #[inline]
    pub fn contains(&self, node: &T) -> bool where T: Ord {
        self.set.contains_key(node)
    }

    /// Get the index of the node, if we have it
    #[inline]
    pub fn position(&self, node: &T) -> Option<usize> where T: Ord {
        self.set.get(node).copied()
    }

    /// Insert a node at the end. *Warns* if the node was already in the set.
    #[inline]
    pub fn push(&mut self, node: T) where T: Debug + Clone + Ord {
        debug_assert_eq!(self.set.len(), self.order.len(), "InnerVec broken invariant: set and order have different lengths");
        let index = self.set.len();
        if self.set.insert(node.clone(), index).is_none() {
            let None = self.order.insert(index, node) else {
                unreachable!("InnerVec broken invariant: inserted index {} twice", index);
            };
        } else {
            log::warn!("inserted node {:?} into InnerVec (an ordered set) twice", node);
        }
    }

    /// Insert a node at the given index. *Warns* if the node was already in the set. *Panics* if
    /// the index is out of bounds.
    #[inline]
    pub fn insert(&mut self, node: T, index: usize) where T: Debug + Clone + Ord {
        assert!(index <= self.set.len(), "index out of bounds");
        if self.set.contains_key(&node) {
            log::warn!("inserted node {:?} into InnerVec (an ordered set) twice", node);
        } else {
            self.increment_indices_at_and_after(index);
            let None = self.set.insert(node.clone(), index) else {
                unreachable!()
            };
            let None = self.order.insert(index, node) else {
                unreachable!("InnerVec broken invariant: inserted index {} twice", index);
            };
        }
    }

    /// Remove a node from the set. *Warns* if the node was not in the set.
    #[inline]
    pub fn remove(&mut self, node: &T) where T: Debug + Clone + Ord {
        if let Some(index) = self.set.remove(node) {
            let Some(node2) = self.order.remove(&index) else {
                unreachable!("InnerVec broken invariant: T->usize has an entry with no corresponding usize->T");
            };
            debug_assert_eq!(node, &node2, "InnerVec broken invariant: T->usize and usize->T are out of sync");
            self.decrement_indices_after(index);
        } else {
            log::warn!("removed node {:?} from InnerVec (an ordered set) twice", node);
        }
    }

    /// Remove and return the node from the set at the specified index. *Panics* if the index is out
    /// of bounds
    #[inline]
    pub fn remove_at(&mut self, index: usize) -> T where T: Debug + Clone + Ord {
        assert!(index < self.order.len(), "index out of bounds");
        let Some(node) = self.order.remove(&index) else {
            unreachable!("InnerVec broken invariant: missing index {}", index);
        };
        let Some(index2) = self.set.remove(&node) else {
            unreachable!("InnerVec broken invariant: usize->T has an entry with no corresponding T->usize");
        };
        debug_assert_eq!(index, index2, "InnerVec broken invariant: usize->T and T->usize are out of sync");
        self.decrement_indices_after(index);
        node
    }

    /// Get the index and a reference to the node at the given annotation, or `None` if the node
    /// doesn't exist
    #[inline]
    pub fn by_ann(&self, ann: &Ann<'tree>) -> Option<(usize, &T)> where T: Borrow<Ann<'tree>> {
        self.set.get_key_value(ann).map(|(node, index)| (*index, node))
    }

    /// Iterate over the nodes in the set, in their explicit order
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.order.values()
    }

    /// Increment the indices after `index`. This is called before inserting at `index` to keep the
    /// indices contiguous and in sync with the order.
    #[inline]
    fn increment_indices_at_and_after(&mut self, index: usize) where T: Ord {
        for i in index..self.order.len() {
            let Some(node) = self.order.remove(&i) else {
                unreachable!("InnerVec broken invariant: missing subsequent index to increment {}", i);
            };
            let Some(i2) = self.set.get_mut(&node) else {
                unreachable!("InnerVec broken invariant: T->usize has an entry with no corresponding usize->T (for subsequent index to increment {})", i);
            };
            self.order.insert(i + 1, node);
            debug_assert_eq!(i, *i2, "InnerVec broken invariant: T->usize and usize->T are out of sync (for subsequent index to increment {})", i);
            *i2 = i + 1;
        }
    }
    /// Decrement the indices after `index`. This is called after removing at `index` to keep the
    /// indices contiguous and in sync with the order.
    #[inline]
    fn decrement_indices_after(&mut self, index: usize) where T: Ord {
        for i in index..self.order.len() {
            let Some(node) = self.order.remove(&(i + 1)) else {
                unreachable!("InnerVec broken invariant: missing subsequent index to decrement {}", i);
            };
            let Some(i2) = self.set.get_mut(&node) else {
                unreachable!("InnerVec broken invariant: T->usize has an entry with no corresponding usize->T (for subsequent index to decrement {})", i);
            };
            self.order.insert(i, node);
            debug_assert_eq!(i + 1, *i2, "InnerVec broken invariant: T->usize and usize->T are out of sync (for subsequent index to decrement {})", i);
            *i2 = i;
        }
    }
}

impl<'a, 'tree: 'a, T> IntoIterator for &'a InnerVec<'tree, T> {
    type Item = &'a T;
    type IntoIter = btree_plus_store::map::Values<'a, usize, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.order.values()
    }
}