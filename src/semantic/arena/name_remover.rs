use std::collections::HashSet;

use type_sitter_lib::tree_sitter_wrapper::Node;

use crate::semantic::ann::HasAnn;
use crate::semantic::arena::{AnnArena, AnnRemover, HasName, NameArena};

/// An [AnnRemover] which corresponds to a [NameArena]. However, we don't actually care about the
/// names when using this.
pub struct NameRemover<'tree, T: HasName<'tree>> {
    /// Underlying remover
    base: AnnRemover<'tree, T>
}

impl<'tree, T: HasName<'tree>> NameRemover<'tree, T> {
    /// Create a [NameRemover] from a [NameArena]'s inner data
    pub(super) fn new(mut base: AnnRemover<T>) -> Self {
        // Remove later elements with the same name as earlier ones
        let mut seen_names = HashSet::new();
        base.retain(|elem| !seen_names.insert(elem.name()));
        Self { base }
    }

    /// Remove all elements which would be affected by the given node; that is, elements which are
    /// in the given node's range (including start point but excluding end point).
    #[inline]
    pub fn remove_in_node(&mut self, node: Node<'tree>) {
        self.base.remove_in_node(node)
    }

    /// Converts this back into an [AnnArena], so we can view and add elements again.
    pub fn into_arena(self) -> NameArena<T> {
        let mut arena = NameArena::<T>::new();
        arena.alloc_extend(self.base.into_ordered_elems().filter_map(|e| e));
        arena
    }
}