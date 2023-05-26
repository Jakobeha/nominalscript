use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;
use std::mem::MaybeUninit;
use std::ops::RangeBounds;

use once_cell::unsync::OnceCell;
use type_sitter_lib::tree_sitter_wrapper::Node;

use crate::{error, issue};
use crate::diagnostics::ProjectLogger;
use crate::semantic::ann::Point;
use crate::semantic::arena::{AnnArena, AnnArenaPhase, HasName};

/// An [AnnArena] which also has fast lookup and removal of values by their name (only in the proper
/// phases), and logs *diagnostics* against insertion of multiple elements with the same name when
/// converting into the "retrieval" phase (unlike with same annotation, this *can* happen and is a
/// user error. Multiple elements with the same name are only considered if they have different
/// annotations)
pub struct NameArena<'tree, T: HasName<'tree>> {
    /// Base arena
    base: AnnArena<'tree, T>,
    /// Inserts elements by name when converting into the [AnnArentPhase::Retrieval] phase, allows
    /// users to access the pointers while in the [AnnArenaPhase::Retrieval] phase, and then remove
    /// the indices while in the [AnnArenaPhase::Removal] phase.
    by_name: OnceCell<HashMap<&'tree T::Name, (usize, *const T)>>
}

impl<'tree, T: HasName<'tree>> NameArena<'tree, T> {
    /// Create an empty [NameArena] in the [AnnArenaPhase::Insertion] phase.
    pub fn new() -> Self {
        NameArena {
            base: AnnArena::new(),
            by_name: OnceCell::new()
        }
    }

    /// Get the current phase. **Panics** if in the [AnnArenaPhase::Broken] phase.
    #[inline]
    pub fn phase(&self) -> AnnArenaPhase {
        self.base.phase()
    }

    /// Returns whether [AnnArena] is in the "broken" phase, which only happens if there's a caught
    /// panic when changing into the [AnnArenaPhase::Removal] phase.
    ///
    /// The "broken" phase isn't part of [AnnArenaPhase], because most users won't care about it and
    /// will simply want to panic if it happens.
    #[inline]
    pub fn is_broken(&self) -> bool {
        self.base.is_broken()
    }

    // region insertion/retrieval/removal conversions
    /// Converts from [AnnArenaPhase::Insertion] to [AnnArenaPhase::Retrieval].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    pub fn conv_retrieval(&self, e: &ProjectLogger<'_>) {
        self.base.conv_retrieval();
        // Create the map of elements by name
        let mut by_name = self.base.iter()
            .enumerate()
            .map(|(index, elem)| (elem.name(), (index, elem as *const T)))
            .collect::<HashMap<_, _>>();
        // Log elements with the same name
        for (index, elem) in self.base.iter().enumerate() {
            let name = elem.name();
            if let Some(&(other_index, other)) = by_name.get(name) {
                let other = unsafe { &*other };
                // Indices here should be in the same order as annotations
                debug_assert_eq!(index.cmp(&other_index), elem.ann().cmp(&other.ann()));
                // Comparing indices is faster
                match index.cmp(&other_index) {
                    Ordering::Less => {
                        error!(e, "Name already defined: `{}`", name => other;
                            issue!("First defined here" => elem));
                    },
                    Ordering::Equal => {},
                    Ordering::Greater => {
                        error!(e, "Name already defined: `{}`", name => elem;
                            issue!("First defined here" => other));
                    }
                }
            }
        }
        // Set by_name
        let Ok(()) = self.by_name.set(by_name) else {
            unreachable!("base conv_retrieval worked, so this should be in insertion phase and therefore by_name should be unset")
        };
    }

    /// Converts from [AnnArenaPhase::Retrieval] to [AnnArenaPhase::Removal].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Retrieval] phase.
    pub fn conv_removal(&mut self) {
        self.base.conv_removal();
        if self.by_name.get().is_none() {
            unreachable!("base conv_removal worked, so this should be in retrieval phase and therefore by_name should be set")
        }
    }

    /// Converts from [AnnArenaPhase::Removal] to [AnnArenaPhase::Insertion].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn conv_insertion(&mut self) {
        self.base.conv_insertion();
        // Unset by_name
        // ???: like with ann_arena, preserve names we already know so we don't have to recompute?
        let Some(_) = self.by_name.take() else {
            unreachable!("base conv_insertion worked, so this should be in removal phase and therefore by_name should be set")
        };
    }
    // endregion

    // region new retrieval methods
    /// Checks whether an element with the name is in the arena.
    pub fn has_name(&self, name: &T::Name) -> bool {
        let by_name = self.by_name_retrieval("has_name");
        by_name.contains_key(name)
    }


    /// Gets an element by its name, or `None` if it is not in the arena.
    pub fn with_name(&self, name: &T::Name) -> Option<&T> {
        let by_name = self.by_name_retrieval("with_name");
        // SAFETY: Pointer points to data in `self` so it's alive
        by_name.get(name).map(|&(_, elem)| unsafe { &*elem })
    }
    // endregion

    // region new removal methods
    /// Removes the element with the name from the arena, if present. Does nothing otherwise.
    pub fn remove_name(&mut self, name: &T::Name) -> bool {
        let by_name = self.by_name_removal("remove_name");
        if let Some(&(index, _)) = by_name.get(name) {
            self.base.remove(index);
            true
        } else {
            false
        }
    }
    // endregion

    // region retrieval/removal by_name visitors
    fn by_name_retrieval(&self, fn_name: &'static str) -> &HashMap<T::Name, (usize, *const T)> {
        assert_eq!(self.base.phase(), AnnArenaPhase::Retrieval, "NameArena::{} not called in retrieval phase", fn_name);
        self.by_name.get().unwrap()
    }

    fn by_name_removal(&mut self, fn_name: &'static str) -> &mut HashMap<T::Name, (usize, *const T)> {
        assert_eq!(self.base.phase(), AnnArenaPhase::Removal, "NameArena::{} not called in removal phase", fn_name);
        self.by_name.get_mut().unwrap()
    }
    // endregion

    // region forwarded insertion methods
    /// Insert an element and return its reference.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning
    /// and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    #[inline]
    pub fn alloc(&self, elem: T) -> &T {
        self.base.alloc(elem)
    }

    /// Insert multiple elements and return a reference to the slice.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning
    /// and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    #[inline]
    pub fn alloc_extend(&self, elems: impl Iterator<Item=T>) -> &[T] {
        self.base.alloc_extend(elems)
    }

    /// Makes sure there's enough continuous space for at least `additional` elements.
    ///
    /// This may save some work if called before [Self::alloc_extend]. On the other hand this might
    /// waste up to `n - 1` elements of space. In case new allocation is needed, the unused ones in
    /// current chunk are never used.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning
    /// and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    #[inline]
    pub fn reserve_extend(&self, additional: usize) {
        self.base.reserve_extend(additional)
    }

    /// Insert an element without initializing it and return its reference.
    ///
    /// Later, if an element with the same annotation has already been inserted, it will log a
    /// warning and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    ///
    /// ## Safety
    ///
    /// After calling this method, the arena considers the elements initialized. If you fail to
    /// initialize them (which includes because of panicking during the initialization), the arena
    /// will run destructors on the uninitialized memory. Therefore, you must initialize them.
    ///
    /// Considering how easy it is to cause undefined behaviour using this, you're advised to prefer
    /// the other (safe) methods, like [Self::alloc_extend].
    #[inline]
    pub unsafe fn alloc_uninitialized(&self, additional: usize) -> &mut [MaybeUninit<T>] {
        self.base.alloc_uninitialized(additional)
    }

    /// Returns unused space.
    ///
    /// This unused space is still not considered "allocated". Therefore, it won't be dropped unless
    /// there are further calls to [Self::alloc], [Self::alloc_uninitialized], or
    /// [Self::alloc_extend] which is why the method is safe.
    ///
    /// It returns a raw pointer to avoid creating multiple mutable references to the same place. It
    /// is up to the caller not to dereference it after any of the alloc_ methods are called.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    #[inline]
    pub fn uninitialized_array(&self) -> *mut [MaybeUninit<T>] {
        self.base.uninitialized_array()
    }
    // endregion

    // region forwarded retrieval methods
    /// Gets the number of elements
    #[inline]
    pub fn len(&self) -> usize {
        self.base.len()
    }

    /// Iterates the elements in order of their annotations
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.base.iter()
    }

    /// Iterate all elements at the given point.
    #[inline]
    fn at_point(&self, point: Point<'tree>) -> impl Iterator<Item=&T> {
        self.base.at_point(point)
    }

    /// Iterate all elements within the given point range.
    #[inline]
    pub fn in_range(&self, range: impl RangeBounds<Point<'tree>>) -> impl Iterator<Item=&T> {
        self.base.in_range(range)
    }

    pub fn in_node(&self, node: Node<'tree>) -> impl Iterator<Item=&T> {
        self.base.in_node(node)
    }
    // endregion

    // region forwarded removal methods
    /// Remove elements which don't satisfy the predicate.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    #[inline]
    pub fn retain(&mut self, predicate: impl FnMut(&T) -> bool) {
        self.base.retain(predicate)
    }

    /// Remove all elements at the given point. Does nothing to elements already removed.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    #[inline]
    pub fn remove_at_point(&mut self, point: Point<'tree>) {
        self.base.remove_at_point(point)
    }

    /// Remove all elements within the given point range. Does nothing to elements already removed.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    #[inline]
    fn remove_in_range(&mut self, range: impl RangeBounds<Point<'tree>>) {
        self.base.remove_in_range(range)
    }

    /// Remove all elements which would be affected by the given node; that is, elements which are
    /// in the given node's range (including start point but excluding end point).
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    #[inline]
    pub fn remove_in_node(&mut self, node: Node<'tree>) {
        self.base.remove_in_node(node)
    }
    // endregion
}