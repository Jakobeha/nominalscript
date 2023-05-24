use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;

use crate::semantic::ann::Ann;
use crate::semantic::arena::{AnnArena, HasName, NameRemover};

/// An [AnnArena] which also has fast lookup of values by their name, and warns against insertion of
/// multiple elements with the same name (like with same annotation, technically this should never
/// happen, but handling is easy so it's considered a soft error).
pub struct NameArena<'tree, T: HasName<'tree>> {
    base: AnnArena<'tree, T>,
    by_name: RefCell<HashMap<&'tree T::Name, *const T>>
}

impl<'tree, T: HasName<'tree>> NameArena<'tree, T> {
    /// Create an empty [AssocArena]
    pub fn new() -> Self {
        NameArena {
            base: AnnArena::new(),
            by_name: RefCell::new(HashMap::new())
        }
    }

    /// Insert an element and return its reference.
    ///
    /// If an element with the same name has already been inserted, it will log a warning.
    pub fn alloc(&self, elem: T) -> &T {
        let elem = self.base.alloc(elem);
        if let Some(old) = self.by_name.borrow_mut().insert(elem.name(), elem as *const T) {
            // SAFETY: `old` is a pointer to a value in `self`, so it's alive
            log::warn!("Inserting element with same name as an existing element: {:?} and {:?}", elem, unsafe { &*old });
        }
        elem
    }

    /// Checks whether an element is in the arena.
    #[inline]
    pub fn contains(&self, elem: &T) -> bool {
        self.base.contains(elem)
    }

    /// Checks whether an element with the name is in the arena.
    pub fn has_name(&self, name: &T::Name) -> bool {
        self.by_name.borrow().contains_key(name)
    }

    /// Gets an element by its name, or `None` if it is not in the arena.
    pub fn with_name(&self, name: &T::Name) -> Option<&T> {
        // SAFETY: Pointer points to data in `self` so it's alive
        self.by_name.borrow().get(name).map(|elem| unsafe { &**elem })
    }

    /// Checks if an element with the exact annotation is present
    #[inline]
    pub fn has_ann(&self, ann: &Ann<'tree>) -> bool {
        self.base.has_ann(ann)
    }

    /// Gets the element with the given annotation, if present
    #[inline]
    pub fn with_ann(&self, ann: &Ann<'tree>) -> Option<&T> {
        self.base.with_ann(ann)
    }

    /// Insert multiple elements and return a reference to the slice.
    ///
    /// If any elements with the same name have already been inserted, it will log a warning.
    pub fn alloc_extend(&self, elems: impl Iterator<Item=T>) -> &[T] {
        let elems = self.base.alloc_extend(elems);
        let mut by_name = self.by_name.borrow_mut();
        for elem in elems {
            if let Some(old) = by_name.insert(elem.name(), elem as *const T) {
                // SAFETY: `old` is a pointer to a value in `self`, so it's alive
                log::warn!("Inserting element with same name as an existing element: {:?} and {:?}", elem, unsafe { &*old });
            }
        }
        elems
    }

    /// Makes sure there's enough continuous space for at least `additional` elements.
    ///
    /// This may save some work if called before [Self::alloc_extend]. On the other hand this might
    /// waste up to `n - 1` elements of space. In case new allocation is needed, the unused ones in
    /// current chunk are never used.
    #[inline]
    pub fn reserve_extend(&self, additional: usize) {
        self.base.reserve_extend(additional);
    }

    /// Iterate over the elements in the arena, in the order of their annotations (*not* the order
    /// they were allocated)
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.base.iter()
    }

    /// Convert this arena into a [NameRemover], to remove allocated elements and coalesce before
    /// converting back.
    #[inline]
    pub fn into_remover(self) -> NameRemover<T> {
        NameRemover::new(self.base.into_remover())
    }
}