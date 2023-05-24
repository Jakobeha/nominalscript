use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::ops::RangeBounds;

use typed_arena_nomut::Arena;

use crate::semantic::ann::{Ann, HasAnn};
use crate::semantic::arena::AnnRemover;

/// An [Arena](https://docs.rs/typed-arena/latest/typed_arena/) of annotated nodes, which iterates
/// the nodes in order of their annotations.
///
/// Warns against insertion of multiple elements with the same annotation (technically this should
/// never happen, but handling is easy so it's considered a soft error).
pub struct AnnArena<'tree, T: HasAnn<'tree>> {
    arena: Arena<T>,
    // TODO: Replace with `DroplessBTreeMap`s
    by_ann: RefCell<BTreeMap<LiveAnnPtr<'tree>, *const T>>,
}

/// Pointer to [Ann] which we enforce is alive as long as this struct is
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct LiveAnnPtr<'tree>(*const Ann<'tree>);

impl<'tree, T: HasAnn<'tree>> AnnArena<'tree, T> {
    /// Create an empty [AnnArena]
    pub fn new() -> Self {
        AnnArena {
            arena: Arena::new(),
            by_ann: RefCell::new(BTreeMap::new()),
            // by_start_point: RefCell::new(BTreeMap::new()),
            // by_end_point: RefCell::new(BTreeMap::new()),
        }
    }

    /// Insert an element and return its reference.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning.
    pub fn alloc(&self, elem: T) -> &T {
        let elem = self.arena.alloc(elem);
        // SAFETY: the `LiveAnnPtr` and `elem` are both in `self`, so the former won't outlive the latter
        if let Some(old) = self.by_ann.borrow_mut().insert(unsafe { LiveAnnPtr::of(elem.ann()) }, elem as *const T) {
            // SAFETY: `old` is a pointer to a value in `self`, so it's alive
            log::warn!("Inserting element with same annotation as an existing element: {:?} and {:?}", elem, unsafe { &*old });
        }
        elem
    }

    /// Checks whether an element is in the arena.
    pub fn contains(&self, elem: &T) -> bool {
        self.by_ann.borrow().contains(LiveAnnPtr::ref_of(&elem.ann()))
    }

    /// Checks if an element with the exact annotation is present
    pub fn has_ann(&self, ann: &Ann<'tree>) -> bool {
        self.by_ann.borrow().contains_key(LiveAnnPtr::ref_of(&ann))
    }

    /// Gets the element with the exact annotation, if present
    pub fn with_ann(&self, ann: &Ann<'tree>) -> Option<&T> {
        self.by_ann.borrow().get(LiveAnnPtr::ref_of(&ann))
            // SAFETY: Pointer points to data in `self` so it's alive
            .map(|&elem| unsafe { &*elem })
    }

    /// Insert multiple elements and return a reference to the slice.
    ///
    /// If any elements with the same annotation have already been inserted, it will log a warning.
    pub fn alloc_extend(&self, elems: impl Iterator<Item=T>) -> &[T] {
        let elems = self.arena.alloc_extend(elems);
        let mut by_ann = self.by_ann.borrow_mut();
        for elem in elems {
            // SAFETY: the `LiveAnnPtr` and `elem` are both in `self`, so the former won't outlive the latter
            if let Some(old) = by_ann.insert(unsafe { LiveAnnPtr::of(elem.ann()) }, elem as *const T) {
                // SAFETY: `old` is a pointer to a value in `self`, so it's alive
                log::warn!("Inserting element with same annotation as an existing element: {:?} and {:?}", elem, unsafe { &*old });
            }
        }
        elem
    }

    /// Makes sure there's enough continuous space for at least `additional` elements.
    ///
    /// This may save some work if called before [Self::alloc_extend]. On the other hand this might
    /// waste up to `n - 1` elements of space. In case new allocation is needed, the unused ones in
    /// current chunk are never used.
    #[inline]
    pub fn reserve_extend(&self, additional: usize) {
        self.arena.reserve_extend(additional);
    }

    /// Iterate over the elements in the arena, in the order of their annotations (*not* the order
    /// they were allocated)
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.by_ann.borrow().values().map(|&elem| {
            // SAFETY: Pointer points to data in `self` so it's alive
            unsafe { &*elem }
        })
    }

    /// Convert this arena into an [AnnRemover], to remove allocated elements and coalesce before
    /// converting back.
    pub fn into_remover(self) -> AnnRemover<T> {
        AnnRemover::new(self.arena.into_vec())
    }
}

impl<'tree> LiveAnnPtr<'tree> {
    /// SAFETY: You must guarantee that `value` is still alive when this is used.
    unsafe fn of(value: &Ann<'tree>) -> Self {
        LiveAnnPtr(value as *const Ann<'tree>)
    }

    /// Safe because this is a reference
    fn ref_of<'a>(ann: &'a &Ann<'tree>) -> &'a Self {
        unsafe { &*(ann as *const &Ann<'tree> as *const Self)}
    }
}

impl<'tree> AsRef<Ann<'tree>> for LiveAnnPtr<'tree> {
    #[inline]
    fn as_ref(&self) -> &Ann<'tree> {
        // SAFETY: Struct was created with the invariant that as long as the pointed-to value is
        //   alive, it's also alive
        unsafe { &*self.0 }
    }
}

impl<'tree> PartialEq for LiveAnnPtr<'tree> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl<'tree> Eq for LiveAnnPtr<'tree> {}

impl<'tree> PartialOrd for LiveAnnPtr<'tree> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<'tree> Ord for LiveAnnPtr<'tree> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}