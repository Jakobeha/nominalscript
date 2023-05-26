use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::mem::MaybeUninit;
use std::ops::{Bound, RangeBounds};

use once_cell::unsync::OnceCell;
use replace_with::replace_with;
use slice_group_by::GroupBy;
use type_sitter_lib::tree_sitter_wrapper::Node;
use typed_arena_nomut::Arena;

use crate::misc::VecFilter;
use crate::semantic::ann::{HasAnn, NodePointExt, Point};

/// A special set of semantic nodes, with 3 runtime-enforced phases:
///
/// - **Insertion**: functions as an [Arena](https://docs.rs/typed-arena/latest/typed_arena/) where
///   you can only insert semantic nodes (and can do so behind a shared reference)
/// - **Retrieval**: functions as a set where you can only access and iterate nodes. Nodes are
///   iterated in order of their syntactic location (importantly, order is unaffected by insertion).
/// - **Removal**: functions as a set where you can only remove nodes based on their syntactic
///   location
///
/// Nodes returned from insertion and retrieval are behind **shared** references, but transitioning
/// to the removal phase is behind a **mutable** reference, because in removal phase any of those
/// nodes could be freed.
///
/// If multiple semantic nodes are inserted with the same annotation, it will log a warning, and
/// only choose one of them (currently this should never happen so we could also panic; if we ever
/// support multi-threading and aggregation it would become valid and we'd no longer warn).
pub struct AnnArena<'tree, T: HasAnn<'tree>> {
    state: AnnArenaState<'tree, T>
}

/// The phase an [AnnArena] is in (see [AnnArena] docs)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnnArenaPhase {
    /// Functions as an [Arena](https://docs.rs/typed-arena/latest/typed_arena/) where you can only
    /// insert semantic nodes (and can do so behind a shared reference)
    Insertion,
    /// Functions as a set where you can only access and iterate nodes. Nodes are iterated in order
    /// of their syntactic location (importantly, order is unaffected by insertion).
    Retrieval,
    /// Functions as a set where you can only remove nodes based on their syntactic location
    /// (importantly, order is unaffected by insertion).
    Removal,
}

/// [AnnArena] underlying repr and state data, which indirectly identifies its phase
enum AnnArenaState<'tree, T: HasAnn<'tree>> {
    Shared {
        /// Storage for elements
        arena: Arena<T>,
        /// Retrieval data (a cache) and identifies whether we're in the [AnnArenaPhase::Insertion]
        /// (if `None`) or [AnnArenaPhase::Retrieval] (if `Some`) phase.
        retrieval_data: OnceCell<AnnArenaRetrievalData<'tree, T>>,
    },
    Removal {
        /// Elements in order of their annotations, with removed ones replaced with `None` (saves
        /// performance vs removing them directly)
        ordered_elems: Vec<Option<T>>,
        /// Indices of elements containing each point and the points until the next entry
        indices_at_point: BTreeMap<Point<'tree>, Vec<usize>>,
    },
    /// Only happens if there is a caught panic when changing into the [AnnArenaPhase::Removal]
    /// phase. The [AnnArena] stores nothing and can no longer be used.
    Broken
}

/// [AnnArena] underlying state for retrieval data (a cache)
struct AnnArenaRetrievalData<'tree, T: HasAnn<'tree>> {
    /// Indices of and pointers to elements in `arena`.
    ///
    /// The indices into the arena are for *the original (current) order*
    ordered_by_ann: Vec<(usize, *const T)>,
    /// Indices of and pointers to elements containing each point and the points until the next
    /// entry.
    ///
    /// The indices are for *the order after sorting by annotation*.
    at_point: BTreeMap<Point<'tree>, Vec<(usize, *const T)>>,
}

impl<'tree, T: HasAnn<'tree>> AnnArena<'tree, T> {
    /// Create an empty [AnnArena] in the [AnnArenaPhase::Insertion].
    pub fn new() -> Self {
        AnnArena {
            state: AnnArenaState::Shared {
                arena: Arena::new(),
                retrieval_data: OnceCell::new(),
            },
        }
    }

    /// Get the current phase. **Panics** if in the [AnnArenaPhase::Broken] phase.
    pub fn phase(&self) -> AnnArenaPhase {
        match &self.state {
            AnnArenaState::Shared { retrieval_data, .. } => match retrieval_data.get() {
                None => AnnArenaPhase::Insertion,
                Some(_) => AnnArenaPhase::Retrieval,
            },
            AnnArenaState::Removal { .. } => AnnArenaPhase::Removal,
            AnnArenaState::Broken => panic!("AnnArena::phase: called in broken phase"),
        }
    }

    /// Returns whether [AnnArena] is in the "broken" phase, which only happens if there's a caught
    /// panic when changing into the [AnnArenaPhase::Removal] phase.
    ///
    /// The "broken" phase isn't part of [AnnArenaPhase], because most users won't care about it and
    /// will simply want to panic if it happens.
    pub fn is_broken(&self) -> bool {
        matches!(self.state, AnnArenaState::Broken)
    }

    // region insertion/retrieval/removal conversions
    /// Converts from [AnnArenaPhase::Insertion] to [AnnArenaPhase::Retrieval].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    pub fn conv_retrieval(&self) {
        let AnnArenaState::Shared { arena, retrieval_data } = &self.state else {
            panic!("AnnArena::conv_retrieval: called in removal or broken phase");
        };
        let Ok(()) = retrieval_data.set(Self::mk_retrieval_data(arena)) else {
            panic!("AnnArena::conv_retrieval: called in retrieval phase")
        };
    }

    #[inline]
    fn mk_retrieval_data(arena: &Arena<T>) -> AnnArenaRetrievalData<'tree, T> {
        // This will become `ordered_by_ann`, so we convert `elem` to pointer even though we convert
        // back later; and we want `ordered_by_ann` to have the arena's indices, so we `enumerate`
        // here.
        let mut elems = arena.iter().map(|elem| elem as *const T).enumerate().collect::<Vec<_>>();
        // SAFETY: We just converted to pointer and `arena` is still alive, so in the following
        //     unsafe pointer-to-reference conversions `elem` is still alive
        elems.sort_by_key(|&(_, elem)| unsafe { &*elem }.ann());
        elems.dedup_by(|&(_, lhs), &(_, rhs)| {
            let (lhs, rhs) = unsafe { (&*lhs, &*rhs) };
            if lhs.ann() == rhs.ann() {
                log::warn!("AnnArena: multiple elements with the same annotation inserted: {:?} and {:?}", lhs, rhs);
                true
            } else {
                false
            }
        });
        // The index in `elems` is for the original arena, but we want the sorted index here, which
        // is why we `enumerate` and ignore the original index (yes, it's very confusing)
        let (mut start_points, mut end_points) = elems.iter()
            .enumerate()
            .flat_map(|(index, &(_, elem))| unsafe { &*elem }.ann().sources()
                .map(|s| ((s.start_point(), index, elem), (s.end_point(), index, elem))))
            .unzip::<_, _, Vec<_>, Vec<_>>();
        start_points.sort_by_key(|(point, _, _)| point);
        end_points.sort_by_key(|(point, _, _)| point);
        // Mergesort merge
        let mut points = {
            let mut points = Vec::with_capacity(start_points.len() + end_points.len());
            let mut start_points = start_points.into_iter();
            let mut end_points = end_points.into_iter();
            let mut next_start_point = start_points.next();
            let mut next_end_point = end_points.next();
            while let (Some((start_point, start_index, start_elem)), Some((end_point, end_index, end_elem))) = (next_start_point, next_end_point) {
                match start_point.0.cmp(&end_point.0) {
                    Ordering::Less => {
                        points.push((start_point, false, (start_index, start_elem)));
                        next_start_point = start_points.next();
                    }
                    Ordering::Greater => {
                        points.push((end_point, true, (end_index, end_elem)));
                        next_end_point = end_points.next();
                    }
                    Ordering::Equal => {
                        points.push((start_point, false, (start_index, start_elem)));
                        points.push((end_point, true, (end_index, end_elem)));
                        next_start_point = start_points.next();
                        next_end_point = end_points.next();
                    }
                }
            }
            while let Some((start_point, start_index, start_elem)) = next_start_point {
                points.push((start_point, false, (start_index, start_elem)));
                next_start_point = start_points.next();
            }
            points.extend(next_start_point.map(|(point, index, elem)| (point, false, (index, elem))));
            points.extend(next_end_point.map(|(point, index, elem)| (point, true, (index, elem))));
            points.extend(start_points.map(|(point, index, elem)| (point, false, (index, elem))));
            points.extend(end_points.map(|(point, index, elem)| (point, true, (index, elem))));
            points
        };
        let mut current_indices_and_elems = Vec::new();
        let at_point = points
            .linear_group_by(|(lhs_point, _, _), (rhs_point, _, _)| lhs_point == rhs_point)
            .map(|changes_at_point| {
                let point = changes_at_point.first().unwrap().0;
                if let Ok(num_added) = usize::try_from(changes_at_point.iter().fold(0isize, |acc, (_, is_end, _)| match is_end {
                    false => acc + 1,
                    true => acc - 1,
                })) {
                    current_indices_and_elems.reserve(num_added as usize);
                }
                for (_, &is_end, index_and_elem) in changes_at_point {
                    match is_end {
                        false => {
                            debug_assert!(!current_indices_and_elems.contains(index_and_elem), "Index and element {:?} was added twice", index_and_elem);
                            current_indices_and_elems.push(*index_and_elem)
                        },
                        true => match current_indices_and_elems.find_remove(|ie| ie == index_and_elem) {
                            Some(_) => (),
                            None => panic!("Index and element {:?} was removed twice", index_and_elem),
                        }
                    }
                }
                (point, current_indices_and_elems.clone())
            })
            .collect::<BTreeMap<_, _>>();
        AnnArenaRetrievalData { ordered_by_ann: elems, at_point }
    }

    /// Converts from [AnnArenaPhase::Retrieval] to [AnnArenaPhase::Removal].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Retrieval] phase.
    pub fn conv_removal(&mut self) {
        replace_with(&mut self.state, AnnArenaState::Broken, |state| {
            let AnnArenaState::Shared { retrieval_data, arena } = state else {
                panic!("AnnArena::conv_removal: called in removal or broken phase");
            };
            let Some(retrieval_data) = retrieval_data.into_inner() else {
                panic!("AnnArena::conv_removal: called in insertion phase");
            };
            Self::mk_removal_data(arena, retrieval_data)
        })
    }

    #[inline]
    fn mk_removal_data(arena: Arena<T>, retrieval_data: AnnArenaRetrievalData<T>) -> AnnArenaState<T> {
        let mut elems = arena.into_vec().into_iter().map(Some).collect::<Vec<_>>();
        let ordered_elems = retrieval_data.ordered_by_ann.into_iter().map(|(arena_index, _)| {
            Some(elems[arena_index].take().expect("same index is repeateed in retrieval data ordered_by_ann"))
        }).collect::<Vec<_>>();
        let indices_at_point = retrieval_data.at_point.into_iter().map(|(point, indices_and_elems)| {
            let ordered_indices = indices_and_elems.into_iter().map(|(ordered_index, _)| ordered_index).collect::<Vec<_>>();
            (point, ordered_indices)
        }).collect::<BTreeMap<_, _>>();
        AnnArenaState::Removal { ordered_elems, indices_at_point }
    }

    /// Converts from [AnnArenaPhase::Removal] to [AnnArenaPhase::Insertion].
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn conv_insertion(&mut self) {
        replace_with(&mut self.state, AnnArenaState::Broken, |state| {
            // ???: preserve indices_at_point so we don't have to regenerate when converting back
            //     into retrieval? If it's too expensive, we should try to find a way
            let AnnArenaState::Removal { ordered_elems, indices_at_point: _ } = state else {
                panic!("AnnArena::conv_insertion: called in insertion, retrieval, or broken phase");
            };
            let mut arena = Arena::new();
            arena.alloc_extend(ordered_elems.into_iter().filter_map(|elem| elem));
            AnnArenaState::Shared { arena, retrieval_data: OnceCell::new() }
        })
    }
    // endregion

    // region insertion methods
    /// Insert an element and return its reference.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning
    /// and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    pub fn alloc(&self, elem: T) -> &T {
        self.do_insertion("alloc", |arena| arena.alloc(elem))
    }

    /// Insert multiple elements and return a reference to the slice.
    ///
    /// If an element with the same annotation has already been inserted, it will log a warning
    /// and not insert when changing into retrieval phase.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Insertion] phase.
    pub fn alloc_extend(&self, elems: impl Iterator<Item=T>) -> &[T] {
        self.do_insertion("alloc_extend", |arena| arena.alloc_extend(elems))
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
    pub fn reserve_extend(&self, additional: usize) {
        self.do_insertion("reserve_extend", |arena| arena.reserve_extend(additional))
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
    pub unsafe fn alloc_uninitialized(&self, additional: usize) -> &mut [MaybeUninit<T>] {
        self.do_insertion("alloc_uninitialized", |arena| arena.alloc_uninitialized(additional))
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
    pub fn uninitialized_array(&self) -> *mut [MaybeUninit<T>] {
        self.do_insertion("uninitialized_array", |arena| arena.uninitialized_array())
    }
    // endregion

    // region retrieval methods
    /// Gets the number of elements
    pub fn len(&self) -> usize {
        self.do_retrieval("len", |arena, retrieval_data| retrieval_data.ordered_by_ann.len())
    }

    /// Iterates the elements in order of their annotations
    pub fn iter(&self) -> impl Iterator<Item=&T> {
        self.do_retrieval("iter", |arena, retrieval_data| {
            // SAFETY: The pointers point to data in `self.arena` so they are still alive
            retrieval_data.ordered_by_ann.iter().map(|&(_, elem)| unsafe { &*elem })
        })
    }

    /// Iterate all elements at the given point.
    pub fn at_point(&self, point: Point<'tree>) -> impl Iterator<Item=&T> {
        self.do_retrieval("at_point", |_arena, retrieval_data| {
            // SAFETY: The pointers point to data in `self.arena` so they are still alive
            Self::_at_point(point, retrieval_data).map(|elem| unsafe { &*elem })
        })
    }

    #[inline]
    fn _at_point<'a>(point: Point<'tree>, retrieval_data: &'a AnnArenaRetrievalData<'tree, T>) -> impl Iterator<Item=*const T> + 'a {
        retrieval_data.at_point.range(..point).next_back().into_iter()
            .flat_map(|(_, elems)| elems.iter()).map(|&(_, elem)| elem)
    }

    /// Iterate all elements within the given point range.
    pub fn in_range(&self, range: impl RangeBounds<Point<'tree>>) -> impl Iterator<Item=&T> {
        self.do_retrieval("at_point", |_arena, retrieval_data| {
            // SAFETY: The pointers point to data in `self.arena` so they are still alive
            Self::_in_range(range, retrieval_data).map(|elem| unsafe { &*elem })
        })
    }

    #[inline]
    fn _in_range<'a>(range: impl RangeBounds<Point<'tree>>, retrieval_data: &'a AnnArenaRetrievalData<'tree, T>) -> impl Iterator<Item=*const T> + 'a {
        let at_start = match range.start_bound() {
            Bound::Included(start) | Bound::Excluded(start) => {
                Self::_at_point(*start, retrieval_data)
            },
            Bound::Unbounded => None,
        }.into_iter().flatten();
        let after_start = retrieval_data.at_point.range(range)
            .flat_map(|(_, elems)| elems.iter().map(|&(_, elem)| elem));
        at_start.chain(after_start)
    }

    /// Returns all elements which would be affected by the given node; that is, elements which are
    /// in the given node's range (including start point but excluding end point).
    pub fn in_node(&self, node: Node<'tree>) -> impl Iterator<Item=&T> {
        self.do_retrieval("in_node", |_arena, retrieval_data| {
            // SAFETY: The pointers point to data in `self.arena` so they are still alive
            Self::_in_range(node.point_range(), retrieval_data).map(|elem| unsafe { &*elem })
        })
    }
    // endregion

    // region removal methods
    /// Removes the element at the specified index (in order of annotations). Does nothing if
    /// already removed. This is only for [NameArena]
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub(super) fn remove(&mut self, index: usize) {
        self.do_removal("remove", |ordered_by_ann, _indices_at_point| {
            ordered_by_ann[index] = None;
        })
    }

    /// Remove elements which don't satisfy the predicate.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn retain(&mut self, predicate: impl FnMut(&T) -> bool) {
        self.do_removal("retain", |ordered_by_ann, _indices_at_point| {
            for elem in ordered_by_ann {
                // For this and all other remove operations, we don't need to change `indices_at_point`,
                //   because removing already-removed elements is redundant, and it will probably waste
                //   more efficiency to change `indices_at_point` than to leave it alone
                if matches!(elem, Some(elem) if !predicate(elem)) {
                    *elem = None;
                }
            }
        })
    }

    /// Remove all elements at the given point. Does nothing to elements already removed.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn remove_at_point(&mut self, point: Point<'tree>) {
        self.do_removal("retain", |ordered_by_ann, indices_at_point| {
            Self::_remove_at_point(point, ordered_by_ann, indices_at_point)
        })
    }

    #[inline]
    fn _remove_at_point(
        point: Point<'tree>,
        ordered_by_ann: &mut Vec<Option<T>>,
        indices_at_point: &mut BTreeMap<Point<'tree>, Vec<usize>>
    ) {
        if let Some((_, indices)) = indices_at_point.range(..point).next_back() {
            for index in indices {
                ordered_by_ann[index] = None;
            }
        }
    }

    /// Remove all elements within the given point range. Does nothing to elements already removed.
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn remove_in_range(&mut self, range: impl RangeBounds<Point<'tree>>) {
        self.do_removal("retain", |ordered_by_ann, indices_at_point| {
            Self::_remove_in_range(range, ordered_by_ann, indices_at_point)
        })
    }

    #[inline]
    fn _remove_in_range(
        range: impl RangeBounds<Point<'tree>>,
        ordered_by_ann: &mut Vec<Option<T>>,
        indices_at_point: &mut BTreeMap<Point<'tree>, Vec<usize>>
    ) {
        match range.start_bound() {
            Bound::Included(start) | Bound::Excluded(start) => {
                Self::_remove_at_point(*start, ordered_by_ann, indices_at_point)
            },
            Bound::Unbounded => (),
        }
        for (_, indices) in indices_at_point.range(range) {
            for index in indices {
                ordered_by_ann[index] = None;
            }
        }
    }

    /// Remove all elements which would be affected by the given node; that is, elements which are
    /// in the given node's range (including start point but excluding end point).
    ///
    /// **Panics** if not in the [AnnArenaPhase::Removal] phase.
    pub fn remove_in_node(&mut self, node: Node<'tree>) {
        self.do_removal("retain", |ordered_by_ann, indices_at_point| {
            Self::_remove_in_range(node.point_range(), ordered_by_ann, indices_at_point)
        })
    }
    // endregion

    // region insertion/retrieval/removal visitors
    #[inline]
    fn do_insertion<R>(
        &self,
        fn_name: &'static str,
        fun: impl FnOnce(&Arena<T>) -> R
    ) -> R {
        let AnnArenaState::Shared { arena, retrieval_data } = &self.state else {
            panic!("AnnArena::{} called in removal or broken phase", fn_name);
        };
        let None = retrieval_data.get().is_none() else {
            panic!("AnnArena::{} called in retrieval phase", fn_name)
        };

        fun(arena)
    }

    #[inline]
    fn do_retrieval<R>(
        &self,
        fn_name: &'static str,
        fun: impl FnOnce(&Arena<T>, &AnnArenaRetrievalData<'tree, T>) -> R
    ) -> R {
        let AnnArenaState::Shared { arena, retrieval_data } = &self.state else {
            panic!("AnnArena::{} called in removal or broken phase", fn_name);
        };
        let Some(retrieval_data) = retrieval_data.get() else {
            panic!("AnnArena::{} called in insertion phase", fn_name);
        };

        fun(arena, retrieval_data)
    }

    #[inline]
    fn do_removal<R>(
        &mut self,
        fn_name: &'static str,
        fun: impl FnOnce(&mut Vec<Option<T>>, &mut BTreeMap<Point<'tree>, Vec<usize>>) -> R
    ) -> R {
        let AnnArenaState::Removal { ordered_elems, indices_at_point } = &mut self.state else {
            panic!("AnnArena::{} called in insertion, retrieval, or broken phase", fn_name);
        };

        fun(ordered_elems, indices_at_point)
    }
    // endregion
}