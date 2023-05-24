use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::ops::{Bound, RangeBounds};

use slice_group_by::GroupBy;
use type_sitter_lib::tree_sitter_wrapper::Node;

use crate::misc::VecFilter;
use crate::semantic::ann::{HasAnn, NodePointExt, Point};
use crate::semantic::arena::AnnArena;

/// A collection of elements which you can't access, but can remove based on their annotations.
/// The purpose is to remove semantic nodes whose syntax has changed.
///
/// This is roughly the opposite of [AnnArena], which allows you to add and access elements behind a
/// shared reference. You convert [AnnArena] into [AnnRemover] to remove some elements, and then
/// convert back into [AnnArena] when you are finished.
pub struct AnnRemover<'tree, T: HasAnn<'tree>> {
    /// Elements in order of their annotations, with removed ones replaced with `None` (saves
    /// performance vs removing them directly)
    ordered_elems: Vec<Option<T>>,
    /// Indices of elements containing each point and the points until the next entry
    indices_at_point: BTreeMap<Point<'tree>, Vec<usize>>,
}

impl<'tree, T: HasAnn<'tree>> AnnRemover<'tree, T> {
    /// Create an [AnnRemover] from an [AnnArena]'s inner data
    pub(super) fn new(mut elems: Vec<T>) -> Self {
        elems.sort_by_key(|elem| elem.ann());
        elems.dedup_by_key(|elem| elem.ann());
        let (mut start_points, mut end_points) = elems.iter()
            .enumerate()
            .flat_map(|(index, elem)| elem.ann().sources()
                .map(|s| ((s.start_point(), index), (s.end_point(), index))))
            .unzip::<_, _, Vec<_>, Vec<_>>();
        start_points.sort_by_key(|(point, _)| point);
        end_points.sort_by_key(|(point, _)| point);
        // Mergesort merge
        let mut points = {
            let mut points = Vec::with_capacity(start_points.len() + end_points.len());
            let mut start_points = start_points.into_iter();
            let mut end_points = end_points.into_iter();
            let mut next_start_point = start_points.next();
            let mut next_end_point = end_points.next();
            while let (Some((start_point, start_index)), Some((end_point, end_index))) = (next_start_point, next_end_point) {
                match start_point.0.cmp(&end_point.0) {
                    Ordering::Less => {
                        points.push((start_point, false, start_index));
                        next_start_point = start_points.next();
                    }
                    Ordering::Greater => {
                        points.push((end_point, true, end_index));
                        next_end_point = end_points.next();
                    }
                    Ordering::Equal => {
                        points.push((start_point, false, start_index));
                        points.push((end_point, true, end_index));
                        next_start_point = start_points.next();
                        next_end_point = end_points.next();
                    }
                }
            }
            while let Some((start_point, start_index)) = next_start_point {
                points.push((start_point, false, start_index));
                next_start_point = start_points.next();
            }
            points.extend(next_start_point.map(|(point, index)| (point, false, index)));
            points.extend(next_end_point.map(|(point, index)| (point, true, index)));
            points.extend(start_points.map(|(point, index)| (point, false, index)));
            points.extend(end_points.map(|(point, index)| (point, true, index)));
            points
        };
        let mut current_indices = Vec::new();
        let indices_at_point = points
            .linear_group_by(|(lhs_point, _, _), (rhs_point, _, _)| lhs_point == rhs_point)
            .map(|changes_at_point| {
                let point = changes_at_point.first().unwrap().0;
                if let Ok(num_added) = usize::try_from(changes_at_point.iter().fold(0isize, |acc, (_, is_end, _)| match is_end {
                    false => acc + 1,
                    true => acc - 1,
                })) {
                    current_indices.reserve(num_added as usize);
                }
                for (_, is_end, index) in changes_at_point.iter() {
                    match is_end {
                        false => {
                            debug_assert!(!current_indices.contains(index), "Element {} was added twice", index);
                            current_indices.push(*index)
                        },
                        true => match current_indices.find_remove(|i| i == index) {
                            Some(_) => (),
                            None => panic!("Element {} was removed twice", index),
                        }
                    }
                }
                (point, current_indices.clone())
            })
            .collect::<BTreeMap<_, _>>();
        Self {
            ordered_elems: elems.into_iter().map(Some).collect(),
            indices_at_point
        }
    }

    /// Remove elements which don't satisfy the predicate
    pub(super) fn retain(&mut self, predicate: impl FnMut(&T) -> bool) {
        for elem in &mut self.ordered_elems.iter_mut() {
            // For this and all other remove operations, we don't need to change `indices_at_point`,
            //   because removing already-removed elements is redundant, and it will probably waste
            //   more efficiency to change `indices_at_point` than to leave it alone
            if matches!(elem, Some(elem) if !predicate(elem)) {
                *elem = None;
            }
        }
    }

    /// Remove all elements at the given point. Does nothing to elements already removed.
    fn remove_at_point(&mut self, point: Point<'tree>) {
        if let Some((_, indices)) = self.indices_at_point.range(..point).next_back() {
            for index in indices {
                self.ordered_elems[index] = None;
            }
        }
    }

    /// Remove all elements within the given point range. Does nothing to elements already removed.
    fn remove_in_range(&mut self, range: impl RangeBounds<Point<'tree>>) {
        match range.start_bound() {
            Bound::Included(start) | Bound::Excluded(start) => self.remove_at_point(*start),
            Bound::Unbounded => (),
        }
        for (_, indices) in self.indices_at_point.range(range) {
            for index in indices {
                self.ordered_elems[index] = None;
            }
        }
    }

    /// Remove all elements which would be affected by the given node; that is, elements which are
    /// in the given node's range (including start point but excluding end point).
    pub fn remove_in_node(&mut self, node: Node<'tree>) {
        self.remove_in_range(node.point_range());
    }

    /// Converts this back into an [AnnArena], so we can view and add elements again.
    pub fn into_arena(self) -> AnnArena<T> {
        let mut arena = AnnArena::<T>::new();
        arena.alloc_extend(self.into_ordered_elems().filter_map(|e| e));
        arena
    }

    /// Converts this into the ordered elements, in order to convert this back into some type of
    /// arena.
    pub(super) fn into_ordered_elems(self) -> impl Iterator<Item = Option<T>> {
        self.ordered_elems.into_iter()
    }
}