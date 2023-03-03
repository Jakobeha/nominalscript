use std::collections::{btree_map, BTreeMap};
use std::ops::RangeBounds;
use nonempty::NonEmpty;
use replace_with::replace_with;
use crate::misc::id_maps::{Id, IdMap, IdSet};

/// `BTreeMap<R, NonEmpty<L>>`, but faster and works both ways
#[derive(Debug, Default, Clone, PartialEq)]
pub struct IdBTreeMany2One<L: Id, R: Copy + Ord> {
    ltr: IdMap<L, R>,
    rtl: BTreeMap<R, NonEmpty<L>>
}

pub struct IdBTreeBidimapCollectOverwritten<L: Id, R: Copy + Ord> {
    pub bidimap: IdBTreeMany2One<L, R>,
    pub overwritten: Vec<R>
}

pub struct IdBTreeBidimapIgnoreOverwritten<L: Id, R: Copy + Ord> {
    pub bidimap: IdBTreeMany2One<L, R>
}

impl<L: Id, R: Copy + Ord> IdBTreeMany2One<L, R> {
    pub fn new() -> Self {
        Self { ltr: IdMap::new(), rtl: BTreeMap::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { ltr: IdMap::with_capacity(capacity), rtl: BTreeMap::new() }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.ltr.reserve(additional);
    }

    pub fn insert(&mut self, left: L, right: R) -> Option<R> {
        let old_right = self.ltr.insert(left, right);
        if let Some(old_right) = old_right {
            // Try to fastcase if rtl only has one element or old_right == right
            let old_lefts = self.rtl.get_mut(&old_right).expect("self.rtl[self.ltr[left]] doesn't exist");
            if old_lefts.tail().is_empty() {
                assert_eq!(old_lefts.head, left, "self.rtl[self.ltr[left]] doesn't contain left");
                if old_right == right {
                    old_lefts.head = left;
                    return Some(right)
                } else {
                    self.rtl.remove(&old_right);
                }
            } else {
                let old_index = old_lefts.iter().position(|&old_left| old_left == left)
                    .expect("self.rtl[self.ltr[left]] doesn't contain left");
                if old_right == right {
                    old_lefts[old_index] = left;
                    return Some(right)
                } else if old_index == 0 {
                    replace_with(
                        old_lefts,
                        || NonEmpty::new(L::from(u64::default())),
                        |old_lefts| NonEmpty::from_vec(old_lefts.tail).unwrap()
                    );
                } else {
                    old_lefts.tail.remove(old_index - 1);
                }
            }
        }
        if !self.rtl.contains_key(&right) {
            self.rtl.insert(right, NonEmpty::new(left));
        } else {
            self.rtl.get_mut(&right).unwrap().push(left);
        }
        old_right
    }

    pub fn get_ltr(&self, left: L) -> Option<R> {
        self.ltr.get(left).copied()
    }

    pub fn get_ltr_and_idx(&self, left: L) -> Option<(usize, R)> {
        self.get_ltr(left).map(|right|
            (self.get_rtl(right).unwrap().iter().position(|&left2|
                left2 == left).unwrap(), right))
    }

    pub fn get_ltr_and_idx_len(&self, left: L) -> Option<((usize, usize), R)> {
        self.get_ltr(left).map(|right| {
            let lefts = self.get_rtl(right).unwrap();
            ((lefts.iter().position(|&left2| left2 == left).unwrap(), lefts.len()), right)
        })
    }

    pub fn get_rtl(&self, right: R) -> Option<&NonEmpty<L>> {
        self.rtl.get(&right)
    }

    pub fn range_rtl(&self, range: impl RangeBounds<R>) -> btree_map::Range<'_, R, NonEmpty<L>> {
        self.rtl.range(range)
    }

    pub fn contains_left(&self, left: L) -> bool {
        self.ltr.contains_key(left)
    }

    pub fn contains_right(&self, right: R) -> bool {
        self.rtl.contains_key(&right)
    }

    pub fn remove_left(&mut self, left: L) -> Option<R> {
        let Some(right) = self.ltr.remove(left) else { return None };
        // Try to fastcase if rtl only has one element or old_right == right
        let lefts = self.rtl.get_mut(&right).expect("self.rtl[self.ltr[left]] doesn't exist");
        if lefts.tail().is_empty() {
            self.rtl.remove(&right);
        } else {
            let old_index = lefts.iter().position(|&old_left| old_left == left)
                .expect("self.rtl[self.ltr[left]] doesn't contain left");
            if old_index == 0 {
                replace_with(
                    lefts,
                    || NonEmpty::new(L::from(u64::default())),
                    |lefts| NonEmpty::from_vec(lefts.tail).unwrap()
                );
            } else {
                lefts.tail.remove(old_index - 1);
            }
        }
        Some(right)
    }

    //noinspection DuplicatedCode
    pub fn remove_right(&mut self, right: R) -> Option<NonEmpty<L>> {
        let Some(lefts) = self.rtl.remove(&right) else { return None };
        for &left in &lefts {
            let also_right = self.ltr.remove(left);
            debug_assert!(also_right == Some(right), "self.ltr[left] != right");
        }
        Some(lefts)
    }

    pub fn clear(&mut self) {
        self.ltr.clear();
        self.rtl.clear();
    }

    //noinspection DuplicatedCode
    pub fn iter(&self) -> impl Iterator<Item = (L, R)> + '_ {
        self.ltr.iter().map(|(left, &right)| (left, right))
    }

    pub fn iter_rights(&self) -> impl Iterator<Item = (&NonEmpty<L>, R)> + '_ {
        self.rtl.iter().map(|(&left_but_actually_right, right_but_actually_left)|
            (right_but_actually_left, left_but_actually_right))
    }

    pub fn lefts(&self) -> &IdSet<L> {
        self.ltr.keys()
    }

    pub fn rights(&self) -> btree_map::Keys<'_, R, NonEmpty<L>> {
        self.rtl.keys()
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.ltr.len() >= self.rtl.len());
        self.ltr.len()
    }

    pub fn len_rights(&self) -> usize {
        debug_assert!(self.ltr.len() >= self.rtl.len());
        self.rtl.len()
    }


    pub fn is_empty(&self) -> bool {
        debug_assert_eq!(self.ltr.is_empty(), self.rtl.is_empty());
        self.ltr.is_empty()
    }
}

impl<L: Id, R: Copy + Ord> IdBTreeBidimapCollectOverwritten<L, R> {
    pub fn new() -> Self {
        Self { bidimap: IdBTreeMany2One::new(), overwritten: Vec::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        if let Some(overwritten) = self.bidimap.insert(left, right) {
            self.overwritten.push(overwritten);
        }
    }
}

impl<L: Id, R: Copy + Ord> IdBTreeBidimapIgnoreOverwritten<L, R> {
    pub fn new() -> Self {
        Self { bidimap: IdBTreeMany2One::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        self.bidimap.insert(left, right);
    }
}

// Don't want to implement FromIterator and Extend on IdBTreeBimap because overwritten could be unexpected;
// this option still works because you can just create use IdBTreeBimapIgnoreOverwritten if you don't care
//noinspection DuplicatedCode
impl<L: Id, R: Copy + Ord> FromIterator<(L, R)> for IdBTreeBidimapCollectOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Copy + Ord> Extend<(L, R)> for IdBTreeBidimapCollectOverwritten<L, R> {
    fn extend<T: IntoIterator<Item = (L, R)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Copy + Ord> FromIterator<(L, R)> for IdBTreeBidimapIgnoreOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Copy + Ord> Extend<(L, R)> for IdBTreeBidimapIgnoreOverwritten<L, R> {
    fn extend<T: IntoIterator<Item = (L, R)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}