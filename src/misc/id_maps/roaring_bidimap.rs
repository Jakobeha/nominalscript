use nonempty::NonEmpty;
use replace_with::replace_with_or_default;
use roaring::RoaringTreemap;
use crate::misc::id_maps::RoaringMap;

/// Bidirectional but *not bijective*\* variant of [RoaringMap].
///
/// The only benefit over a regular `RoaringMap<u64>` is performance:
/// we store the right-to-left multimap.
///
/// \* Each left has zero or one right, but each right can have many lefts.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct RoaringBidimap {
    ltr: RoaringMap<u64>,
    rtl: RoaringMap<NonEmpty<u64>>,
}

pub struct RoaringBidimapCollectOverwritten {
    pub bidimap: RoaringBidimap,
    pub overwritten: Vec<u64>
}

pub struct RoaringBidimapIgnoreOverwritten {
    pub bidimap: RoaringBidimap
}

// TODO: strip unused or test
#[allow(unused)]
impl RoaringBidimap {
    pub fn new() -> Self {
        Self { ltr: RoaringMap::new(), rtl: RoaringMap::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { ltr: RoaringMap::with_capacity(capacity), rtl: RoaringMap::with_capacity(capacity) }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.ltr.reserve(additional);
        self.rtl.reserve(additional);
    }

    pub fn insert(&mut self, left: u64, right: u64) -> Option<u64> {
        let old_right = self.ltr.insert(left, right);
        if let Some(old_right) = old_right {
            // Try to fastcase if rtl only has one element or old_right == right
            let old_lefts = self.rtl.get_mut(old_right).expect("self.rtl[self.ltr[left]] doesn't exist");
            if old_lefts.tail().is_empty() {
                assert_eq!(old_lefts.head, left, "self.rtl[self.ltr[left]] doesn't contain left");
                if old_right == right {
                    old_lefts.head = left;
                    return Some(right)
                } else {
                    self.rtl.remove(old_right);
                }
            } else {
                let old_index = old_lefts.iter().position(|&old_left| old_left == left)
                    .expect("self.rtl[self.ltr[left]] doesn't contain left");
                if old_right == right {
                    old_lefts[old_index] = left;
                    return Some(right)
                } else if old_index == 0 {
                    replace_with_or_default(
                        old_lefts,
                        |old_lefts| NonEmpty::from_vec(old_lefts.tail).unwrap()
                    );
                } else {
                    old_lefts.tail.remove(old_index - 1);
                }
            }
        }
        if !self.rtl.contains_key(right) {
            self.rtl.insert(right, NonEmpty::new(left));
        } else {
            self.rtl.get_mut(right).unwrap().push(left);
        }
        old_right
    }

    pub fn get_ltr(&self, left: u64) -> Option<u64> {
        self.ltr.get(left).copied()
    }

    pub fn get_ltr_and_idx(&self, left: u64) -> Option<(usize, u64)> {
        self.get_ltr(left).map(|right|
            (self.get_rtl(right).unwrap().iter().position(|&left2|
                left2 == left).unwrap(), right))
    }

    pub fn get_ltr_and_idx_len(&self, left: u64) -> Option<((usize, usize), u64)> {
        self.get_ltr(left).map(|right| {
            let lefts = self.get_rtl(right).unwrap();
            ((lefts.iter().position(|&left2| left2 == left).unwrap(), lefts.len()), right)
        })
    }

    pub fn get_rtl(&self, right: u64) -> Option<&NonEmpty<u64>> {
        self.rtl.get(right)
    }

    pub fn contains_left(&self, left: u64) -> bool {
        self.ltr.contains_key(left)
    }

    pub fn contains_right(&self, right: u64) -> bool {
        self.rtl.contains_key(right)
    }

    //noinspection DuplicatedCode
    pub fn remove_left(&mut self, left: u64) -> Option<u64> {
        let Some(right) = self.ltr.remove(left) else { return None };
        // Try to fastcase if rtl only has one element or old_right == right
        let lefts = self.rtl.get_mut(right).expect("self.rtl[self.ltr[left]] doesn't exist");
        if lefts.tail().is_empty() {
            self.rtl.remove(right);
        } else {
            let old_index = lefts.iter().position(|&old_left| old_left == left)
                .expect("self.rtl[self.ltr[left]] doesn't contain left");
            if old_index == 0 {
                replace_with_or_default(
                    lefts,
                    |lefts| NonEmpty::from_vec(lefts.tail).unwrap()
                );
            } else {
                lefts.tail.remove(old_index - 1);
            }
        }
        Some(right)
    }

    //noinspection DuplicatedCode
    pub fn remove_right(&mut self, right: u64) -> Option<NonEmpty<u64>> {
        let Some(lefts) = self.rtl.remove(right) else { return None };
        for &left in &lefts {
            let also_right = self.ltr.remove(left);
            debug_assert_eq!(also_right, Some(right));
        }
        Some(lefts)
    }

    pub fn clear(&mut self) {
        self.ltr.clear();
        self.rtl.clear();
    }

    //noinspection DuplicatedCode
    pub fn iter(&self) -> impl Iterator<Item = (u64, u64)> + '_ {
        self.ltr.iter().map(|(left, &right)| (left, right))
    }

    pub fn iter_rights(&self) -> impl Iterator<Item = (&NonEmpty<u64>, u64)> + '_ {
        self.rtl.iter().map(|(left_but_actually_right, right_but_actually_left)|
            (right_but_actually_left, left_but_actually_right))
    }

    pub fn into_iter_rights(self) -> impl Iterator<Item = (NonEmpty<u64>, u64)> {
        self.rtl.into_iter().map(|(left_but_actually_right, right_but_actually_left)|
            (right_but_actually_left, left_but_actually_right))
    }

    pub fn lefts(&self) -> &RoaringTreemap {
        &self.ltr.keys()
    }

    pub fn rights(&self) -> &RoaringTreemap {
        &self.rtl.keys()
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

impl RoaringBidimapCollectOverwritten {
    pub fn new() -> Self {
        Self { bidimap: RoaringBidimap::new(), overwritten: Vec::new() }
    }

    pub fn insert(&mut self, left: u64, right: u64) {
        if let Some(overwritten) = self.bidimap.insert(left, right) {
            self.overwritten.push(overwritten);
        }
    }
}

impl RoaringBidimapIgnoreOverwritten {
    pub fn new() -> Self {
        Self { bidimap: RoaringBidimap::new() }
    }

    pub fn insert(&mut self, left: u64, right: u64) {
        self.bidimap.insert(left, right);
    }
}

// Don't want to implement FromIterator and Extend on RoaringBidimap because overwritten could be unexpected;
// this option still works because you can just create use RoaringBidimapIgnoreOverwritten if you don't care
//noinspection DuplicatedCode
impl FromIterator<(u64, u64)> for RoaringBidimapCollectOverwritten {
    fn from_iter<T: IntoIterator<Item = (u64, u64)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl Extend<(u64, u64)> for RoaringBidimapCollectOverwritten {
    fn extend<T: IntoIterator<Item = (u64, u64)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

//noinspection DuplicatedCode
impl FromIterator<(u64, u64)> for RoaringBidimapIgnoreOverwritten {
    fn from_iter<T: IntoIterator<Item = (u64, u64)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl Extend<(u64, u64)> for RoaringBidimapIgnoreOverwritten {
    fn extend<T: IntoIterator<Item = (u64, u64)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}