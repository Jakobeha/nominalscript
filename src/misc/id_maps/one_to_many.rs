use nonempty::NonEmpty;
use crate::misc::id_maps::{Id, IdMany2One, IdSet};

/// `HashMap<L, NonEmpty<R>>`, but faster and works both ways
#[derive(Debug, Default, Clone, PartialEq)]
pub struct IdOne2Many<L: Id, R: Id>(IdMany2One<R, L>);

pub struct IdRevBidimapCollectOverwritten<L: Id, R: Id> {
    pub rev_bidimap: IdOne2Many<L, R>,
    pub overwritten: Vec<L>
}

pub struct IdRevBidimapIgnoreOverwritten<L: Id, R: Id> {
    pub rev_bidimap: IdOne2Many<L, R>
}

impl<L: Id, R: Id> IdOne2Many<L, R> {
    pub fn new() -> Self {
        Self(IdMany2One::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(IdMany2One::with_capacity(capacity))
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }

    pub fn insert(&mut self, left: L, right: R) -> Option<L> {
        self.0.insert(right, left)
    }

    pub fn get_ltr(&self, left: L) -> Option<&NonEmpty<R>> {
        self.0.get_rtl(left)
    }

    pub fn get_rtl(&self, right: R) -> Option<L> {
        self.0.get_ltr(right)
    }

    pub fn get_rtl_and_idx(&self, right: R) -> Option<(usize, L)> {
        self.0.get_ltr_and_idx(right)
    }

    pub fn get_rtl_and_idx_len(&self, right: R) -> Option<((usize, usize), L)> {
        self.0.get_ltr_and_idx_len(right)
    }

    pub fn contains_left(&self, left: L) -> bool {
        self.0.contains_right(left)
    }

    pub fn contains_right(&self, right: R) -> bool {
        self.0.contains_left(right)
    }

    pub fn remove_left(&mut self, left: L) -> Option<NonEmpty<R>> {
        self.0.remove_right(left)
    }

    pub fn remove_right(&mut self, right: R) -> Option<L> {
        self.0.remove_left(right)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    //noinspection DuplicatedCode
    pub fn iter(&self) -> impl Iterator<Item = (L, R)> + '_ {
        self.0.iter().map(|(l_but_r, r_but_l)| (r_but_l, l_but_r))
    }

    pub fn iter_rights(&self) -> impl Iterator<Item = (L, &NonEmpty<R>)> + '_ {
        self.0.iter_rights().map(|(l_but_r, r_but_l)| (r_but_l, l_but_r))
    }

    pub fn lefts(&self) -> &IdSet<L> {
        self.0.rights()
    }

    pub fn rights(&self) -> &IdSet<R> {
        self.0.lefts()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn len_lefts(&self) -> usize {
        self.0.len_rights()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<L: Id, R: Id> IdRevBidimapCollectOverwritten<L, R> {
    pub fn new() -> Self {
        Self { rev_bidimap: IdOne2Many::new(), overwritten: Vec::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        if let Some(overwritten) = self.rev_bidimap.insert(left, right) {
            self.overwritten.push(overwritten);
        }
    }
}

impl<L: Id, R: Id> IdRevBidimapIgnoreOverwritten<L, R> {
    pub fn new() -> Self {
        Self { rev_bidimap: IdOne2Many::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        self.rev_bidimap.insert(left, right);
    }
}

// Don't want to implement FromIterator and Extend on RoaringBidimap because overwritten could be unexpected;
// this option still works because you can just create use RoaringBidimapIgnoreOverwritten if you don't care
//noinspection DuplicatedCode
impl<L: Id, R: Id> FromIterator<(L, R)> for IdRevBidimapCollectOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Id> Extend<(L, R)> for IdRevBidimapCollectOverwritten<L, R> {
    fn extend<T: IntoIterator<Item = (L, R)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.rev_bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Id> FromIterator<(L, R)> for IdRevBidimapIgnoreOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Id> Extend<(L, R)> for IdRevBidimapIgnoreOverwritten<L, R> {
    fn extend<T: IntoIterator<Item = (L, R)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.rev_bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}