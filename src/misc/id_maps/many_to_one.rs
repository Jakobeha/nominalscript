use std::marker::PhantomData;
use std::mem::transmute;
use nonempty::NonEmpty;
use crate::misc::id_maps::{Id, IdSet, RoaringBidimap};

/// `HashMap<R, NonEmpty<L>>`, but faster and works both ways
#[derive(Debug, Default, Clone, PartialEq)]
pub struct IdMany2One<L: Id, R: Id>(RoaringBidimap, PhantomData<(L, R)>);

pub struct IdBidimapCollectOverwritten<L: Id, R: Id> {
    pub bidimap: IdMany2One<L, R>,
    pub overwritten: Vec<R>
}

pub struct IdBidimapIgnoreOverwritten<L: Id, R: Id> {
    pub bidimap: IdMany2One<L, R>
}

impl<L: Id, R: Id> IdMany2One<L, R> {
    pub fn new() -> Self {
        Self(RoaringBidimap::new(), PhantomData)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(RoaringBidimap::with_capacity(capacity), PhantomData)
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }

    pub fn insert(&mut self, left: L, right: R) -> Option<R> {
        self.0.insert(left.into(), right.into()).map(R::from)
    }

    pub fn get_ltr(&self, left: L) -> Option<R> {
        self.0.get_ltr(left.into()).map(R::from)
    }

    pub fn get_ltr_and_idx(&self, left: L) -> Option<(usize, R)> {
        self.0.get_ltr_and_idx(left.into()).map(|(idx, r)| (idx, R::from(r)))
    }

    pub fn get_ltr_and_idx_len(&self, left: L) -> Option<((usize, usize), R)> {
        self.0.get_ltr_and_idx_len(left.into()).map(|(idx_and_len, r)| (idx_and_len, R::from(r)))
    }

    pub fn get_rtl(&self, right: R) -> Option<&NonEmpty<L>> {
        // SAFETY: transparent repr
        unsafe { transmute(self.0.get_rtl(right.into())) }
    }

    pub fn contains_left(&self, left: L) -> bool {
        self.0.contains_left(left.into())
    }

    pub fn contains_right(&self, right: R) -> bool {
        self.0.contains_right(right.into())
    }

    pub fn remove_left(&mut self, left: L) -> Option<R> {
        self.0.remove_left(left.into()).map(R::from)
    }

    //noinspection DuplicatedCode
    pub fn remove_right(&mut self, right: R) -> Option<NonEmpty<L>> {
        self.0.remove_right(right.into()).map(|lefts| lefts.map(L::from))
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    //noinspection DuplicatedCode
    pub fn iter(&self) -> impl Iterator<Item = (L, R)> + '_ {
        self.0.iter().map(|(l, r)| (L::from(l), R::from(r)))
    }

    pub fn iter_rights(&self) -> impl Iterator<Item = (&NonEmpty<L>, R)> + '_ {
        // SAFETY: equivalent repr (TODO: only safe on 64-bit machines, use IdNonEmpty to ensure transparent repr which will work for all)
        self.0.iter_rights()
            .map(|(lefts, r)| (unsafe { transmute(lefts) }, R::from(r)))
    }

    pub fn into_iter_rights(self) -> impl Iterator<Item = (NonEmpty<L>, R)> {
        // SAFETY: equivalent repr (TODO: only safe on 64-bit machines, use IdNonEmpty to ensure transparent repr which will work for all)
        self.0.into_iter_rights()
            .map(|(lefts, r)| (unsafe { hack_transmute_fix_later(lefts) }, R::from(r)))
    }

    pub fn lefts(&self) -> &IdSet<L> {
        // SAFETY: transparent repr
        unsafe { transmute(self.0.lefts()) }
    }

    pub fn rights(&self) -> &IdSet<R> {
        // SAFETY: transparent repr
        unsafe { transmute(self.0.rights()) }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn len_rights(&self) -> usize {
        self.0.len_rights()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<L: Id, R: Id> IdBidimapCollectOverwritten<L, R> {
    pub fn new() -> Self {
        Self { bidimap: IdMany2One::new(), overwritten: Vec::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        if let Some(overwritten) = self.bidimap.insert(left, right) {
            self.overwritten.push(overwritten);
        }
    }
}

impl<L: Id, R: Id> IdBidimapIgnoreOverwritten<L, R> {
    pub fn new() -> Self {
        Self { bidimap: IdMany2One::new() }
    }

    pub fn insert(&mut self, left: L, right: R) {
        self.bidimap.insert(left, right);
    }
}

// Don't want to implement FromIterator and Extend on RoaringBidimap because overwritten could be unexpected;
// this option still works because you can just create use RoaringBidimapIgnoreOverwritten if you don't care
//noinspection DuplicatedCode
impl<L: Id, R: Id> FromIterator<(L, R)> for IdBidimapCollectOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Id> Extend<(L, R)> for IdBidimapCollectOverwritten<L, R> {
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
impl<L: Id, R: Id> FromIterator<(L, R)> for IdBidimapIgnoreOverwritten<L, R> {
    fn from_iter<T: IntoIterator<Item = (L, R)>>(iter: T) -> Self {
        let mut map = Self::new();
        map.extend(iter);
        map
    }
}

//noinspection DuplicatedCode
impl<L: Id, R: Id> Extend<(L, R)> for IdBidimapIgnoreOverwritten<L, R> {
    fn extend<T: IntoIterator<Item = (L, R)>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        // By default, we assume all items will go in successfully (no overwritten)
        self.bidimap.reserve(iter.size_hint().0);
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}

#[inline]
unsafe fn hack_transmute_fix_later<A, B>(a: A) -> B {
    let b = std::ptr::read(&a as *const A as *const B);
    std::mem::forget(a);
    b
}