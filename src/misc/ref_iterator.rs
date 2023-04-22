use std::cell::{Ref, RefCell};
use std::iter::FusedIterator;

/// Lets you safely create an [Iterator] using a [RefCell] reference, and the while the iterator is
/// alive the reference will be borrowed.
///
/// There is no UB, but calling `borrow_mut` on the [RefCell] while this is alive will **panic**.
pub struct RefIterator<'a, R: 'a, T, I: Iterator<Item = T>> {
    /// A reference, which must be kept alive so that the borrow is tracked while the iterator is
    /// alive. This makes it safe to hold the iterator, which doesn't have a borrow tracker itself.
    #[allow(unused)] ref_: Ref<'a, R>,
    /// The iterator
    iter: I
}

impl<'a, R: 'a, T, I: Iterator<Item = T> + 'a> RefIterator<'a, R, T, I> {
    /// Borrows `ref_cell` to create the iterator. The borrow is held from creation until this
    /// instance is dropped.
    pub fn new(ref_cell: &'a RefCell<R>, get_iter: impl FnOnce(&'a R) -> I) -> RefIterator<'a, R, T, I> {
        let ref_ = ref_cell.borrow();
        // SAFETY: We store ref_ so that the borrow is tracked while iter is alive
        let iter = get_iter(unsafe { ref_cell.try_borrow_unguarded() }.unwrap());
        RefIterator { ref_, iter }
    }
}

impl<'a, R: 'a, T, I: Iterator<Item = T> + 'a> Iterator for RefIterator<'a, R, T, I> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, R: 'a, T, I: DoubleEndedIterator<Item = T> + 'a> DoubleEndedIterator for RefIterator<'a, R, T, I> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<'a, R: 'a, T, I: ExactSizeIterator<Item = T> + 'a> ExactSizeIterator for RefIterator<'a, R, T, I> {}

impl<'a, R: 'a, T, I: FusedIterator<Item = T> + 'a> FusedIterator for RefIterator<'a, R, T, I> {}