use std::cell::Ref;
use std::iter::FusedIterator;

/// Adapts a [Ref] of an [Iterator] into an [Iterator] itself, keeping the ref alive
pub struct RefIterator<'a, T: 'a, I: Iterator<Item = T> + 'a> {
    iter: Ref<'a, I>
}

impl<'a, T: 'a, I: Iterator<Item = T> + 'a> RefIterator<'a, T, I> {
    pub fn new(iter: Ref<'a, I>) -> RefIterator<'a, T, I> {
        RefIterator { iter }
    }
}

impl<'a, T: 'a, I: Iterator<Item = T> + 'a> Iterator for RefIterator<'a, T, I> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a, T: 'a, I: DoubleEndedIterator<Item = T> + 'a> DoubleEndedIterator for RefIterator<'a, T, I> {
    fn next_back(&mut self) -> Option<T> {
        self.iter.next_back()
    }
}

impl<'a, T: 'a, I: ExactSizeIterator<Item = T> + 'a> ExactSizeIterator for RefIterator<'a, T, I> {}

impl<'a, T: 'a, I: FusedIterator<Item = T> + 'a> FusedIterator for RefIterator<'a, T, I> {}