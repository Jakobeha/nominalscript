/// `Vec::extend_no_dup`
pub trait VecExtendNoDup<T> {
    /// Extend the vector with the given iterator, but don't add any items which are already in the
    /// vector.
    fn extend_no_dup(&mut self, iter: impl IntoIterator<Item = T>);
}

impl<T: PartialEq> VecExtendNoDup<T> for Vec<T> {
    fn extend_no_dup(&mut self, iter: impl IntoIterator<Item = T>) {
        let iter = iter.into_iter();
        // May reserve extra but that's ok, we assume that there are not many already-contained values
        self.reserve(iter.size_hint().0);
        for item in iter {
            if !self.contains(&item) {
                self.push(item);
            }
        }
    }
}