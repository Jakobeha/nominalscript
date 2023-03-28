/// `Vec::filter`
pub trait VecFilter<T> {
    /// Find and then, if found, remove a single element.
    fn find_remove(&mut self, predicate: impl FnMut(&T) -> bool) -> Option<T>;
}

impl<T> VecFilter<T> for Vec<T> {
    fn find_remove(&mut self, predicate: impl FnMut(&T) -> bool) -> Option<T> {
        self.iter().position(predicate).map(|i| self.remove(i))
    }
}