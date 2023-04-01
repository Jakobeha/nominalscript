/// Returns the element if the condition is met, otherwise empty
pub fn once_if<T>(cond: bool, f: impl FnOnce() -> T) -> impl Iterator<Item=T> {
    Some(f).filter(|_| cond).map(|f| f()).into_iter()
}

/// Returns the iterator if the condition is met, otherwise empty
pub fn iter_if<T, I: Iterator<Item=T>>(cond: bool, mk_iter: impl FnOnce() -> I) -> impl Iterator<Item=T> {
    once_if(cond, mk_iter).flatten()
}