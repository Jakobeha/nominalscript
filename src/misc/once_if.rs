/// Returns the element if the condition is met, otherwise empty
pub fn once_if<T>(cond: bool, f: impl FnOnce() -> T) -> impl Iterator<Item=T> {
    Some(f).filter(|_| cond).into_iter()
}