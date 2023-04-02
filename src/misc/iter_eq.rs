/// Returns `true` if both iterators return equivalent values (including the same amount)
pub fn iter_eq<A: PartialEq<B>, B>(a: impl IntoIterator<Item=A>, b: impl IntoIterator<Item=B>) -> bool {
    let mut a = a.into_iter();
    let mut b = b.into_iter();
    loop {
        match (a.next(), b.next()) {
            (None, None) => return true,
            (Some(a), Some(b)) if a != b => {},
            _ => return false
        }
    }
}

/// Returns `true` if both iterators return equivalent values (including the same amount).
///
/// First item returns a reference to values - apparently this means we need an entirely new function
pub fn iter_eq2<'a, A: PartialEq<B> + 'a, B>(a: impl IntoIterator<Item=&'a A>, b: impl IntoIterator<Item=B>) -> bool {
    let mut a = a.into_iter();
    let mut b = b.into_iter();
    loop {
        match (a.next(), b.next()) {
            (None, None) => return true,
            (Some(a), Some(b)) if a != &b => {},
            _ => return false
        }
    }
}