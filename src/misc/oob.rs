use derive_more::{Display, Error};

/// Owned or borrowed value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display, Error)]
pub enum Oob<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}
