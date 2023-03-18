use std::error::Error;
use std::fmt::{Debug, Display};
use derive_more::Display;

/// Owned or borrowed value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
pub enum Oob<'a, T> {
    Owned(T),
    Borrowed(&'a T),
}

impl<'a, T: Debug + Display + Error> Error for Oob<'a, T> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Oob::Owned(ref t) => t.source(),
            Oob::Borrowed(t) => t.source(),
        }
    }
}
