use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;

/// Reference which checks for equality based on identity
#[derive(Debug)]
#[repr(transparent)]
#[fundamental]
pub struct IdentityRef<'a, T>(&'a T);

impl<'a, T> From<&'a T> for IdentityRef<'a, T> {
    /// Create a new `IdentityRef` from a reference
    #[inline]
    fn from(value: &'a T) -> Self {
        Self(value)
    }
}

impl<'a, T> Deref for IdentityRef<'a, T> {
    type Target = &'a T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> AsRef<T> for IdentityRef<'a, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<'a, T> Clone for IdentityRef<'a, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Copy for IdentityRef<'a, T> {}

impl<'a, T> PartialEq for IdentityRef<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const T, other.0 as *const T)
    }
}

impl<'a, T> Eq for IdentityRef<'a, T> {}

impl<'a, T> Hash for IdentityRef<'a, T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0 as *const T, state)
    }
}

impl<'a, T: Display> Display for IdentityRef<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0, f)
    }
}