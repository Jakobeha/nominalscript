use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;

/// Reference which checks for equality and gets hashed based on identity (address).
///
/// Semantic nodes are interned within the scope they originated from, except for toplevel scopes
/// which are interned within the package filepath-to-scope map. There should never exist two
/// interned semantic nodes which have equal contents but different addresses, it would create bugs
/// because we expect them to be equal.
#[derive(Debug)]
pub struct Interned<'a, T>(&'a T);

impl<'a, T> Interned<'a, T> {
    /// Create a new [Interned] reference from a semantic node. The returned reference *must* be
    /// stored ("interned") in a scope, or be a toplevel scope itself; and the scope must not have
    /// stored any other equal semantic nodes.
    pub(in crate::semantic) fn new_unchecked(node: &'a T) -> Self {
        Self(node)
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = &'a T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> AsRef<T> for Interned<'a, T> {
    #[inline]
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<'a, T> Clone for Interned<'a, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T> Copy for Interned<'a, T> {}

impl<'a, T> PartialEq for Interned<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const T, other.0 as *const T)
    }
}

impl<'a, T> Eq for Interned<'a, T> {}

impl<'a, T> Hash for Interned<'a, T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.0 as *const T, state)
    }
}

impl<'a, T: Display> Display for Interned<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0, f)
    }
}