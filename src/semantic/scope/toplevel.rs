use std::ops::{Deref, DerefMut};
use crate::semantic::ann::Ann;
use crate::semantic::scope::{OwnedScope, Scope};

/// A top-level scope
#[derive(Debug)]
pub struct TopLevelScope<'tree>(OwnedScope<'tree>);

impl<'tree> TopLevelScope<'tree> {
    /// Create a new, empty top-level scope
    #[inline]
    pub fn new(ann: Ann<'tree>) -> Self {
        TopLevelScope(OwnedScope::new(ann, None))
    }
}

impl<'tree> From<TopLevelScope<'tree>> for Scope<'tree> {
    #[inline]
    fn from(s: TopLevelScope<'tree>) -> Self {
        s.0.into()
    }
}

impl<'tree> Deref for TopLevelScope<'tree> {
    type Target = OwnedScope<'tree>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'tree> DerefMut for TopLevelScope<'tree> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'tree> AsRef<OwnedScope<'tree>> for TopLevelScope<'tree> {
    #[inline]
    fn as_ref(&self) -> &OwnedScope<'tree> {
        &self.0
    }
}
