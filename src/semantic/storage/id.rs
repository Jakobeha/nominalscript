use std::borrow::Borrow;
use std::cell::Cell;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;
use derivative::Derivative;
use crate::semantic::storage::ann::Ann;
use crate::semantic::storage::generation::Generation;

/// An semantic node identifier: a typed wrapper around an annotation (source location) which
/// identifies a semantic node, taking advantage of the fact that there can't exist 2 different
/// semantic nodes of the same type derived from the exact same sources.
#[derive(Derivative)]
#[derivative(Debug(bound=""), Clone(bound=""))]
pub struct Id<'tree, T> {
    pub(super) id: Ann<'tree>,
    pub(super) owner: RootSetId,
    pub(super) cached: Cell<(NonNull<T>, Generation)>,
    pub(super) _p: PhantomData<&'tree T>,
}

/// Unique id for each [Set] which distinguishes its [Id]s from those in other [Set]s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct RootSetId(usize);

impl RootSetId {
    /// Create a new, unique set id
    pub(super) fn new() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(0);
        Self(NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

impl<'tree, T> Id<'tree, T> {
    /// Get the annotation which identifies this semantic node.
    #[inline]
    pub fn ann(&self) -> &Ann<'tree> {
        &self.id
    }
}

impl<'tree, T> PartialEq for Id<'tree, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        if self.owner != other.owner {
            // Different sets, can't be equal
            return false;
        }
        let (cached_ptr, cached_generation) = self.cached.get();
        let (other_cached_ptr, other_cached_generation) = other.cached.get();
        if !cached_generation.is_always_outdated() && cached_generation == other_cached_generation {
            // If both are cached, just check the pointers
            cached_ptr == other_cached_ptr
        } else {
            // Actual equality test
            self.id == other.id
        }
    }
}

impl<'tree, T> Eq for Id<'tree, T> {}

impl<'tree, T> PartialOrd for Id<'tree, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'tree, T> Ord for Id<'tree, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.owner != other.owner {
            // Different sets, we compare the ordering of the set ids
            return self.owner.cmp(&other.owner);
        }
        // The cached pointer order may be different from the actual order, so we can only use it
        // for equality, at which point we're probably faster just doing the fallback but...
        let (cached_ptr, cached_generation) = self.cached.get();
        let (other_cached_ptr, other_cached_generation) = other.cached.get();
        if !cached_generation.is_always_outdated() && cached_generation == other_cached_generation && cached_ptr == other_cached_ptr {
            std::cmp::Ordering::Equal
        } else {
            // Actual equality test
            self.id.cmp(&other.id)
        }
    }
}

impl<'tree, T> Hash for Id<'tree, T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.owner.hash(state);
    }
}

/// This allows annotations to be used to get map keys and set entries
impl<'tree, T> Borrow<Ann<'tree>> for Id<'tree, T> {
    fn borrow(&self) -> &Ann<'tree> {
        self.ann()
    }
}