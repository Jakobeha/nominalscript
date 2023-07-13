use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::ptr::NonNull;
use crate::semantic::storage::{Generation, Id};
use crate::semantic::storage::ann::Ann;
use crate::semantic::storage::id::RootSetId;

/// Collection of semantic nodes of a specific type in a package.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootSet<'tree, Data> {
    pub(in crate::semantic::storage) data: HashMap<Ann<'tree>, Data>,
    pub(in crate::semantic::storage) id: RootSetId,
    pub(in crate::semantic::storage) generation: Generation,
    /// If `true`, no more nodes can be added to this set until the generation is incremented.
    pub(in crate::semantic::storage) is_frozen: bool,
}

/// Contains the root set of the given semantic node type, so you can use it to access and mutate
/// semantic node instances' data from their [Id]s.
///
/// This is similar to [HasStore], but instead of allocation, provides indexing, and instead of
/// access to the underlying store, provides the indexing directly.
pub trait HasData<'tree, Data>: for<'a> IndexMut<&'a Id<'tree, Data>, Output=Data> {}

impl<'tree, Data> RootSet<'tree, Data> {
    /// Create a new, empty set
    #[inline]
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            id: RootSetId::new(),
            generation: Generation::new(),
            is_frozen: false,
        }
    }

    /// Insert a new node into the set with the given annotation and data, and return an identifier
    /// to it. *Warns* and replaces if a node with the annotation already exists in the set (to be
    /// clear, this is a bug but not fatal).
    ///
    /// *Panics* if frozen.
    #[inline]
    pub fn insert(&mut self, ann: Ann<'tree>, data: Data) -> Id<'tree, Data> where Data: Debug {
        assert!(!self.is_frozen, "attempted to insert into frozen set");

        if let Some(old) = self.data.insert(ann, data) {
            log::warn!("semantic node with this identifier already in the set ({:?}), replacing", old);
        }
        Id {
            id: ann,
            owner: self.id,
            // No point in caching; since this is already not frozen, the cache is already outdated.
            // Setting to the outdated generation ensures the (dangling) value will never be read.
            cached: Cell::new((NonNull::dangling(), Generation::outdated())),
            _p: PhantomData,
        }
    }

    /// Remove the given node from the set. *Panics* if the identifier isn't in the set (only
    /// possible if we use an identifier from another set).
    ///
    /// *Panics* if frozen.
    #[inline]
    pub fn remove(&mut self, node: &Id<'tree, Data>) -> Data {
        assert_eq!(node.owner, self.id, "attempted to remove node from a different set");
        assert!(!self.is_frozen, "attempted to remove from frozen set");

        self.data.remove(&node.id).expect("internal error: set-id is correct, but no semantic node with this identifier in the set (how?)")
    }

    /// Iterate over the semantic nodes (identifiers and data) in the set
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = Id<'tree, Data>> + '_ {
        self.data.iter().map(|(ann, data)| Id {
            id: *ann,
            owner: self.id,
            // This may be frozen *and* we already have the data without lookup, so try to cache.
            // If we aren't frozen, too bad, we can still set this.
            cached: Cell::new((NonNull::from(data), self.generation)),
            _p: PhantomData,
        })
    }

    /// Disables mutation (calls to [Self::insert], [Self::remove], and [IndexMut] will *panic*)
    /// until the generation is incremented. This enables caching data references in [Id]s.
    #[inline]
    pub fn freeze(&mut self) {
        assert!(!self.is_frozen, "attempted to freeze already frozen set");

        self.is_frozen = true;
    }

    /// Increments the generation and unfreezes. *Warns* if not frozen.
    #[inline]
    pub fn increment_generation(&mut self) {
        if !self.is_frozen {
            log::warn!("unexpected state: incrementing generation of unfrozen set");
        }

        self.generation.increment();
        self.is_frozen = false;
    }
}

impl<'a, 'tree, Data> Index<&'a Id<'tree, Data>> for RootSet<'tree, Data> {
    type Output = Data;

    /// Lookup a node's data from its identifier. *Panics* if the node isn't in the set (only
    /// possible using an [Id] constructed from another set).
    #[inline]
    fn index(&self, node: &'a Id<'tree, Data>) -> &Self::Output {
        assert_eq!(node.owner, self.id, "attempted to index with identifier from a different set");

        if self.is_frozen {
            // We're frozen, so either use or update cache for next time
            let (cached, cache_generation) = node.cached.get();
            match cache_generation.cmp(&self.generation) {
                Ordering::Less => {
                    // Cache is null or out of date, update it
                    let data = self.data.get(node.ann()).expect("no semantic node with this identifier in the set");
                    node.cached.set((NonNull::from(data), self.generation));
                    data
                }
                Ordering::Equal => {
                    // Cache is up to date, return it
                    // SAFETY: Eq owner, eq generation, and frozen means that the map didn't mutate
                    // since the pointer was set, and the map's pointers are stable sans mutation.
                    unsafe { cached.as_ref() }
                }
                Ordering::Greater => {
                    // Cache is from the future, this is a bug
                    panic!("semantic node cache is from the future")
                }
            }
        } else {
            // Can't reach cache because we're not frozen, so it may be outdated.
            self.data.get(node.ann()).expect("internal error: set-id is correct, but no semantic node with this identifier in the set (how?)")
        }
    }
}

impl<'a, 'tree, Data> IndexMut<&'a Id<'tree, Data>> for RootSet<'tree, Data> {
    /// Lookup a node's data from its identifier (mutable). *Panics* if the node isn't in the set
    /// (only possible using an [Id] constructed from another set).
    #[inline]
    fn index_mut(&mut self, node: &'a Id<'tree, Data>) -> &mut Data {
        assert_eq!(node.owner, self.id, "attempted to mutably index with identifier from a different set");
        assert!(!self.is_frozen, "attempted to mutably index into frozen set");

        self.data.get_mut(node.ann()).expect("internal error: set-id is correct, but no semantic node with this identifier in the set")
    }
}

impl<'tree, Data> HasData<'tree, Data> for RootSet<'tree, Data> {}