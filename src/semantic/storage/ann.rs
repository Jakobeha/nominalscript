use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::iter::{empty, once};
use std::path::Path;

use auto_enums::auto_enum;
use type_sitter_lib::TypedNode;
use yak_sitter::Node;
use btree_plus_store::{copyable, BTreeStore};

pub use point::*;
use crate::semantic::storage::HasStore;

mod point;

/// Annotation = source (syntax) info for semantic nodes.
///
/// For proper memoization, *any* syntax node used to create a semantic node must be part of its
/// annotation. When syntqx nodes are edited, we:
///
/// - Delete all semantic nodes annotated with deleted or changed syntax nodes.
/// - (Re)process inserted or changed syntax nodes, which will insert new data into scopes
///   and parent expressions (including re-inserting data which was deleted because its syntax
///   changed).
///
/// Even though there are different types of annotations, the annotations are ultimately compared
/// based on their sources, and the types are only used for error reporting. This will lead to bugs
/// if you expect a direct source to be different than an inferred source or an implicit source, but
/// in practice you should not expect those things unless you're doing something wrong.
#[derive(Debug, Default, Clone, Copy)]
pub enum Ann<'tree> {
    #[default]
    /// The annotated semantic node is defined globally, so not tied to any source.
    Intrinsic,
    /// The annotated semantic node was parsed from source.
    DirectSource {
        /// Syntax node the semantic node was derived from.
        loc: Node<'tree>
    },
    /// The annotated semantic node came from the absence of source.
    ImplicitSource {
        /// Syntax node immediately after what would be parsed into the semantic node.
        ///
        /// Importantly, this syntax node should be what *prevented* the semantic node from *not*
        /// existing, i.e. caused it to exist. This means we can treat this syntax node as the
        /// "source" of the semantic node.
        after_loc: Node<'tree>
    },
    /// The annotated semantic node was inferred from source.
    InferredSource {
        /// One of more syntax nodes the semantic node was inferred from.
        locs: DerivedNodeSet<'tree>
    },
    /// The annotated semantic node was derived from other semantic nodes.
    Derived {
        /// Syntax nodes of the semantic nodes this semantic node was derived from.
        locs: DerivedNodeSet<'tree>
    }
}

pub type DerivedNodeSet<'tree> = copyable::BTreeSet<'tree, Node<'tree>>;
pub type DerivedNodeStore<'tree> = BTreeStore<Node<'tree>, ()>;

impl<'tree> Ann<'tree> {
    /// Create an annotation derived directly from a typed node. Generalized `Ann::DirectSource`
    /// which can take any typed node.
    #[inline]
    pub fn direct(node: impl TypedNode<'tree>) -> Self {
        Self::DirectSource { loc: node.into_node() }
    }

    /// Source location(s) AKA syntax nodes this semantic node was created from, in lexicographic
    /// order.
    #[auto_enum(DoubleEndedIterator)]
    pub fn sources(&self) -> impl DoubleEndedIterator<Item=&Node<'tree>> + '_ {
        match self {
            Self::Intrinsic => empty(),
            Self::DirectSource { loc } => once(loc),
            Self::ImplicitSource { after_loc } => once(after_loc),
            Self::InferredSource { locs } => locs.iter(),
            Self::Derived { locs } => locs.iter()
        }
    }

    /// Number of sources AKA syntax nodes
    pub fn num_sources(&self) -> usize {
        match self {
            Ann::Intrinsic => 0,
            Ann::DirectSource { .. } => 1,
            Ann::ImplicitSource { .. } => 1,
            Ann::InferredSource { locs } => locs.len(),
            Ann::Derived { locs } => locs.len()
        }
    }

    /// Primary source location AKA first syntax node this was created from. Some nodes may be
    /// created from multiple nodes, but when we need one, this is the one we choose
    #[inline]
    pub fn first_source(&self) -> Option<&Node<'tree>> {
        self.sources().next()
    }

    /// File path the semantic node was created from (path of its primary location). Very complex
    /// derived nodes may be created from multiple paths.
    #[inline]
    pub fn first_path(&self) -> Option<&'tree Path> {
        self.first_source().and_then(|n| n.path())
    }

    /// Whether one of this annotation's sources contains the given point
    #[inline]
    pub fn contains_point(&self, point: &Point<'tree>) -> bool {
        self.sources().any(|source| source.contains_point(&point))
    }

    /// Whether one of this annotation's sources intersects the given point range
    #[inline]
    pub fn intersects_range(&self, range: &RangeAtPath<'tree>) -> bool {
        self.sources().any(|source| source.intersects_range_at_path(range))
    }


    /// Whether one of this annotation's sources touches or intersects the given point range
    #[inline]
    pub fn touches_or_intersects_range(&self, range: &RangeAtPath<'tree>) -> bool {
        self.sources().any(|source| source.touches_or_intersects_range_at_path(range))
    }

    /// If either annotation is [Ann::Intrinsic], this becomes the other annotation. If they both
    /// have sources, this becomes [Ann::Derived]
    pub fn merge_with(&mut self, other: Ann<'tree>, store: impl HasStore<'tree, DerivedNodeStore<'tree>>) {
        *self = match (*self, other) {
            (this, Ann::Intrinsic) => this,
            (Ann::Intrinsic, other) => other,
            (this, other) => Ann::Derived {
                locs: DerivedNodeSet::build(store.inner_store(), |locs| {
                    locs.extend(this.sources().copied());
                    locs.extend(other.sources().copied());
                }),
            }
        }
    }
}

impl<'tree> From<Node<'tree>> for Ann<'tree> {
    #[inline]
    fn from(loc: Node<'tree>) -> Self {
        Self::DirectSource { loc }
    }
}

impl<'tree> PartialEq for Ann<'tree> {
    fn eq(&self, other: &Self) -> bool {
        self.sources().eq(other.sources())
    }
}

impl<'tree> Eq for Ann<'tree> {}

impl<'tree> PartialOrd for Ann<'tree> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'tree> Ord for Ann<'tree> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.sources().cmp(other.sources())
    }
}

impl<'tree> Hash for Ann<'tree> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for source in self.sources() {
            source.hash(state);
        }
    }
}