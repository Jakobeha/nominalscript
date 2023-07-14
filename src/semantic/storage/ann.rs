use std::cmp::Ordering;
use std::fmt::Debug;
use std::iter::{empty, once, zip};
use std::path::Path;

use auto_enums::auto_enum;
use yak_sitter::{Node, Range};
use btree_plus_store::copyable::BTreeSet;

pub use point::*;

mod point;

/// Annotation = source (syntax) info for semantic nodes.
///
/// For proper memoization, *any* syntax node used to create a semantic node must be part of its
/// annotation. When syntqx nodes are edited, we:
///
/// - Delete all semantic nodes annotated with deleted or changed syntax nodes
/// - (Re)process inserted or changed syntax nodes, which will insert new data into scopes
///   and parent expressions (including re-inserting data which was deleted because its syntax
///   changed)
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ann<'tree> {
    #[default]
    /// The annotated semantic node is defined globally, so not tied to any source
    Intrinsic,
    /// The annotated semantic node was parsed from source
    DirectSource {
        /// Source syntax node it was derived from
        loc: Node<'tree>
    },
    /// The annotated semantic node came from the absence of source
    ImplicitSource {
        /// Syntax node immediately before what would be parsed into this.
        ///
        /// The end of this node is the point where the implicit source would exist, and the next
        /// node is the syntax node immediately after what would be parsed into this.
        before_loc: Node<'tree>
    },
    /// The annotated semantic node was inferred from source
    InferredSource {
        /// Syntax nodes it was inferred from
        locs: DerivedNodeSet<'tree>
    },
    /// The annotated semantic node was derived from other semantic nodes
    Derived {
        /// Syntax nodes of derived semantic nodes
        locs: DerivedNodeSet<'tree>
    }
}

pub type DerivedNodeSet<'tree> = BTreeSet<'tree, Node<'tree>>;

impl<'tree> Ann<'tree> {
    /// Source location(s) AKA syntax nodes this semantic node was created from, in lexicographic order
    #[auto_enum(DoubleEndedIterator)]
    pub fn sources(&self) -> impl DoubleEndedIterator<Item=&Node<'tree>> + '_ {
        match self {
            Self::Intrinsic => empty(),
            Self::DirectSource { loc } => once(loc),
            Self::ImplicitSource { before_loc } => once(before_loc),
            Self::InferredSource { locs } => locs.iter(),
            Self::Derived { locs } => locs.iter()
        }
    }

    /// Number of sources
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

    /// Source location ranges for error reporting.
    #[inline]
    pub fn ranges(&self) -> impl DoubleEndedIterator<Item=Range> + '_ {
        self.sources().map(|source| source.range())
    }

    /// Whether one of this annotation's sources contains the given point
    #[inline]
    pub fn contains_point(&self, point: Point<'tree>) -> bool {
        self.sources().any(|source| source.contains_point(point))
    }

    /// Whether one of this annotation's sources intersects the given point range
    #[inline]
    pub fn intersects_range(&self, range: std::ops::Range<Point<'tree>>) -> bool {
        self.sources().any(|source| source.intersects_range(range.clone()))
    }


    /// Whether one of this annotation's sources touches or intersects the given point range
    #[inline]
    pub fn touches_or_intersects_range(&self, range: std::ops::Range<Point<'tree>>) -> bool {
        self.sources().any(|source| source.touches_or_intersects_range(range.clone()))
    }
}

impl<'tree> PartialOrd for Ann<'tree> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'tree> Ord for Ann<'tree> {
    fn cmp(&self, other: &Self) -> Ordering {
        zip(self.sources(), other.sources())
            .map(|(lhs, rhs)| lhs.cmp(rhs))
            .fold(Ordering::Equal, Ordering::then)
            .then(self.num_sources().cmp(&other.num_sources()))
    }
}