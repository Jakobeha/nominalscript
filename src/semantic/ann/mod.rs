use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::iter::{empty, once, zip};
use std::path::Path;

use auto_enums::auto_enum;
use streaming_iterator::StreamingIterator;
use yak_sitter::{Node, Range};

pub use point::*;

use crate::misc::define_wrapper_structs;
use crate::semantic::arena::IdentityRef;

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
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum Ann<'tree> {
    #[default]
    /// The annotated semantic node is defined globally, so not tied to any source
    Intrinsic,
    /// The annotated semantic node was parsed from source
    DirectSource {
        /// Source syntax node it was derived from
        loc: Node<'tree>
    },
    /// The annotated semantic node was inferred from source
    InferredSource {
        /// Syntax nodes it was inferred from
        locs: BTreeSet<Node<'tree>>
    },
    /// The annotated semantic node came from the absence of source
    ImplicitSource {
        /// Syntax node immediately before what would be parsed into this
        before_loc: Node<'tree>,
        /// Syntax node immediately after what would be parsed into this
        after_loc: Node<'tree>
    },
    /// The annotated semantic node was derived from other semantic nodes
    Derived {
        /// Syntax nodes of derived semantic nodes
        locs: BTreeSet<Node<'tree>>
    }
}

/// Type with an annotation
pub trait HasAnn<'tree>: Debug {
    /// Annotation (reference)
    fn ann(&self) -> &Ann<'tree>;
    /// Annotation (mutable reference)
    fn ann_mut(&mut self) -> &mut Ann<'tree>;

    /// Compare values' annotations
    #[inline]
    fn cmp_ann(&self, other: &Self) -> Ordering {
        self.ann().cmp(other.ann())
    }

    /// Does this value come after the other?
    #[inline]
    fn gt_ann(&self, other: &Self) -> bool {
        self.cmp_ann(other) == Ordering::Greater
    }

    /// Does this value come before the other?
    #[inline]
    fn lt_ann(&self, other: &Self) -> bool {
        self.cmp_ann(other) == Ordering::Less
    }

    /// Do this value and the other have the same annotation? (Means they should be the same)
    #[inline]
    fn eq_ann(&self, other: &Self) -> bool {
        self.cmp_ann(other) == Ordering::Equal
    }
}

impl<'tree> Ann<'tree> {
    /// Source location(s) AKA syntax nodes this semantic node was created from, in lexicographic order
    #[auto_enum]
    pub fn sources(&self) -> impl DoubleEndedIterator<Item=&Node<'tree>> + '_ {
        match self {
            Self::Intrinsic => empty(),
            Self::DirectSource { loc } => once(loc),
            Self::InferredSource { locs } => locs.iter(),
            Self::ImplicitSource { before_loc, after_loc } => once(before_loc).chain(once(after_loc)),
            Self::Derived { locs } => locs.iter()
        }
    }

    /// Number of sources
    pub fn num_sources(&self) -> usize {
        match self {
            Ann::Intrinsic => 0,
            Ann::DirectSource { .. } => 1,
            Ann::InferredSource { locs } => locs.len(),
            Ann::ImplicitSource { .. } => 2,
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
    pub fn ranges(&self) -> impl DoubleEndedIterator<Item=Range> {
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
        self.sources().any(|source| source.intersects_range(range))
    }


    /// Whether one of this annotation's sources touches or intersects the given point range
    #[inline]
    pub fn touches_or_intersects_range(&self, range: std::ops::Range<Point<'tree>>) -> bool {
        self.sources().any(|source| source.touches_or_intersects_range(range))
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
            .map(Node::cmp)
            .fold(Ordering::Equal, Ordering::then)
            .then(self.num_sources().cmp(&other.num_sources()))
    }
}

#[macro_export]
macro_rules! impl_has_ann_as_ref_body {
    ($tree:lifetime) => {
        #[inline]
        fn ann(&self) -> &$crate::semantic::ann::Ann<$tree> {
            self.as_ref().ann()
        }
        #[inline]
        fn ann_mut(&mut self) -> &mut $crate::semantic::ann::Ann<$tree> {
            self.as_mut().ann_mut()
        }
    }
}

#[macro_export]
macro_rules! impl_has_ann_as_ref {
    ($Name:ident<$a:lifetime, $T:ident>) => {
        impl<$a, 'tree, $T: $crate::semantic::ann::HasAnn<'tree>> $crate::semantic::ann::HasAnn<'tree> for $Name<$a, $T> {
            $crate::impl_has_ann_as_ref_body!('tree);
        }
    };
    ($Name:ident<$T:ident>) => {
        impl<'tree, $T: $crate::semantic::ann::HasAnn<'tree>> $crate::semantic::ann::HasAnn<'tree> for $Name<$T> {
            $crate::impl_has_ann_as_ref_body!('tree);
        }
    };
    ($Name:ident) => {
        impl<'tree> $crate::semantic::ann::HasAnn<'tree> for $Name {
            $crate::impl_has_ann_as_ref_body!('tree);
        }
    };
}

#[macro_export]
macro_rules! impl_has_ann_wrapper_struct {
    ($Name:ident) => {
        impl<'tree> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree> {
            #[inline]
            fn ann(&self) -> &$crate::semantic::ann::Ann<'tree> {
                self.0.ann()
            }
            #[inline]
            fn ann_mut(&mut self) -> &mut $crate::semantic::ann::Ann<'tree> {
                self.0.ann_mut()
            }
        }
    };
    ($Name:ident by $field:ident) => {
        impl<'tree> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree> {
            #[inline]
            fn ann(&self) -> &$crate::semantic::ann::Ann<'tree> {
                self.$field.ann()
            }
            #[inline]
            fn ann_mut(&mut self) -> &mut $crate::semantic::ann::Ann<'tree> {
                self.$field.ann_mut()
            }
        }
    };
}

macro_rules! impl_has_ann_record_struct_body {
    ($tree:lifetime) => {
        #[inline]
        fn ann(&self) -> &$crate::semantic::ann::Ann<$tree> {
            &self.ann
        }
        #[inline]
        fn ann_mut(&mut self) -> &mut $crate::semantic::ann::Ann<$tree> {
            &self.ann
        }
    }
}

#[macro_export]
macro_rules! impl_has_ann_record_struct {
    ($Name:ident) => {
        impl<'tree> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
    ($Name:ident<$T:ident>) => {
        impl<'tree, $T> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
    ($Name:ident<$T:ident: $Bound:ident>) => {
        impl<'tree, $T: $Bound<'tree>> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
}

#[macro_export]
macro_rules! impl_has_ann_enum_body {
    ($tree:lifetime $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        #[inline]
        fn ann(&self) -> &$crate::semantic::ann::Ann<$tree> {
            match self {
                $($(Self::$StructCase(x) => x.ann(),)*)?
                $($(Self::$RecordCase { ann, .. } => ann,)*)?
            }
        }
        #[inline]
        fn ann_mut(&mut self) -> &mut $crate::semantic::ann::Ann<$tree> {
            match self {
                $($(Self::$StructCase(x) => x.ann_mut(),)*)?
                $($(Self::$RecordCase { ann, .. } => ann,)*)?
            }
        }
    };
}

#[macro_export]
macro_rules! impl_has_ann_enum {
    ($Name:ident $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident> $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident: $Bound:ident> $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T: $Bound<'tree>> $crate::semantic::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
}

impl_has_ann_as_ref!(Box<T>);
impl_has_ann_as_ref!(IdentityRef<'a, T>);

impl<'tree> HasAnn<'tree> for Ann<'tree> {
    #[inline]
    fn ann(&self) -> &Ann {
        self
    }
    #[inline]
    fn ann_mut(&mut self) -> &mut Ann {
        self
    }
}
