use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::iter::{empty, once};
use std::path::Path;
use auto_enums::auto_enum;
use type_sitter_lib::tree_sitter_wrapper::{Node, Range};
use crate::misc::arena::IdentityRef;

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
    /// The annotated semantic node came from source
    DirectSource {
        /// Source syntax node it was derived from
        loc: Node<'tree>
    },
    /// The annotated semantic node came from the absence of source
    ImplicitSource {
        /// Syntax node immediately before what would be parsed into this
        before_loc: Node<'tree>,
        /// Syntax node immediately after what would be parsed into this
        after_loc: Node<'tree>
    },
    /// The annotated semantic node was inferred from source
    InferredSource {
        /// "Main" syntax node it was inferred from
        primary_loc: Node<'tree>,
        /// Other syntax nodes it was inferred from
        other_locs: BTreeSet<Node<'tree>>
    },
    /// The annotated semantic node was derived from other semantic nodes with the given annotations
    Derived {
        /// "Main" syntax nodes of derived semantic nodes
        primary_locs: BTreeSet<Ann<'tree>>,
        /// Other syntax nodes of derived semantic nodes
        other_locs: BTreeSet<Ann<'tree>>
    }
}

/// Type with an annotation
pub trait HasAnn<'tree> {
    /// Annotation (reference)
    fn ann(&self) -> &Ann<'tree>;
    /// Annotation (mutable reference)
    fn ann_mut(&mut self) -> &mut Ann<'tree>;
}

impl<'tree> Ann<'tree> {
    /// Source location(s) AKA syntax nodes this semantic node was created from
    #[auto_enum]
    pub fn locs(&self) -> impl Iterator<Item=Node<'tree>> {
        match self {
            Self::Intrinsic => empty(),
            Self::DirectSource { loc } => once(loc),
            Self::ImplicitSource { before_loc, after_loc } => once(before_loc).chain(once(after_loc)),
            Self::InferredSource { primary_loc, other_locs } => once(primary_loc).chain(other_locs.iter()),
            Self::Derived { primary_locs, other_locs } => primary_locs.iter().chain(other_locs.iter())
        }
    }

    /// Primary source location AKA syntax node this was created from. Some nodes may be created
    /// from multiple nodes
    #[inline]
    pub fn primary_loc(&self) -> Option<Node<'tree>> {
        self.locs().next()
    }

    /// File path the semantic node was created from (path of its primary location). Very complex
    /// derived nodes may be created from multiple paths.
    #[inline]
    pub fn primary_path(&self) -> Option<&'tree Path> {
        self.primary_loc().and_then(|n| n.path())
    }

    /// Reported source location range
    pub fn reported_range(&self) -> Option<Range> {
        match self {
            Self::ImplicitSource { after_loc, .. } => {
                let after_loc_range = after_loc.range();
                Some(Range::new(
                    after_loc_range.start_byte(),
                    after_loc_range.start_byte(),
                    after_loc_range.start_point(),
                    after_loc_range.start_point(),
                ))
            },
            _ => self.range()
        }
    }

    /// Source location range. The smallest range containing all source nodes, except if this node
    /// was created multiple paths, the range only contains nodes in the primary path.
    ///
    /// This is used for ordering and error reporting.
    pub fn range(&self) -> Option<Range> {
        let primary_path = self.primary_path();
        self.locs()
            .filter(|n| n.path() == primary_path)
            .map(|n| n.range())
            .reduce(|a, b| a | b)
    }
}

impl<'tree> PartialOrd for Ann<'tree> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'tree> Ord for Ann<'tree> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.primary_path().cmp(&other.primary_path())
            .then(self.range().cmp(&other.range()))
    }
}

#[macro_export]
macro_rules! impl_has_ann_as_ref {
    ($Name:ident) => {
        impl<'tree, T: $crate::ast::ann::HasAnn<'tree>> $crate::ast::ann::HasAnn<'tree> for $Name<T> {
            fn ann(&self) -> &$crate::ast::ann::Ann<'tree> {
                self.as_ref().ann()
            }
            fn ann_mut(&mut self) -> &mut $crate::ast::ann::Ann<'tree> {
                self.as_mut().ann_mut()
            }
        }
    };
}

#[macro_export]
macro_rules! impl_has_ann_wrapper_struct {
    ($Name:ident) => {
        impl<'tree> $crate::ast::ann::HasAnn<'tree> for $Name<'tree> {
            fn ann(&self) -> &$crate::ast::ann::Ann<'tree> {
                self.0.ann()
            }
            fn ann_mut(&mut self) -> &mut $crate::ast::ann::Ann<'tree> {
                self.0.ann_mut()
            }
        }
    };
    ($Name:ident by $field:ident) => {
        impl<'tree> $crate::ast::ann::HasAnn<'tree> for $Name<'tree> {
            fn ann(&self) -> &$crate::ast::ann::Ann<'tree> {
                self.$field.ann()
            }
            fn ann_mut(&mut self) -> &mut $crate::ast::ann::Ann<'tree> {
                self.$field.ann_mut()
            }
        }
    };
}

macro_rules! impl_has_ann_record_struct_body {
    ($tree:lifetime) => {
        fn ann(&self) -> &$crate::ast::ann::Ann<$tree> {
            &self.ann
        }
        fn ann_mut(&mut self) -> &mut $crate::ast::ann::Ann<$tree> {
            &self.ann
        }
    }
}

#[macro_export]
macro_rules! impl_has_ann_record_struct {
    ($Name:ident) => {
        impl<'tree> $crate::ast::ann::HasAnn<'tree> for $Name<'tree> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
    ($Name:ident<$T:ident>) => {
        impl<'tree, $T> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
    ($Name:ident<$T:ident: $Bound:ident>) => {
        impl<'tree, $T: $Bound<'tree>> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_record_struct_body!('tree);
        }
    };
}

#[macro_export]
macro_rules! impl_has_ann_enum_body {
    ($tree:lifetime $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        fn ann(&self) -> &$crate::ast::ann::Ann<$tree> {
            match self {
                $($(Self::$StructCase(x) => x.ann(),)*)?
                $($(Self::$RecordCase { ann, .. } => ann,)*)?
            }
        }
        fn ann_mut(&mut self) -> &mut $crate::ast::ann::Ann<$tree> {
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
        impl<'tree> $crate::ast::ann::HasAnn<'tree> for $Name<'tree> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident> $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident: $Bound:ident> $(($($StructCase:ident),*))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T: $Bound<'tree>> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($($StructCase),*))? $({ $($RecordCase),* })?);
        }
    };
}

impl_has_ann_as_ref!(Box);
impl_has_ann_as_ref!(IdentityRef);

impl<'tree> HasAnn<'tree> for Ann<'tree> {
    fn ann(&self) -> &Ann {
        self
    }
    fn ann_mut(&mut self) -> &mut Ann {
        self
    }
}