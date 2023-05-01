use crate::ast::tree_sitter::{TSNode, TSRange};

/// Annotation = source info
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ann<'tree> {
    /// The annotated data came from source
    DirectSource { loc: TSNode<'tree> },
    /// The annotated data was inferred from source
    InferredSource { loc: TSNode<'tree> },
    #[default]
    /// The annotated data is a global constant
    Intrinsic,
}

/// Type with an annotation
pub trait HasAnn<'tree> {
    /// Annotation (reference)
    fn ann(&self) -> &Ann<'tree>;
    /// Annotation (mutable reference)
    fn ann_mut(&mut self) -> &mut Ann<'tree>;
}

impl<'tree> Ann<'tree> {
    /// Source location
    pub fn loc(&self) -> Option<TSNode<'tree>> {
        match self {
            Self::DirectSource { loc } | Self::InferredSource { loc } => Some(*loc),
            Self::Intrinsic => None,
        }
    }

    /// Source location range
    pub fn range(&self) -> Option<TSRange> {
        self.loc().map(|l| l.range())
    }
}

impl<'tree> PartialOrd for Ann<'tree> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.range().partial_cmp(&other.range())
    }
}

impl<'tree> Ord for Ann<'tree> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.range().cmp(&other.range())
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
    ($tree:lifetime $(($StructCase:ident))? $({ $($RecordCase:ident),* })?) => {
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
    ($Name:ident $(($StructCase:ident))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree> $crate::ast::ann::HasAnn<'tree> for $Name<'tree> {
            $crate::impl_has_ann_enum_body!('tree $(($StructCase))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident> $(($StructCase:ident))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($StructCase))? $({ $($RecordCase),* })?);
        }
    };
    ($Name:ident<$T:ident: $Bound:ident> $(($StructCase:ident))? $({ $($RecordCase:ident),* })?) => {
        impl<'tree, $T: $Bound<'tree>> $crate::ast::ann::HasAnn<'tree> for $Name<'tree, $T> {
            $crate::impl_has_ann_enum_body!('tree $(($StructCase))? $({ $($RecordCase),* })?);
        }
    };
}

impl_has_ann_as_ref!(Box);

impl<'tree> HasAnn<'tree> for Ann<'tree> {
    fn ann(&self) -> &Ann {
        self
    }
    fn ann_mut(&mut self) -> &mut Ann {
        self
    }
}