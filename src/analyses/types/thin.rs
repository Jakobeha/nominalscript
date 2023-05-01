use std::fmt::Debug;
use std::hash::Hash;

use replace_with::replace_with_or_default;

use crate::{impl_has_ann_enum, impl_has_ann_record_struct};
use crate::analyses::bindings::TypeIdent;
use crate::analyses::types::generic::{HasNullability, IdentType, impl_structural_type_constructors, InheritedTrait, NominalGuard, Nullability, RestArgTrait, StructureType, TypeArgTrait, TypeParam, TypescriptType, TypeTrait};
use crate::ast::ann::Ann;

/// Type declaration before we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct ThinTypeDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: TypeIdent<'tree>,
    pub type_params: Vec<TypeParam<'tree, ThinType<'tree>>>,
    pub supertypes: Vec<ThinType<'tree>>,
    pub typescript_supertype: Vec<TypescriptType<'tree>>,
    pub guard: Vec<NominalGuard<'tree>>,
}
impl_has_ann_record_struct!(ThinTypeDecl);

/// Thin type = type reference which is parsed from a string or AST node
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ThinType<'tree> {
    #[default]
    Any { ann: Ann<'tree> },
    Never {
        ann: Ann<'tree>,
        nullability: Nullability
    },
    Structural {
        ann: Ann<'tree>,
        nullability: Nullability,
        structure: StructureType<'tree, ThinType<'tree>>
    },
    Nominal {
        ann: Ann<'tree>,
        nullability: Nullability,
        id: IdentType<'tree, ThinType<'tree>>
    },
    IllegalVoid {
        ann: Ann<'tree>
    }
}
impl_has_ann_enum!(ThinType { Any, Never, Structural, Nominal, IllegalVoid });

/// Formal parameter's type, which is different from [ThinType] because it sometimes it's inferred
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InferrableThinType<'tree> {
    /// Explicitly provided type
    Explicit(ThinType<'tree>),
    /// Backwards-inferred hole
    Hole { ann: Ann<'tree> },
}
impl_has_ann_enum!(InferrableThinType (Explicit) { Hole });

impl<'tree> ThinTypeDecl<'tree> {
    pub const MISSING: Self = Self::missing(Ann::Intrinsic);

    pub const fn missing(ann: Ann<'tree>) -> Self {
        Self {
            ann,
            name: TypeIdent::<'tree>::MISSING,
            type_params: Vec::new(),
            supertypes: Vec::new(),
            typescript_supertype: Vec::new(),
            guard: Vec::new()
        }
    }

    /// Converts this into the nominal type it declares: for [ThinType], just a nominal type with
    /// the name and type parameters
    pub fn into_type(self) -> ThinType<'tree> {
        ThinType::Nominal {
            ann: self.ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann: self.ann,
                name: self.name,
                generic_args: self.type_params.into_iter().map(|x| x.into_type()).collect(),
            }
        }
    }
}

impl<'tree> ThinType<'tree> {
    pub const NEVER: Self = Self::never(Ann::Intrinsic);

    pub const NULL: Self = Self::null(Ann::Intrinsic);

    pub const fn never(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::NonNullable }
    }

    pub const fn null(ann: Ann<'tree>) -> Self {
        Self::Never { ann, nullability: Nullability::Nullable }
    }

    pub fn dummy_for_hole(ann: Ann<'tree>, nullability: Nullability) -> Self {
        Self::Nominal {
            ann,
            nullability,
            id: IdentType {
                ann,
                name: TypeIdent::<'tree>::DUMMY_FOR_HOLE,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn ident(ann: Ann<'tree>, name: TypeIdent<'tree>) -> Self {
        Self::Nominal {
            ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann,
                name,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn generic(ann: Ann<'tree>, name: TypeIdent<'tree>, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::Nominal {
            ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann,
                name,
                generic_args: generic_args.collect(),
            }
        }
    }

    pub fn ident2(ann: Ann<'tree>, name: &str) -> Self {
        match name {
            "Any" => Self::Any { ann },
            "Never" => Self::never(ann),
            "Null" => Self::null(ann),
            "Void" => Self::IllegalVoid { ann },
            _ => Self::ident(ann, TypeIdent::of(ann, name))
        }
    }

    pub fn generic2(ann: Ann<'tree>, name: &str, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::generic(ann, TypeIdent::of(ann, name), generic_args)
    }

    impl_structural_type_constructors!('tree);
}

impl<'tree> HasNullability for ThinType<'tree> {
    fn nullability(&self) -> Nullability {
        match self {
            Self::Any { .. } => Nullability::Nullable,
            Self::Never { nullability, .. } => *nullability,
            Self::Structural { nullability, .. } => *nullability,
            Self::Nominal { nullability, .. } => *nullability,
            Self::IllegalVoid { .. } => Nullability::NonNullable,
        }
    }

    fn make_nullable(&mut self) {
        match self {
            Self::Any { .. } => {},
            Self::Never { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::IllegalVoid { .. } => {},
        }
    }
}

impl<'tree> TypeTrait<'tree> for ThinType<'tree> {
    type Inherited = Vec<ThinType<'tree>>;
    type TypeArg = ThinType<'tree>;
    type RestArg = ThinType<'tree>;

    fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }

    fn is_never(&self) -> bool {
        matches!(self, Self::Never { nullability: Nullability::NonNullable })
    }

    fn map_inherited(
        mut inherited: Self::Inherited,
        mut f: impl FnMut(Self) -> Self
    ) -> Self::Inherited {
        for inherited in inherited.iter_mut() {
            replace_with_or_default(inherited, &mut f);
        }
        inherited
    }

    fn map_ref_inherited(
        inherited: &Self::Inherited,
        f: impl FnMut(&Self) -> Self,
    ) -> Self::Inherited {
        inherited.iter().map(f).collect()
    }

    fn map_type_arg(type_arg: Self::TypeArg, mut f: impl FnMut(Self) -> Self) -> Self::TypeArg where Self: Sized {
        f(type_arg)
    }

    fn map_ref_type_arg(type_arg: &Self::TypeArg, mut f: impl FnMut(&Self) -> Self) -> Self::TypeArg {
        f(type_arg)
    }

    fn map_rest_arg_type(rest_arg_type: Self::RestArg, mut f: impl FnMut(Self) -> Self) -> Self::RestArg {
        f(rest_arg_type)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArg, mut f: impl FnMut(&Self) -> Self) -> Self::RestArg {
        f(rest_arg_type)
    }
}

impl<'tree> InheritedTrait<'tree> for Vec<ThinType<'tree>> {
    fn is_empty_inherited(&self) -> bool {
        self.is_empty()
    }
}

impl<'tree> TypeArgTrait<'tree> for ThinType<'tree> {
    type Type = ThinType<'tree>;

    fn type_(&self) -> &Self::Type {
        self
    }

    fn type_mut(&mut self) -> &mut Self::Type {
        self
    }

    fn into_type(self) -> Self::Type where Self::Type: Sized {
        self
    }
}

impl<'tree> RestArgTrait<'tree> for ThinType<'tree> {
    fn is_empty_rest_arg(&self) -> bool {
        matches!(self, Self::Structural {
            nullability: Nullability::NonNullable,
            structure: StructureType::Tuple { element_types }
        } if element_types.is_empty())
    }
}

impl<'tree> TypeParam<ThinType<'tree>> {
    /// Converts this into the nominal type it declares: for [ThinType], just a nominal type with
    /// the name
    pub fn into_type(self) -> ThinType<'tree> {
        ThinType::Nominal {
            ann: self.ann,
            nullability: Nullability::NonNullable,
            id: IdentType {
                ann: self.ann,
                name: self.name,
                // No HKTs
                generic_args: Vec::new()
            }
        }
    }
}

impl<'tree> InferrableThinType<'tree> {
    /// Is this a hole AKA is this backwards-inferred?
    pub fn is_hole(&self) -> bool {
        matches!(self, Self::Hole { .. })
    }

    /// If this is a hole it collapses into `Never`, otherwise the explicit type is returned.
    pub fn collapse_contravariant(self) -> ThinType<'tree> {
        match self {
            Self::Explicit(type_) => type_,
            Self::Hole { ann, .. } => ThinType::never(ann),
        }
    }
}

impl<'tree> From<ThinType<'tree>> for InferrableThinType<'tree> {
    fn from(type_: ThinType<'tree>) -> Self {
        Self::Explicit(type_)
    }
}