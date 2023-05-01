use std::borrow::Cow;
use std::fmt::Debug;
use std::hash::Hash;

use replace_with::replace_with_or_default;

use crate::ast::ann::{Ann, HasAnn};
use crate::ast::tree_sitter::TSNode;

/// A TypeScript type (not used by nominalscript)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypescriptType<'tree>(TSNode<'tree>);

/// A nominal guard is a special function which is called after a nominal wrap expression,
/// and checks that the value is of the correct type.
/// If it returns false, the program will throw a TypeError.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NominalGuard<'tree> {
    pub ann: Ann<'tree>,
    /// The parameter binding
    pub param: ValueIdent<'tree>,
    /// The body of the guard.
    pub body: StatementBlock<'tree>
}
impl_has_ann_record_struct!(NominalGuard);

/// An identifier and its generic args
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentType<'tree, Type: TypeTrait<'tree>> {
    pub ann: Ann<'tree>,
    /// Name
    pub name: TypeIdent<'tree>,
    /// Generic args
    // Remember: these don't need to be boxed because they are in a vec
    pub generic_args: Vec<Type::TypeArg>,
}
impl_has_ann_record_struct!(IdentType<Type: TypeTrait>);

/// A generic parameter (= def in type scope)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam<'tree, Type: TypeTrait<'tree>> {
    pub ann: Ann<'tree>,
    /// Note that the actual variance is the intersection of this and the inferred variance.
    /// Furthermore, if this is outside of the actual variance the compiler will log an error
    pub variance_bound: Variance,
    /// Parameter name
    pub name: TypeIdent<'tree>,
    // Remember: these don't need to be boxed because they are in a vec
    /// Any instantiation of this parameter must inherit these
    pub supers: Type::Inherited,
}
impl_has_ann_record_struct!(TypeParam<Type: TypeTrait>);

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum Variance {
    /// `lhs` must be equivalent to `rhs`, i.e. `lhs` and `rhs` must be subtypes of each other
    Invariant,
    /// `lhs` must be a subtype of `rhs`
    Covariant,
    /// `lhs` must be a supertype of `rhs`
    Contravariant,
    /// `lhs` and `rhs` must be non-disjoint, *or* either one can be `Never`. i.e. if `lhs` and
    /// `rhs` are inhabited (there exists at least one instance of each) `lhs & rhs` must be
    /// inhabited (there exists at least one value which is an instance of both). This is relevant
    /// for downcast (nominal wrap), since we 100% know a value will fail to downcast if the types
    /// are disjoint, but if `lhs` is uninhabited we know the code will never be reached, and if
    /// `rhs` is inhabited we don't care.
    #[default]
    Bivariant,
}

/// The JavaScript structure of instances of the type, if it is a compound type
/// (primitive types have identifiers).
///
/// All inner types are boxed to reduce size and allow recursive definitions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructureType<'tree, Type: TypeTrait<'tree>> {
    Fn(
        /// Function type. Boxed because it's large
        Box<FnType<'tree, Type>>,
    ),
    Array {
        ann: Ann<'tree>,
        /// The type of the elements
        element_type: Box<Type>,
    },
    Tuple {
        ann: Ann<'tree>,
        /// The types of the elements
        element_types: Vec<OptionalType<'tree, Type>>,
    },
    Object {
        ann: Ann<'tree>,
        /// The type of the fields
        field_types: Vec<Field<'tree, OptionalType<'tree, Type>>>,
    }
}
impl_has_ann_enum!(StructureType<Type: TypeTrait> (Fn) { Array, Tuple, Object });

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructureKind {
    Fn,
    Array,
    Tuple,
    Object,
}

/// Function type structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType<'tree, Type: TypeTrait<'tree>> {
    pub ann: Ann<'tree>,
    /// Type parameters if polymorphic
    pub type_params: Vec<TypeParam<'tree, Type>>,
    /// The type of `this` in the function
    pub this_type: Type,
    /// The types of the arguments
    pub arg_types: Vec<OptionalType<'tree, Type>>,
    /// The type of the rest argument, pass `[]` for no rest argument
    pub rest_arg_type: Type::RestArg,
    /// The type of the return value
    pub return_type: ReturnType<'tree, Type>,
}
impl_has_ann_record_struct!(FnType<Type: TypeTrait>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Type of a function return. Note that `Void` is different (and a subtype) of `Type(Any)`,
/// which is the default.
pub enum ReturnType<'tree, Type> {
    Void { ann: Ann<'tree> },
    Type(Type),
}
impl_has_ann_enum!(ReturnType<Type: TypeTrait> (Type) { Void });

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field<'tree, Type> {
    pub ann: Ann<'tree>,
    pub name: FieldIdent<'tree>,
    pub type_: Type,
}
impl_has_ann_record_struct!(Field<Type>);

/// A type which may be optional
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OptionalType<'tree, Type> {
    pub ann: Ann<'tree>,
    pub optionality: Optionality,
    pub type_: Type
}
impl_has_ann_record_struct!(OptionalType<Type>);

/// A type which can be nullable or non-nullable.
///
/// `make_non_nullable` is not part of this trait, so a type which once made nullable can't
/// necessarily by made it non-nullable again can inherit.
pub trait HasNullability: Debug {
    /// Returns whether this instance is nullable or non-nullable.
    fn nullability(&self) -> Nullability;

    /// Make this instance nullable
    ///
    /// `make_non_nullable` is not part of this trait: once you make an instance nullable, you can't
    /// necessarily make it non-nullable again.
    fn make_nullable(&mut self);

    /// Make nullable if `nullable` is true.
    ///
    /// If `nullable` is false but this is already nullable it will still be nullable.
    fn make_nullable_if(&mut self, nullable: bool) {
        if nullable {
            self.make_nullable();
        }
    }

    /// Returns a nullable clone
    ///
    /// There's no `non_nullable(&self)` or `non_nullable_if(&self)` because type holes make that
    /// operation ambiguous
    fn nullable(&self) -> Self where Self: Clone {
        let mut clone = self.clone();
        clone.make_nullable();
        clone
    }

    /// Returns a nullable clone if `nullable` is true, otherwise a normal clone
    ///
    /// There's no `non_nullable(&self)` or `non_nullable_if(&self)` because type holes make that
    /// operation ambiguous
    fn nullable_if(&self, nullable: bool) -> Self where Self: Clone {
        if nullable {
            self.nullable()
        } else {
            self.clone()
        }
    }
}

/// Base type of [ThinType] and [FatType] which contains associated types that are different
/// between the 2.
pub trait TypeTrait<'tree>: HasAnn<'tree> + HasNullability + Default + Clone + PartialEq + Eq {
    type Inherited: InheritedTrait<'tree>;
    type TypeArg: TypeArgTrait<'tree, Type = Self>;
    type RestArg: RestArgTrait<'tree>;

    /// Is 'Any' AKA top
    fn is_any(&self) -> bool;
    /// Is 'Never' AKA bottom
    fn is_never(&self) -> bool;
    fn map_inherited(inherited: Self::Inherited, f: impl FnMut(Self) -> Self) -> Self::Inherited where Self: Sized;
    fn map_ref_inherited(inherited: &Self::Inherited, f: impl FnMut(&Self) -> Self) -> Self::Inherited;
    fn map_type_arg(mut type_arg: Self::TypeArg, f: impl FnMut(Self) -> Self) -> Self::TypeArg where Self: Sized {
        replace_with_or_default(type_arg.type_mut(), f);
        type_arg
    }
    fn map_ref_type_arg(type_arg: &Self::TypeArg, f: impl FnMut(&Self) -> Self) -> Self::TypeArg;
    fn map_rest_arg_type(rest_arg_type: Self::RestArg, f: impl FnMut(Self) -> Self) -> Self::RestArg where Self: Sized;
    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArg, f: impl FnMut(&Self) -> Self) -> Self::RestArg;
}

/// Inherited (supertypes) of a type
pub trait InheritedTrait<'tree>: Debug + Default + Clone + PartialEq + Eq {
    /// Is this empty inherited (AKA Any, AKA inherits nothing)
    fn is_empty_inherited(&self) -> bool;
}

/// A type argument AKA instantiated type parameter
pub trait TypeArgTrait<'tree>: HasAnn<'tree> + Debug + Default + Clone + PartialEq + Eq {
    type Type: TypeTrait + ?Sized;

    /// Reference to the type this argument is instantiated with (what this wraps with extra info)
    fn type_(&self) -> &Self::Type;
    /// Mutable reference to the type this argument is instantiated with (what this wraps with extra info)
    fn type_mut(&mut self) -> &mut Self::Type;
    /// Destruct into the type this argument is instantiated with (what this wraps with extra info)
    fn into_type(self) -> Self::Type where Self::Type: Sized;
}

/// The "rest arg" of a function type
pub trait RestArgTrait<'tree>: Debug + Clone + PartialEq + Eq {
    /// Is the empty rest arg (AKA no arguments after the explicit ones)
    fn is_empty_rest_arg(&self) -> bool;
}

/// Maps the associated types of [TypeTrait] from the `OldType` into ours.
///
/// This may be a bad abstraction, since it's only used for conversion between [ThinType] and
/// [FatType]. The [FatType]s provided to the function in its implementation are bare, and can
/// only be converted into [ThinType]s
pub trait TypeTraitMapsFrom<'tree, OldType: TypeTrait<'tree>>: TypeTrait<'tree> {
    fn map_inherited(inherited: OldType::Inherited, f: impl FnMut(OldType) -> Self) -> Self::Inherited;
    fn map_ref_inherited(inherited: &OldType::Inherited, f: impl FnMut(Cow<'_, OldType>) -> Self) -> Self::Inherited;
    fn map_type_arg(type_arg: OldType::TypeArg, f: impl FnMut(OldType) -> Self) -> Self::TypeArg;
    fn map_ref_type_arg(type_arg: &OldType::TypeArg, f: impl FnMut(&OldType) -> Self) -> Self::TypeArg;
    fn map_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: OldType::RestArg,
        return_type: ReturnType<'tree, Self>,
        f: impl FnMut(OldType) -> Self
    ) -> FnType<'tree, Self> where Self: Sized;
    fn map_ref_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: &OldType::RestArg,
        return_type: ReturnType<'tree, Self>,
        f: impl FnMut(Cow<'_, OldType>) -> Self
    ) -> FnType<'tree, Self> where Self: Sized;
}

/// Whether or not a type permits `null`.
///
/// **Important:** A *nullable* type is different from an *optional* type:
/// optional types exist in function arguments, tuple elems, and object fields, and specify that
/// the given argument elem or field may completely not exist. Non-optional nullable types must
/// exist but can be `null`. All optional types are nullable but not vice versa
///
/// This type implements `Ord` according to the type lattice: `Nullable` < `NonNullable` because all
/// instances of a nullable type are instances of the corresponding non-nullable type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Nullability {
    Nullable,
    NonNullable,
}

/// Whether or not the place with this type (function argument, tuple elem, field) can be completely absent.
///
/// **Important:** A *nullable* type is different from an *optional* type:
/// optional types exist in function arguments, tuple elems, and object fields, and specify that
/// the given argument elem or field may completely not exist. Non-optional nullable types must
/// exist but can be `null`. All optional types are nullable but not vice versa
///
/// This type implements `Ord` according to the type lattice: `Optional` < `Required` because all
/// instances of a optional type are instances of the corresponding required type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Optionality {
    Optional,
    Required,
}

macro_rules! impl_structural_type_constructors {
    ($tree:lifetime) => {
    pub const EMPTY_REST_ARG: Self = Self::empty_rest_arg($crate::ast::ann::Ann::Intrinsic);

    pub const fn empty_rest_arg(ann: $crate::ast::ann::Ann<$tree>) -> Self {
        Self::Structural {
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Tuple {
                element_types: Vec::new(),
            }
        }
    }

    pub fn func(
        ann: $crate::ast::ann::Ann<$tree>,
        type_params: impl IntoIterator<Item=$crate::analyses::types::TypeParam<Self>>,
        this_type: Self,
        arg_types: impl IntoIterator<Item=$crate::analyses::types::OptionalType<Self>>,
        rest_arg_type: <Self as $crate::analyses::types::TypeTrait>::RestArg,
        return_type: $crate::analyses::types::ReturnType<Self>
    ) -> Self {
        Self::Structural {
            ann,
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Fn(
                Box::new($crate::analyses::types::FnType::new(
                    ann,
                    type_params,
                    this_type,
                    arg_types,
                    rest_arg_type,
                    return_type
                ))
            )
        }
    }

    pub fn array(ann: $crate::ast::ann::Ann<$tree>, element_type: Self) -> Self {
        Self::Structural {
            ann,
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Array {
                ann,
                element_type: Box::new(element_type),
            }
        }
    }

    pub fn tuple(ann: $crate::ast::ann::Ann<$tree>, element_types: impl IntoIterator<Item=$crate::analyses::types::OptionalType<Self>>) -> Self {
        Self::Structural {
            ann,
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Tuple {
                ann,
                element_types: element_types.into_iter().collect(),
            }
        }
    }

    pub fn object(ann: $crate::ast::ann::Ann<$tree>, property_types: impl IntoIterator<Item=$crate::analyses::types::Field<$crate::analyses::types::OptionalType<Self>>>) -> Self {
        Self::Structural {
            ann,
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Object {
                ann,
                field_types: property_types.into_iter().collect(),
            }
        }
    }
    }
}
pub(crate) use impl_structural_type_constructors;
use crate::{impl_has_ann_enum, impl_has_ann_record_struct};
use crate::analyses::bindings::{FieldIdent, TypeIdent, ValueIdent};

impl<'tree, T: InheritedTrait<'tree>> InheritedTrait<'tree> for Box<T> {
    fn is_empty_inherited(&self) -> bool {
        self.as_ref().is_empty_inherited()
    }
}

impl<'tree, T: TypeTrait<'tree>> TypeTraitMapsFrom<'tree, T> for T {
    fn map_inherited(inherited: T::Inherited, f: impl FnMut(T) -> Self) -> Self::Inherited {
        Self::map_inherited(inherited, f)
    }

    fn map_ref_inherited(inherited: &T::Inherited, mut f: impl FnMut(Cow<'_, T>) -> Self) -> Self::Inherited {
        Self::map_ref_inherited(inherited, |x| f(Cow::Borrowed(x)))
    }

    fn map_type_arg(type_arg: T::TypeArg, f: impl FnMut(T) -> Self) -> Self::TypeArg {
        Self::map_type_arg(type_arg, f)
    }

    fn map_ref_type_arg(type_arg: &T::TypeArg, f: impl FnMut(&T) -> Self) -> Self::TypeArg {
        Self::map_ref_type_arg(type_arg, f)
    }

    fn map_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: T::RestArg,
        return_type: ReturnType<'tree, Self>,
        f: impl FnMut(T) -> Self
    ) -> FnType<'tree, Self> {
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type: Self::map_rest_arg_type(rest_arg_type, f),
            return_type,
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        ann: Ann<'tree>,
        type_params: Vec<TypeParam<'tree, Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<'tree, Self>>,
        rest_arg_type: &T::RestArg,
        return_type: ReturnType<'tree, Self>,
        mut f: impl FnMut(Cow<'_, T>) -> Self
    ) -> FnType<Self> {
        FnType {
            ann,
            type_params,
            this_type,
            arg_types,
            rest_arg_type: Self::map_ref_rest_arg_type(rest_arg_type, |x| f(Cow::Borrowed(x))),
            return_type,
        }
    }
}

