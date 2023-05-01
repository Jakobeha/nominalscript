use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

use replace_with::replace_with_or_default;

use crate::ast::ann::{Ann, HasAnn};
use crate::ast::tree_sitter::TSNode;

/// A TypeScript type (not used by nominalscript)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypescriptType<'tree>(TSNode<'tree>);

/// A statement block (not used by nominalscript)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatementBlock<'tree>(TSNode<'tree>);

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

impl<'tree, Type: TypeTrait<'tree>> IdentType<'tree, Type> {
    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> IdentType<'tree, NewType> {
        IdentType {
            ann: self.ann,
            name: self.name,
            generic_args: self.generic_args.into_iter().map(|arg| <NewType as TypeTraitMapsFrom<'tree, Type>>::map_type_arg(arg, &mut f)).collect()
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> IdentType<'tree, NewType> {
        IdentType {
            ann: self.ann,
            name: self.name.clone(),
            generic_args: self.generic_args.iter().map(|arg| <NewType as TypeTraitMapsFrom<'tree, Type>>::map_ref_type_arg(arg, &mut f)).collect(),
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> TypeParam<'tree, Type> {
    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, f: impl FnMut(Type) -> NewType) -> TypeParam<'tree, NewType> {
        TypeParam {
            ann: self.ann,
            variance_bound: self.variance_bound,
            name: self.name,
            supers: <NewType as TypeTraitMapsFrom<'tree, Type>>::map_inherited(self.supers, f),
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> TypeParam<'tree, NewType> {
        TypeParam {
            ann: self.ann,
            variance_bound: self.variance_bound,
            name: self.name.clone(),
            supers: <NewType as TypeTraitMapsFrom<'tree, Type>>::map_ref_inherited(&self.supers, |x| f(&x)),
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> StructureType<'tree, Type> {
    /// Get the structure-kind AKA enum variant.
    ///
    /// This is different than the formal definition of "kind" as in "higher-kinded types".
    pub fn kind(&self) -> StructureKind {
        match self {
            StructureType::Fn { .. } => StructureKind::Fn,
            StructureType::Array { .. } => StructureKind::Array,
            StructureType::Tuple { .. } => StructureKind::Tuple,
            StructureType::Object { .. } => StructureKind::Object,
        }
    }

    /// If this is an array, returns the element type. Otherwise `None`
    pub fn into_array_element_type(self) -> Option<Type> {
        match self {
            StructureType::Array { element_type, .. } => Some(*element_type),
            _ => None,
        }
    }

    /// If this is a tuple, returns the element types. Otherwise `None`
    pub fn into_tuple_element_types(self) -> Option<Vec<OptionalType<'tree, Type>>> {
        match self {
            StructureType::Tuple { element_types, ..  } => Some(element_types),
            _ => None,
        }
    }

    /// If this is an object, returns the field types. Otherwise `None`
    pub fn into_object_field_types(self) -> Option<Vec<Field<'tree, OptionalType<'tree, Type>>>> {
        match self {
            StructureType::Object { field_types, .. } => Some(field_types),
            _ => None,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> StructureType<'tree, NewType> {
        match self {
            StructureType::Fn(fn_type) => {
                StructureType::Fn(Box::new(fn_type.map(f)))
            },
            StructureType::Array { ann, element_type } => StructureType::Array {
                ann,
                element_type: Box::new(f(*element_type)),
            },
            StructureType::Tuple { ann, element_types } => StructureType::Tuple {
                ann,
                element_types: element_types.into_iter().map(|elem| elem.map(&mut f)).collect(),
            },
            StructureType::Object { ann, field_types } => StructureType::Object {
                ann,
                field_types: field_types.into_iter().map(|field| field.map(|opt| opt.map(&mut f))).collect(),
            },
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> StructureType<'tree, NewType> {
        match self {
            StructureType::Fn(fn_type) => {
                StructureType::Fn(Box::new(fn_type.map_ref(f)))
            },
            StructureType::Array { ann, element_type } => StructureType::Array {
                ann: *ann,
                element_type: Box::new(f(element_type)),
            },
            StructureType::Tuple { ann, element_types } => StructureType::Tuple {
                ann: *ann,
                element_types: element_types.iter().map(|elem| elem.map_ref(&mut f)).collect(),
            },
            StructureType::Object { ann, field_types } => StructureType::Object {
                ann: *ann,
                field_types: field_types.iter().map(|field| field.map_ref(|opt| opt.map_ref(&mut f))).collect(),
            },
        }
    }
}

impl<'tree, Type: TypeTrait + Hash> Hash for StructureType<'tree, Type> where Type::RestArg: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            StructureType::Fn(fn_type) => {
                state.write_u8(0);
                fn_type.hash(state);
            }
            StructureType::Array { ann, element_type } => {
                state.write_u8(1);
                ann.hash(state);
                element_type.hash(state);
            }
            StructureType::Tuple { ann, element_types } => {
                state.write_u8(2);
                ann.hash(state);
                element_types.hash(state);
            }
            StructureType::Object { ann, field_types } => {
                state.write_u8(3);
                ann.hash(state);
                field_types.hash(state);
            }
        }
    }
}

impl<'tree, Type: TypeTrait<'tree>> FnType<'tree, Type> {
    pub fn new(
        ann: Ann<'tree>,
        type_params: impl IntoIterator<Item=TypeParam<'tree, Type>>,
        this_type: Type,
        arg_types: impl IntoIterator<Item=OptionalType<'tree, Type>>,
        rest_arg_type: Type::RestArg,
        return_type: ReturnType<'tree, Type>
    ) -> Self {
        FnType {
            ann,
            type_params: type_params.into_iter().collect(),
            this_type,
            arg_types: arg_types.into_iter().collect(),
            rest_arg_type,
            return_type,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<'tree, Type>>(self, mut f: impl FnMut(Type) -> NewType) -> FnType<'tree, NewType> {
        let type_params = self.type_params.into_iter().map(|param| param.map(&mut f)).collect();
        let this_type = f(self.this_type);
        let arg_types = self.arg_types.into_iter().map(|arg| arg.map(&mut f)).collect();
        let return_type = self.return_type.map(&mut f);
        NewType::map_rest_arg_type_in_fn(
            self.ann,
            type_params,
            this_type,
            arg_types,
            self.rest_arg_type,
            return_type,
            f
        )
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<'tree, Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> FnType<'tree, NewType> {
        let type_params = self.type_params.iter().map(|param| param.map_ref(&mut f)).collect();
        let this_type = f(&self.this_type);
        let arg_types = self.arg_types.iter().map(|arg| arg.map_ref(&mut f)).collect();
        let return_type = self.return_type.map_ref(&mut f);
        NewType::map_ref_rest_arg_type_in_fn(
            self.ann,
            type_params,
            this_type,
            arg_types,
            &self.rest_arg_type,
            return_type,
            |x| f(&x)
        )
    }

    pub fn with_return_type(self, return_type: ReturnType<'tree, Type>) -> Self {
        FnType {
            ann: self.ann,
            type_params: self.type_params,
            this_type: self.this_type,
            arg_types: self.arg_types,
            rest_arg_type: self.rest_arg_type,
            return_type,
        }
    }
}

impl<'tree, Type: TypeTrait + Hash> Hash for FnType<'tree, Type> where Type::RestArg: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Don't need to hash everything, and 2 functions will rarely differ in only type-params
        // (also requires more boilerplate...).
        // We should look for other cases to try and hash less without running into conflucts
        // self.type_params.hash(state);
        self.this_type.hash(state);
        self.arg_types.hash(state);
        self.rest_arg_type.hash(state);
        self.return_type.hash(state);
    }
}

impl<'tree, Type> OptionalType<'tree, Type> {
    pub fn new(ann: Ann<'tree>, type_: Type, is_optional: bool) -> Self {
        Self {
            ann,
            optionality: Optionality::from(is_optional),
            type_,
        }
    }

    pub fn required(ann: Ann<'tree>, type_: Type) -> Self {
        Self {
            ann,
            optionality: Optionality::Required,
            type_,
        }
    }

    pub fn optional(ann: Ann<'tree>, type_: Type) -> Self {
        Self {
            ann,
            optionality: Optionality::Required,
            type_,
        }
    }

    /// Like [HasNullability::make_nullable] but for optional types
    pub fn make_optional(&mut self) {
        self.optionality = Optionality::Optional;
    }

    /// Like [HasNullability::make_nullable_if] but for optional types
    pub fn make_optional_if(&mut self, condition: bool) {
        if condition {
            self.make_optional();
        }
    }

    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> OptionalType<'tree, NewType> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> OptionalType<'tree, NewType> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: f(&self.type_),
        }
    }

    pub fn as_ref(&self) -> OptionalType<&Type> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: &self.type_,
        }
    }
}

impl<'tree, Type: Clone> OptionalType<&Type> {
    pub fn cloned(self) -> OptionalType<'tree, Type> {
        OptionalType {
            ann: self.ann,
            optionality: self.optionality,
            type_: self.type_.clone(),
        }
    }
}

impl<'tree, Type: HasNullability> OptionalType<'tree, Type> {
    /// If optional, returns the inner type nullable. Otherwise just the inner type.
    /// Optionality is very similar to nullability, the difference is that you can completely omit
    /// providing optional-typed values at the end of an argument list or tuple
    pub fn collapse_optionality_into_nullability(mut self) -> Type {
        match self.optionality {
            Optionality::Required => {},
            Optionality::Optional => { self.type_.make_nullable(); },
        }
        self.type_.ann_mut() = self.ann;
        self.type_
    }
}

impl<'tree, Type> ReturnType<'tree, Type> {
    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> ReturnType<'tree, NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> ReturnType<'tree, NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }
}

impl<'tree, Type> From<Type> for ReturnType<'tree, Type> {
    fn from(value: Type) -> Self {
        ReturnType::Type(value)
    }
}

impl<'tree, Type> Field<Type> {
    pub fn map<'tree, NewType>(self, f: impl FnOnce(Type) -> NewType) -> Field<'tree, NewType> {
        Field {
            ann: self.ann,
            name: self.name,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<'tree, NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> Field<'tree, NewType> {
        Field {
            ann: self.ann,
            name: self.name.clone(),
            type_: f(&self.type_),
        }
    }
}

impl<'tree, Type: Default> Default for ReturnType<'tree, Type> {
    fn default() -> Self {
        Self::Type(Default::default())
    }
}

impl Variance {
    pub fn new(is_covariant: bool, is_contravariant: bool) -> Self {
        match (is_covariant, is_contravariant) {
            (true, true) => Variance::Bivariant,
            (true, false) => Variance::Covariant,
            (false, true) => Variance::Contravariant,
            (false, false) => Variance::Invariant,
        }
    }

    pub fn reversed(self) -> Self {
        match self {
            Variance::Bivariant => Variance::Bivariant,
            Variance::Covariant => Variance::Contravariant,
            Variance::Contravariant => Variance::Covariant,
            Variance::Invariant => Variance::Invariant,
        }
    }

    pub fn is_covariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => true,
            Variance::Contravariant => false,
            Variance::Invariant => false,
        }
    }

    pub fn is_contravariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => false,
            Variance::Contravariant => true,
            Variance::Invariant => false,
        }
    }

    pub fn do_union(self) -> bool {
        self.is_covariant()
    }

    /// Applies variance bound `bound` onto `self`.
    /// TODO: Also check for variance bounds which are less restricted than the actual inferred
    ///   variance, but in another place
    pub fn bounded(self, bound: Variance) -> Self {
        match bound {
            Variance::Bivariant => Variance::Bivariant,
            Variance::Covariant => self,
            Variance::Contravariant => self.reversed(),
            Variance::Invariant => Variance::Invariant
        }
    }
}

impl PartialOrd for Variance {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Variance::Invariant, Variance::Invariant) => Some(Ordering::Equal),
            (Variance::Invariant, _) => Some(Ordering::Less),
            (_, Variance::Invariant) => Some(Ordering::Greater),
            (Variance::Bivariant, Variance::Bivariant) => Some(Ordering::Equal),
            (Variance::Bivariant, _) => Some(Ordering::Greater),
            (_, Variance::Bivariant) => Some(Ordering::Less),
            (Variance::Covariant, Variance::Covariant) => Some(Ordering::Equal),
            (Variance::Covariant, Variance::Contravariant) => None,
            (Variance::Contravariant, Variance::Contravariant) => Some(Ordering::Equal),
            (Variance::Contravariant, Variance::Covariant) => None,
        }
    }
}

impl BitOr for Variance {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Invariant, other) => other,
            (this, Variance::Invariant) => this,
            (Variance::Bivariant, _) => Variance::Bivariant,
            (_, Variance::Bivariant) => Variance::Bivariant,
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            (Variance::Covariant, Variance::Contravariant) => Variance::Bivariant,
            (Variance::Contravariant, Variance::Covariant) => Variance::Bivariant,
        }
    }
}

impl BitOrAssign for Variance {
    fn bitor_assign(&mut self, other: Self) {
        *self = *self | other;
    }
}

impl BitAnd for Variance {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (Variance::Invariant, _) => Variance::Invariant,
            (_, Variance::Invariant) => Variance::Invariant,
            (Variance::Bivariant, other) => other,
            (this, Variance::Bivariant) => this,
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Contravariant,
            (Variance::Covariant, Variance::Contravariant) => Variance::Invariant,
            (Variance::Contravariant, Variance::Covariant) => Variance::Invariant,
        }
    }
}

impl BitAndAssign for Variance {
    fn bitand_assign(&mut self, other: Self) {
        *self = *self & other;
    }
}

impl StructureKind {
    pub fn display(self) -> &'static str {
        match self {
            StructureKind::Fn => "function",
            StructureKind::Array => "array",
            StructureKind::Tuple => "tuple",
            StructureKind::Object => "object",
        }
    }

    pub fn a_display(self) -> &'static str {
        match self {
            StructureKind::Fn => "a function",
            StructureKind::Array => "an array",
            StructureKind::Tuple => "a tuple",
            StructureKind::Object => "an object",
        }
    }
}

impl From<bool> for Optionality {
    fn from(is_optional: bool) -> Self {
        if is_optional {
            Self::Optional
        } else {
            Self::Required
        }
    }
}

impl BitOr for Optionality {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Optionality::Required, Optionality::Required) => Optionality::Required,
            _ => Optionality::Optional,
        }
    }
}

impl BitOrAssign for Optionality {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Optionality {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Optionality::Optional, Optionality::Optional) => Optionality::Optional,
            _ => Optionality::Required,
        }
    }
}

impl BitAndAssign for Optionality {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl From<bool> for Nullability {
    fn from(is_nullable: bool) -> Self {
        if is_nullable {
            Self::Nullable
        } else {
            Self::NonNullable
        }
    }
}

impl BitOr for Nullability {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Nullability::NonNullable, Nullability::NonNullable) => Nullability::NonNullable,
            _ => Nullability::Nullable,
        }
    }
}

impl BitOrAssign for Nullability {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAnd for Nullability {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Nullability::Nullable, Nullability::Nullable) => Nullability::Nullable,
            _ => Nullability::NonNullable,
        }
    }
}

impl BitAndAssign for Nullability {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl<F: FnOnce() -> Nullability> BitOr<F> for Nullability {
    type Output = Self;

    fn bitor(self, rhs: F) -> Self::Output {
        match self {
            Nullability::NonNullable => Nullability::NonNullable,
            Nullability::Nullable => rhs(),
        }
    }
}