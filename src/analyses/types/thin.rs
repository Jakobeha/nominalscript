use std::borrow::Cow;
use std::cmp::Ordering;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};
use replace_with::replace_with_or_default;
use smol_str::SmolStr;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::ast::tree_sitter::SubTree;

/// Type declaration before we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct ThinTypeDecl {
    pub name: TypeName,
    pub type_params: Vec<TypeParam<ThinType>>,
    pub supertypes: Vec<ThinType>,
    pub typescript_supertype: Option<SubTree>,
    pub guard: Option<NominalGuard>,
}

/// A nominal guard is a special function which is called after a nominal wrap expression,
/// and checks that the value is of the correct type.
/// If it returns false, the program will throw a TypeError.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NominalGuard {
    /// The parameter binding
    pub param: ValueName,
    /// The body of the guard.
    pub body: SubTree
}

/// Thin type = type reference which is parsed from a string or AST node
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum ThinType {
    #[default]
    Any,
    Never {
        nullability: Nullability
    },
    Structural {
        nullability: Nullability,
        structure: TypeStructure<ThinType>
    },
    Nominal {
        nullability: Nullability,
        id: TypeIdent<ThinType>
    }
}

/// An identifier and its generic args
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeIdent<Type> {
    pub name: TypeName,
    // Remember: these don't need to be boxed because they are in a vec
    pub generic_args: Vec<Type>,
}

/// A generic parameter (generic def)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam<Type: TypeTrait> {
    /// Note that the actual variance is the intersection of this and the inferred variance.
    /// Furthermore, if this is outside of the actual variance the compiler will log an error
    pub variance_bound: Variance,
    pub name: TypeName,
    // Remember: these don't need to be boxed because they are in a vec
    /// Any instantiation of this parameter must inherit these
    pub supers: TypeTrait::Inherited,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum Variance {
    /// `lhs` must be equivalent to `rhs`, i.e. `lhs` and `rhs` must be subtypes of each other
    Invariant,
    /// `lhs` must be a subtype of `rhs`
    Covariant,
    /// `lhs` must be a supertype of `rhs`
    Contravariant,
    /// `lhs` and `rhs` must not be disjoint, i.e. if `lhs` and `rhs` are inhabited (there exists
    /// at least one instance of each) `lhs & rhs` must be inhabited (there exists at least one
    /// value which is both an instance of `lhs` and a `rhs`).
    #[default]
    Bivariant,
}

/// The JavaScript structure of instances of the type, if it is a compound type
/// (primitive types have identifiers).
///
/// All inner types are boxed to reduce size and allow recursive definitions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeStructure<Type: TypeTrait> {
    Fn {
        /// Function type. Boxed because it's large
        fn_type: Box<FnType<Type>>,
    },
    Array {
        /// The type of the elements
        element_type: Box<Type>,
    },
    Tuple {
        /// The types of the elements
        element_types: Vec<OptionalType<Type>>,
    },
    Object {
        /// The type of the fields
        field_types: Vec<Field<OptionalType<Type>>>,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructureKind {
    Fn,
    Array,
    Tuple,
    Object,
}

/// Function type structure
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnType<Type: TypeTrait> {
    /// Type parameters if polymorphic
    pub type_params: Vec<TypeParam<Type>>,
    /// The type of `this` in the function
    pub this_type: Type,
    /// The types of the arguments
    pub arg_types: Vec<OptionalType<Type>>,
    /// The type of the rest argument, pass `[]` for no rest argument
    pub rest_arg_type: Type::RestArgType,
    /// The type of the return value
    pub return_type: ReturnType<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Type of a function return. Note that `Void` is different (and a subtype) of `Type(Any)`,
/// which is the default.
pub enum ReturnType<Type> {
    Void,
    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field<Type> {
    pub name: SmolStr,
    pub type_: Type,
}

/// A type which may be optional
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OptionalType<Type> {
    pub optionality: Optionality,
    pub type_: Type
}

/// Base type of [ThinType] and [FatType] which contains associated types that are different
/// between the 2.
pub trait TypeTrait {
    type Inherited;
    type RestArgType;

    fn map_inherited(inherited: Self::Inherited, f: impl FnMut(Self) -> Self) -> Self::Inherited;
    fn map_ref_inherited(inherited: &Self::Inherited, f: impl FnMut(&Self) -> Self) -> Self::Inherited;
    fn map_rest_arg_type(rest_arg_type: Self::RestArgType, f: impl FnMut(Self) -> Self) -> Self::RestArgType;
    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArgType, f: impl FnMut(&Self) -> Self) -> Self::RestArgType;
}

/// Maps the associated types of [TypeTrait] from the `OldType` into ours.
pub trait TypeTraitMapsFrom<OldType: TypeTrait>: TypeTrait {
    fn map_inherited(inherited: OldType::Inherited, f: impl FnMut(OldType) -> Self) -> Self::Inherited;
    fn map_ref_inherited(inherited: &OldType::Inherited, f: impl FnMut(Cow<'_, OldType>) -> Self) -> Self::Inherited;
    fn map_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: OldType::RestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(OldType) -> Self
    ) -> FnType<Self>;
    fn map_ref_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &OldType::RestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(Cow<'_, OldType>) -> Self
    ) -> FnType<Self>;
}

impl<T: TypeTrait> TypeTraitMapsFrom<T> for T {
    fn map_inherited(inherited: T::Inherited, f: impl FnMut(T) -> Self) -> Self::Inherited {
        f(inherited)
    }

    fn map_ref_inherited(inherited: &T::Inherited, f: impl FnMut(Cow<'_, T>) -> Self) -> Self::Inherited {
        f(Cow::Borrowed(inherited))
    }

    fn map_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: T::RestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(T) -> Self
    ) -> FnType<Self> {
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type: Self::map_rest_arg_type(rest_arg_type, f),
            return_type,
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &T::RestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(Cow<'_, T>) -> Self
    ) -> FnType<Self> {
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type: Self::map_ref_rest_arg_type(rest_arg_type, |x| f(Cow::Borrowed(x))),
            return_type,
        }
    }
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
    () => {
    pub const EMPTY_REST_ARG: Self = Self::Structural {
        nullability: $crate::analyses::types::Nullability::NonNullable,
        structure: $crate::analyses::types::TypeStructure::Tuple {
            element_types: Vec::new(),
        }
    };

    pub fn func(
        type_params: impl Iterator<Item=$crate::analyses::types::TypeParam<Self>>,
        this_type: Self,
        arg_types: impl Iterator<Item=$crate::analyses::types::OptionalType<Self>>,
        rest_arg_type: Self,
        return_type: $crate::analyses::types::ReturnType<Self>
    ) -> Self {
        Self::Structural {
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Fn {
                fn_type: Box::new($crate::analyses::types::FnType::new(
                    type_params,
                    this_type,
                    arg_types,
                    rest_arg_type,
                    return_type
                ))
            }
        }
    }

    pub fn array(element_type: Self) -> Self {
        Self::Structural {
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Array {
                element_type: Box::new(element_type),
            }
        }
    }

    pub fn tuple(element_types: impl Iterator<Item=$crate::analyses::types::OptionalType<Self>>) -> Self {
        Self::Structural {
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Tuple {
                element_types: element_types.collect(),
            }
        }
    }

    pub fn object(property_types: impl Iterator<Item=$crate::analyses::types::Field<$crate::analyses::types::OptionalType<Self>>>) -> Self {
        Self::Structural {
            nullability: $crate::analyses::types::Nullability::NonNullable,
            structure: $crate::analyses::types::TypeStructure::Object {
                field_types: property_types.collect(),
            }
        }
    }
    }
}
pub(crate) use impl_structural_type_constructors;

impl ThinTypeDecl {
    pub const MISSING: Self = Self {
        name: TypeName::MISSING,
        type_params: Vec::new(),
        supertypes: Vec::new(),
        typescript_supertype: None,
        guard: None
    };
}

impl ThinType {
    pub const NEVER: Self = Self::Never { nullability: Nullability::NonNullable };

    pub const NULL: Self = Self::Never { nullability: Nullability::Nullable };

    pub fn dummy_for_hole(nullability: Nullability) -> Self {
        Self::Nominal {
            nullability,
            id: TypeIdent {
                name: TypeName::DUMMY_FOR_HOLE,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn ident(name: TypeName) -> Self {
        Self::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name,
                generic_args: Vec::new(),
            }
        }
    }

    pub fn generic(name: TypeName, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name,
                generic_args: generic_args.collect(),
            }
        }
    }
    pub fn ident2(name: &str) -> Self {
        Self::ident(TypeName::new(name))
    }

    pub fn generic2(name: &str, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::generic(TypeName::new(name), generic_args)
    }

    impl_structural_type_constructors!();

    pub fn make_nullable(&mut self) {
        match self {
            Self::Any => {},
            Self::Never { nullability } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
        }
    }

    pub fn make_nullable_if(&mut self, nullable: bool) {
        if nullable {
            self.make_nullable();
        }
    }

    pub fn nullable(self) -> Self {
        let mut clone = self.clone();
        clone.make_nullable();
        clone
    }

    pub fn nullable_if(self, nullable: bool) -> Self {
        if nullable {
            self.nullable()
        } else {
            self
        }
    }
}

impl TypeTrait for ThinType {
    type Inherited = Vec<ThinType>;
    type RestArgType = ThinType;

    fn map_inherited(mut inherited: Self::Inherited, mut f: impl FnMut(Self) -> Self) -> Self::Inherited {
        for inherited in inherited.iter_mut() {
            replace_with_or_default(inherited, &mut f);
        }
        inherited
    }

    fn map_ref_inherited(inherited: &Self::Inherited, f: impl FnMut(&Self) -> Self) -> Self::Inherited {
        inherited.iter().map(f).collect()
    }

    fn map_rest_arg_type(rest_arg_type: Self::RestArgType, f: impl FnMut(Self) -> Self) -> Self::RestArgType {
        f(rest_arg_type)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArgType, f: impl FnMut(&Self) -> Self) -> Self::RestArgType {
        f(rest_arg_type)
    }
}

impl<Type> TypeIdent<Type> {
    pub fn map<NewType>(self, f: impl FnMut(Type) -> NewType) -> TypeIdent<NewType> {
        TypeIdent {
            name: self.name,
            generic_args: self.generic_args.into_iter().map(f).collect(),
        }
    }

    pub fn map_ref<NewType>(&self, f: impl FnMut(&Type) -> NewType) -> TypeIdent<NewType> {
        TypeIdent {
            name: self.name.clone(),
            generic_args: self.generic_args.iter().map(f).collect(),
        }
    }
}

impl<Type> TypeParam<Type> {
    pub fn map<NewType>(self, f: impl FnMut(Type) -> NewType) -> TypeParam<NewType> {
        TypeParam {
            variance_bound: self.variance_bound,
            name: self.name,
            supers: self.supers.into_iter().map(f).collect(),
        }
    }

    pub fn map_ref<NewType>(&self, f: impl FnMut(&Type) -> NewType) -> TypeParam<NewType> {
        TypeParam {
            variance_bound: self.variance_bound,
            name: self.name.clone(),
            supers: self.supers.iter().map(f).collect(),
        }
    }
}

impl<Type: TypeTrait> TypeStructure<Type> {
    /// Get the structure-kind AKA enum variant.
    ///
    /// This is different than the formal definition of "kind" as in "higher-kinded types".
    pub fn kind(&self) -> StructureKind {
        match self {
            TypeStructure::Fn { .. } => StructureKind::Fn,
            TypeStructure::Array { .. } => StructureKind::Array,
            TypeStructure::Tuple { .. } => StructureKind::Tuple,
            TypeStructure::Object { .. } => StructureKind::Object,
        }
    }

    /// If this is an array, returns the element type. Otherwise `None`
    pub fn into_array_element_type(self) -> Option<Type> {
        match self {
            TypeStructure::Array { element_type } => Some(*element_type),
            _ => None,
        }
    }

    /// If this is a tuple, returns the element types. Otherwise `None`
    pub fn into_tuple_element_types(self) -> Option<Vec<OptionalType<Type>>> {
        match self {
            TypeStructure::Tuple { element_types } => Some(element_types),
            _ => None,
        }
    }

    /// If this is an object, returns the field types. Otherwise `None`
    pub fn into_object_field_types(self) -> Option<Vec<Field<OptionalType<Type>>>> {
        match self {
            TypeStructure::Object { field_types } => Some(field_types),
            _ => None,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<Type>>(self, mut f: impl FnMut(Type) -> NewType) -> TypeStructure<NewType> {
        match self {
            TypeStructure::Fn { fn_type } => TypeStructure::Fn {
                fn_type: Box::new(fn_type.map(f)),
            },
            TypeStructure::Array { element_type } => TypeStructure::Array {
                element_type: Box::new(f(*element_type)),
            },
            TypeStructure::Tuple { element_types } => TypeStructure::Tuple {
                element_types: element_types.into_iter().map(|elem| elem.map(&mut f)).collect(),
            },
            TypeStructure::Object { field_types } => TypeStructure::Object {
                field_types: field_types.into_iter().map(|field| field.map(|opt| opt.map(&mut f))).collect(),
            },
        }
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> TypeStructure<NewType> {
        match self {
            TypeStructure::Fn { fn_type } => TypeStructure::Fn {
                fn_type: Box::new(fn_type.map_ref(f)),
            },
            TypeStructure::Array { element_type } => TypeStructure::Array {
                element_type: Box::new(f(element_type)),
            },
            TypeStructure::Tuple { element_types } => TypeStructure::Tuple {
                element_types: element_types.iter().map(|elem| elem.map_ref(&mut f)).collect(),
            },
            TypeStructure::Object { field_types } => TypeStructure::Object {
                field_types: field_types.iter().map(|field| field.map_ref(|opt| opt.map_ref(&mut f))).collect(),
            },
        }
    }
}

impl<Type: TypeTrait> FnType<Type> {
    pub fn new(
        type_params: impl Iterator<Item=TypeParam<Type>>,
        this_type: Type,
        arg_types: impl Iterator<Item=OptionalType<Type>>,
        rest_arg_type: Type,
        return_type: ReturnType<Type>
    ) -> Self {
        FnType {
            type_params: type_params.collect(),
            this_type,
            arg_types: arg_types.collect(),
            rest_arg_type,
            return_type,
        }
    }

    pub fn map<NewType: TypeTraitMapsFrom<Type>>(self, mut f: impl FnMut(Type) -> NewType) -> FnType<NewType> {
        let type_params = self.type_params.into_iter().map(|param| param.map(&mut f)).collect();
        let this_type = f(self.this_type);
        let arg_types = self.arg_types.into_iter().map(|arg| arg.map(&mut f)).collect();
        let return_type = self.return_type.map(&mut f);
        NewType::map_rest_arg_type_in_fn(
            type_params,
            this_type,
            arg_types,
            self.rest_arg_type,
            return_type,
            f
        )
    }

    pub fn map_ref<NewType: TypeTraitMapsFrom<Type>>(&self, mut f: impl FnMut(&Type) -> NewType) -> FnType<NewType> {
        let type_params = self.type_params.iter().map(|param| param.map_ref(&mut f)).collect();
        let this_type = f(&self.this_type);
        let arg_types = self.arg_types.iter().map(|arg| arg.map_ref(&mut f)).collect();
        let return_type = self.return_type.map_ref(&mut f);
        NewType::map_ref_rest_arg_type_in_fn(
            type_params,
            this_type,
            arg_types,
            &self.rest_arg_type,
            return_type,
            f
        )
    }
}

impl<Type> OptionalType<Type> {
    pub fn new(type_: Type, is_optional: bool) -> Self {
        Self {
            optionality: Optionality::from(is_optional),
            type_,
        }
    }

    pub fn required(type_: Type) -> Self {
        Self {
            optionality: Optionality::Required,
            type_,
        }
    }

    pub fn optional(type_: Type) -> Self {
        Self {
            optionality: Optionality::Required,
            type_,
        }
    }

    pub fn map<NewType>(self, f: impl FnOnce(Type) -> NewType) -> OptionalType<NewType> {
        OptionalType {
            optionality: self.optionality,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> OptionalType<NewType> {
        OptionalType {
            optionality: self.optionality,
            type_: f(&self.type_),
        }
    }
}

impl<Type> ReturnType<Type> {
    pub fn map<NewType>(self, f: impl FnOnce(Type) -> NewType) -> ReturnType<NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }

    pub fn map_ref<NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> ReturnType<NewType> {
        match self {
            ReturnType::Type(type_) => ReturnType::Type(f(type_)),
            ReturnType::Void => ReturnType::Void,
        }
    }
}

impl<Type> Field<Type> {
    pub fn map<NewType>(self, f: impl FnOnce(Type) -> NewType) -> Field<NewType> {
        Field {
            name: self.name,
            type_: f(self.type_),
        }
    }

    pub fn map_ref<NewType>(&self, f: impl FnOnce(&Type) -> NewType) -> Field<NewType> {
        Field {
            name: self.name.clone(),
            type_: f(&self.type_),
        }
    }
}

impl<Type: Default> Default for ReturnType<Type> {
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

impl From<bool> for Nullability {
    fn from(is_nullable: bool) -> Self {
        if is_nullable {
            Self::Nullable
        } else {
            Self::NonNullable
        }
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