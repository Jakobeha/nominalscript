use std::cmp::Ordering;
use smol_str::SmolStr;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::ast::tree_sitter::TSSubTree;

/// Type declaration before we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct ThinTypeDecl {
    pub name: TypeName,
    pub type_params: Vec<TypeParam<ThinType>>,
    pub supertypes: Vec<ThinType>,
    pub typescript_supertype: Option<TSSubTree>,
    pub guard: Option<NominalGuard>,
}

/// A nominal guard is a special function which is called after a nominal wrap expression,
/// and checks that the value is of the correct type.
/// If it returns false, the program will throw a TypeError.
#[derive(Debug, Clone)]
pub struct NominalGuard {
    /// The parameter binding
    pub param: ValueName,
    /// The body of the guard.
    pub body: TSSubTree
}

/// Thin type = type reference which is parsed from a string or AST node
#[derive(Debug, Clone, Default, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeIdent<Type> {
    pub name: TypeName,
    // Remember: these don't need to be boxed because they are in a vec
    pub generic_args: Vec<Type>,
}

/// A generic parameter (generic def)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam<Type> {
    /// Note that the actual variance is the intersection of this and the inferred variance.
    /// Furthermore, if this is outside of the actual variance the compiler will log an error
    pub variance_bound: Variance,
    pub name: TypeName,
    // Remember: these don't need to be boxed because they are in a vec
    /// Any instantiation of this parameter must inherit these
    pub supers: Vec<Type>
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Variance {
    /// `lhs` must be equivalent to `rhs`, i.e. `lhs` and `rhs` must be subtypes of each other
    Invariant,
    /// `lhs` must be a subtype of `rhs`
    Covariant,
    /// `lhs` must be a supertype of `rhs`
    Contravariant,
    /// `lhs` and `rhs` must not be incompatible, i.e. if `lhs` and `rhs` are inhabited `lhs & rhs` must be inhabited
    #[default]
    Bivariant,
}

/// The JavaScript structure of instances of the type, if it is a compound type
/// (primitive types have identifiers).
///
/// All inner types are boxed to reduce size and allow recursive definitions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeStructure<Type> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructureKind {
    Fn,
    Array,
    Tuple,
    Object,
}

/// Function type structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType<Type> {
    /// Type parameters if polymorphic
    pub type_params: Vec<TypeParam<Type>>,
    /// The type of `this` in the function
    pub this_type: Type,
    /// The types of the arguments
    pub arg_types: Vec<OptionalType<Type>>,
    /// The type of the rest argument, pass `[]` for no rest argument
    pub rest_arg_type: Type,
    /// The type of the return value
    pub return_type: ReturnType<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// Type of a function return. Note that `Void` is different (and a subtype) of `Type(Any)`,
/// which is the default.
pub enum ReturnType<Type> {
    Void,
    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field<Type> {
    pub name: SmolStr,
    pub type_: Type,
}

/// A type which may be optional
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OptionalType<Type> {
    pub optionality: Optionality,
    pub type_: Type
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Optionality {
    Optional,
    Required,
}

impl ThinType {
    pub fn never() -> Self {
        Self::Never { nullability: Nullability::NonNullable }
    }

    pub fn null() -> Self {
        Self::Never { nullability: Nullability::Nullable }
    }

    pub fn ident(name: &str) -> Self {
        Self::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name: TypeName::new(name),
                generic_args: Vec::new(),
            }
        }
    }

    pub fn generic(name: &str, generic_args: impl Iterator<Item=Self>) -> Self {
        Self::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name: TypeName::new(name),
                generic_args: generic_args.collect(),
            }
        }
    }

    pub fn func(
        type_params: impl Iterator<Item=TypeParam<Self>>,
        this_type: Self,
        arg_types: impl Iterator<Item=OptionalType<Self>>,
        rest_arg_type: Self,
        return_type: ReturnType<Self>
    ) -> Self {
        Self::Structural {
            nullability: Nullability::NonNullable,
            structure: TypeStructure::Fn {
                fn_type: Box::new(FnType::new(
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
            nullability: Nullability::NonNullable,
            structure: TypeStructure::Array {
                element_type: Box::new(element_type),
            }
        }
    }

    pub fn tuple(element_types: impl Iterator<Item=OptionalType<Self>>) -> Self {
        Self::Structural {
            nullability: Nullability::NonNullable,
            structure: TypeStructure::Tuple {
                element_types: element_types.collect(),
            }
        }
    }

    pub fn object(property_types: impl Iterator<Item=Field<OptionalType<Self>>>) -> Self {
        Self::Structural {
            nullability: Nullability::NonNullable,
            structure: TypeStructure::Object {
                field_types: property_types.collect(),
            }
        }
    }

    pub fn empty_rest_arg() -> Self {
        Self::tuple(std::iter::empty())
    }

    pub fn nullable(self) -> Self {
        match self {
            Self::Any => Self::Any,
            Self::Never { nullability: _ } => Self::Never { nullability: Nullability::Nullable },
            Self::Structural { nullability: _, structure } => Self::Structural { nullability: Nullability::Nullable, structure },
            Self::Nominal { nullability: _, id } => Self::Nominal { nullability: Nullability::Nullable, id },
        }
    }

    pub fn nullable_if(self, nullable: bool) -> Self {
        if nullable {
            self.nullable()
        } else {
            self
        }
    }

    pub fn make_nullable(&mut self) {
        *self = self.nullable();
    }

    pub fn make_nullable_if(&mut self, nullable: bool) {
        if nullable {
            self.make_nullable();
        }
    }
}

impl<Type> TypeParam<Type> {
    pub fn map<F, NewType>(self, f: F) -> TypeParam<NewType>
    where
        F: FnMut(Type) -> NewType
    {
        TypeParam {
            variance_bound: self.variance_bound,
            name: self.name,
            supers: self.supers.into_iter().map(f).collect(),
        }
    }
}

impl<Type> TypeStructure<Type> {
    pub fn kind(&self) -> StructureKind {
        match self {
            TypeStructure::Fn { .. } => StructureKind::Fn,
            TypeStructure::Array { .. } => StructureKind::Array,
            TypeStructure::Tuple { .. } => StructureKind::Tuple,
            TypeStructure::Object { .. } => StructureKind::Object,
        }
    }
}

impl<Type> FnType<Type> {
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

impl From<bool> for Nullability {
    fn from(is_nullable: bool) -> Self {
        if is_nullable {
            Self::Nullable
        } else {
            Self::NonNullable
        }
    }
}