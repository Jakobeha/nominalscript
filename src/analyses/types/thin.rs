use std::cmp::Ordering;
use smol_str::SmolStr;
use crate::analyses::bindings::TypeName;
use crate::ast::tree_sitter::TSNode;

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
    Invariant,
    Covariant,
    Contravariant,
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
        /// Type parameters if polymorphic
        type_params: Vec<TypeParam<Type>>,
        /// The type of `this` in the function
        this_type: Box<Type>,
        // Remember: these don't need to be boxed because they are in a vec
        /// The types of the arguments
        arg_types: Vec<OptionalType<Type>>,
        /// The type of the rest argument, pass `[]` for no rest argument
        rest_arg_type: Box<Type>,
        /// The type of the return value
        ///
        /// The inner type is boxed because otherwise it's an inline field of `TypeStructure`
        return_type: ReturnType<Box<Type>>,
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
        /// The type of the properties
        property_types: Vec<Field<OptionalType<Type>>>,
    }
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
        return_type: ReturnType<Box<Self>>
    ) -> Self {
        Self::Structural {
            nullability: Nullability::NonNullable,
            structure: TypeStructure::Fn {
                type_params: type_params.collect(),
                this_type: Box::new(this_type),
                arg_types: arg_types.collect(),
                rest_arg_type: Box::new(rest_arg_type),
                return_type,
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
                property_types: property_types.collect(),
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

impl<T> OptionalType<T> {
    pub fn required(type_: T) -> Self {
        Self {
            optionality: Optionality::Required,
            type_,
        }
    }

    pub fn optional(type_: T) -> Self {
        Self {
            optionality: Optionality::Required,
            type_,
        }
    }
}

impl<T> ReturnType<Box<T>> {
    pub fn boxed(type_: T) -> Self {
        Self::Type(Box::new(type_))
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

impl<T: Default> Default for ReturnType<T> {
    fn default() -> Self {
        Self::Type(Default::default())
    }
}