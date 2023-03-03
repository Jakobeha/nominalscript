use std::cmp::Ordering;
use smol_str::SmolStr;

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
        structure: StructuralType<ThinType>
    },
    Nominal {
        nullability: Nullability,
        id: TypeIdent<ThinType>
    }
}

/// The string type used for all type names
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName(SmolStr);

/// An identifier and its generic args
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeIdent<Type> {
    pub name: TypeName,
    pub generic_args: Vec<Type>,
}

/// A generic parameter (generic def)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeParam<Type> {
    /// Note that the actual variance is the intersection of this and the inferred variance.
    /// Furthermore, if this is outside of the actual variance the compiler will log an error
    pub variance_bound: Variance,
    pub name: TypeName,
    /// Any instantiation of this parameter must inherit these
    pub supers: Vec<Type>
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Variance {
    Invariant,
    Covariant,
    Contravariant,
    Bivariant
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
        /// The types of the arguments
        arg_types: Vec<OptionalType<Type>>,
        /// The type of the rest argument, pass `[]` for no rest argument
        rest_arg_type: Box<Type>,
        /// The type of the return value
        return_type: ReturnType<Type>,
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
/// Type of a function return. Note that `Void` is different (and a subtype) of `Type(Any)`,
/// which is the default.
///
/// The inner type is boxed because this is only a field of `TypeStructure`
pub enum ReturnType<Type> {
    Void,
    #[default]
    Type(Box<Type>),
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

impl TypeName {
    pub const RESERVED: [TypeName; 4] = [
        TypeName::new_inline("Any"),
        TypeName::new_inline("Never"),
        TypeName::new_inline("Null"),
        TypeName::new_inline("Void"),
    ];

    pub fn new(name: impl Into<SmolStr>) -> Self {
        Self(name.into())
    }

    pub const fn new_inline(name: &'static str) -> Self {
        Self(SmolStr::new_inline(name))
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