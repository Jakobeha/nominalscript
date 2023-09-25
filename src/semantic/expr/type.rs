use std::cmp::Ordering;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};
use crate::impl_has_name;
use crate::semantic::def::TypeDefData;
use crate::semantic::name::{FieldIdent, FieldName};
use crate::semantic::r#use::TypeUse;
use crate::storage::{Ann, Id, InnerMap, InnerSet, InnerVec};
use crate::type_sitter::nominalscript::nodes;

/// Type = identifier with optional generics, builtin or structural type.
pub type Type<'tree> = Id<'tree, TypeData<'tree>>;
/// [Type] data
#[derive(Debug)]
pub struct TypeData<'tree> {
    /// Is this type nullable?
    pub nullability: Nullability,
    /// The first is the nominal type identifier, followed by nominal supertypes' identifiers.
    /// Iff there are no identifiers, this is a structural type.
    pub identifiers: InnerVec<'tree, TypeIdentifier<'tree>>,
    /// The type's structure(s)
    pub structure: InnerSet<'tree, TypeStructure<'tree>>,
    pub guards: InnerSet<'tree, nodes::NominalTypeGuard<'tree>>,
    pub typescript_types: InnerSet<'tree, nodes::TypeAnnotation<'tree>>,
}

/// Type identifier (references a nominal type declaration) and generic arguments
pub type TypeIdentifier<'tree> = Id<'tree, TypeIdentifierData<'tree>>;
/// [TypeIdentifier] data
#[derive(Debug)]
pub struct TypeIdentifierData<'tree> {
    /// Nominal type name and declaration referenced by the identifier, or [None] if this is a
    /// nonexistent reference
    pub r#use: Option<TypeUse<'tree>>,
    /// Generic arguments
    pub arguments: Vec<Type<'tree>>
}

/// Structural type structure (e.g. array)
pub type TypeStructure<'tree> = Id<'tree, TypeStructureData<'tree>>;
/// [TypeStructure] data
#[derive(Debug)]
pub enum TypeStructureData<'tree> {
    Fn(FnType<'tree>),
    Array {
        /// Array element type
        element: Type<'tree>
    },
    Tuple {
        /// Tuple element types
        elements: InnerVec<'tree, OptionalType<'tree>>
    },
    Object {
        /// Object field names and types
        fields: InnerMap<'tree, FieldName, FieldType<'tree>>
    }
}

/// Function type
type FnType<'tree> = Id<'tree, FnTypeData<'tree>>;
/// [FnType] data
#[derive(Debug)]
pub struct FnTypeData<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Type parameters
    pub type_parameters: InnerVec<'tree, TypeDefData<'tree>>,
    /// This parameter type
    pub this_parameter: Option<Type<'tree>>,
    /// Regular parameter types
    pub regular_parameters: InnerVec<'tree, OptionalType<'tree>>,
    /// Rest parameter type
    pub rest_parameter: RestType<'tree>,
    /// Return type
    pub return_: ReturnType<'tree>
}

/// Optional parameter, tuple element, or field type
pub type OptionalType<'tree> = Id<'tree, OptionalTypeData<'tree>>;
/// [OptionalType] data
#[derive(Debug)]
pub struct OptionalTypeData<'tree> {
    /// Is this type optional?
    ///
    /// Note that [Optionality] is different than [Nullability] (similar but not the same)
    pub optionality: Optionality,
    /// The underlying type
    pub type_: Type<'tree>
}

/// Rest parameter type
pub type RestType<'tree> = Id<'tree, RestTypeData<'tree>>;
/// [RestType] data
#[derive(Debug)]
pub enum RestTypeData<'tree> {
    None,
    Array {
        /// Element type (rest parameter = zero or more of these)
        element: Type<'tree>
    }
}

/// Function return type
pub type ReturnType<'tree> = Id<'tree, ReturnTypeData<'tree>>;
/// [ReturnType] data
#[derive(Debug)]
pub enum ReturnTypeData<'tree> {
    Void,
    Type(Type<'tree>)
}

/// Field type
pub type FieldType<'tree> = Id<'tree, FieldTypeData<'tree>>;
/// [FieldType] data
#[derive(Debug)]
pub struct FieldTypeData<'tree> {
    /// Field name identifier
    pub ident: FieldIdent<'tree>,
    /// Field value's type
    pub value: OptionalType<'tree>
}

/// For each type parameter `T`, if `T1 <: T2`, how do `F<..., T1, ...>` and `F<..., T2, ...>`
/// relate? e.g. if `A` is [Variance::Covariant] and `B` is [Variant::Contravariant] in `F<A, B>`,
/// then `F<A1, B1>` is a subtype of `F<A2, B2>` iff `A1 <: A2` and `B2 <: B1`.
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

impl_has_name!(<'tree> FieldName for FieldTypeData<'tree>);

impl Variance {
    /// Returns the variance which is or isn't covariant and contravariant according to the args,
    /// i.e:
    /// - `(true, true)` => [Variance::Bivariant]
    /// - `(true, false)` => [Variance::Covariant]
    /// - `(false, true)` => [Variance::Contravariant]
    /// - `(false, false)` => [Variance::Invariant]
    pub fn new(is_covariant: bool, is_contravariant: bool) -> Self {
        match (is_covariant, is_contravariant) {
            (true, true) => Variance::Bivariant,
            (true, false) => Variance::Covariant,
            (false, true) => Variance::Contravariant,
            (false, false) => Variance::Invariant,
        }
    }

    /// Returns the opposite variance, which is contravariant iff this is covariant and vice versa,
    /// i.e.:
    /// - [Variance::Bivariant] => [Variance::Bivariant]
    /// - [Variance::Covariant] => [Variance::Contravariant]
    /// - [Variance::Contravariant] => [Variance::Covariant]
    /// - [Variance::Invariant] => [Variance::Invariant]
    pub fn reversed(self) -> Self {
        match self {
            Variance::Bivariant => Variance::Bivariant,
            Variance::Covariant => Variance::Contravariant,
            Variance::Contravariant => Variance::Covariant,
            Variance::Invariant => Variance::Invariant,
        }
    }

    /// Is this covariant or bivariant?
    pub fn is_covariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => true,
            Variance::Contravariant => false,
            Variance::Invariant => false,
        }
    }

    /// Is this contravariant or bivariant?
    pub fn is_contravariant(self) -> bool {
        match self {
            Variance::Bivariant => true,
            Variance::Covariant => false,
            Variance::Contravariant => true,
            Variance::Invariant => false,
        }
    }

    /// Is the subtype operator with this variance a union or an intersection? i.e.
    /// [Self::is_covariant]
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