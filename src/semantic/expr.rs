use indexmap::IndexMap;
use ouroboros::self_referencing;
use crate::analyses::bindings::FieldNameStr;
use crate::analyses::types::{Nullability, Optionality};
use crate::{impl_has_ann_enum, impl_has_ann_record_struct};
use crate::misc::arena::IdentityRef;
use crate::semantic::ann::Ann;
use crate::semantic::def::OwnedTypeDef;
use crate::semantic::r#use::{TypeUse, ValueUse};
use crate::syntax::nodes;

/// Value expression = either a value (identifier, builtin or structure) or operation which reduces to a value
pub type Expr<'tree> = IdentityRef<'tree, Expr<'tree>>;
/// Owned [Expr]
#[derive(Debug)]
pub enum OwnedExpr<'tree> {
    Identifier {
        /// Source location
        ann: Ann<'tree>,
        /// Identifier name and declaration referenced by this identifier, or [None] if this is a
        /// nonexistent reference
        r#use: Option<ValueUse<'tree>>
    },
    /// Misc operation, we only care about the sub-expressions
    Misc {
        /// Source location
        ann: Ann<'tree>,
        /// Immediate sub-expressions (e.g. operator arguments, or function call name and arguments)
        children: Expr<'tree>
    }
}

/// Type = identifier with optional generics, builtin or structural type.
pub type Type<'tree> = IdentityRef<'tree, Type<'tree>>;
/// Owned [Type]
#[derive(Debug)]
pub struct OwnedType<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Is this type nullable?
    pub nullability: Nullability,
    /// The first is the nominal type identifier, followed by nominal supertypes' identifiers.
    /// Iff there are no identifiers, this is a structural type.
    pub identifiers: Vec<TypeIdentifier<'tree>>,
    /// The type's structure(s)
    pub structure: Vec<TypeStructure<'tree>>,
    pub guards: Vec<nodes::NominalTypeGuard<'tree>>,
    pub typescript_types: Vec<nodes::TypeAnnotation<'tree>>,
}

/// Type identifier (references a nominal type declaration) and generic arguments
#[derive(Debug)]
pub struct TypeIdentifier<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Nominal type name and declaration referenced by the identifier, or [None] if this is a
    /// nonexistent reference
    pub r#use: Option<TypeUse<'tree>>,
    /// Generic arguments
    pub arguments: Vec<Type<'tree>>
}

/// Structural type structure (e.g. array)
#[derive(Debug)]
pub enum TypeStructure<'tree> {
    Fn(FnType<'tree>),
    Array {
        /// Source location
        ann: Ann<'tree>,
        /// Array element type
        element: Type<'tree> },
    Tuple {
        /// Source location
        ann: Ann<'tree>,
        /// Tuple element types
        elements: Vec<OptionalType<'tree>> },
    Object {
        /// Source location
        ann: Ann<'tree>,
        /// Object field names and types
        fields: FieldsType<'tree>
    }
}

/// Function type
#[derive(Debug)]
#[self_referencing]
pub struct FnType<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Type parameters
    pub type_parameters: ArenaAssocVec<OwnedTypeDef<'tree>>,
    /// This parameter type
    #[borrows(type_parameters)]
    #[covariant]
    pub this_parameter: Option<Type<'this>>,
    /// Regular parameter types
    #[borrows(type_parameters)]
    #[covariant]
    pub regular_parameters: Vec<OptionalType<'this>>,
    /// Rest parameter type
    #[borrows(type_parameters)]
    #[covariant]
    pub rest_parameter: RestType<'this>,
    /// Return type
    #[borrows(type_parameters)]
    #[covariant]
    pub return_: ReturnType<'this>
}

/// Optional parameter, tuple element, or field type
#[derive(Debug)]
pub struct OptionalType<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Is this type optional?
    ///
    /// Note that [Optionality] is different than [Nullability] (similar but not the same)
    pub optionality: Optionality,
    /// The underlying type
    pub type_: Type<'tree>
}

/// Rest parameter type
#[derive(Debug)]
pub enum RestType<'tree> {
    None {
        /// Source location (will be [Ann::Implied])
        ann: Ann<'tree>,
    },
    Array {
        /// Source location
        ann: Ann<'tree>,
        /// Element type (rest parameter = zero or more of these)
        element: Type<'tree>
    }
}

/// Function return type
#[derive(Debug)]
pub enum ReturnType<'tree> {
    Void {
        /// Source location
        ann: Ann<'tree>,
    },
    Type(Type<'tree>)
}

/// Names and types of a set of fields
#[derive(Debug)]
pub struct FieldsType<'tree> {
    /// Underlying repr
    map: IndexMap<&'tree FieldNameStr, OptionalType<'tree>>
}

/// Field type
#[derive(Debug)]
pub struct FieldType<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Field name
    pub name: &'tree FieldNameStr,
    /// Field value's type
    pub value: Type<'tree>
}

impl_has_ann_enum!(OwnedExpr { Identifier, Misc });
impl_has_ann_record_struct!(OwnedType);
impl_has_ann_record_struct!(TypeIdentifier);
impl_has_ann_enum!(TypeStructure (Fn) { Array, Tuple, Object });
impl_has_ann_record_struct!(FnType);
impl_has_ann_record_struct!(OptionalType);
impl_has_ann_enum!(RestType { None, Array });
impl_has_ann_enum!(ReturnType { Void, Type });
impl_has_ann_record_struct!(FieldType);