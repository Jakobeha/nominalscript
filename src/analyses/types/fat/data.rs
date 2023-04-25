use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::analyses::bindings::TypeName;
use crate::analyses::types::{NominalGuard, Nullability, TypeIdent, TypeParam, TypeStructure, Variance};
use crate::ast::tree_sitter::SubTree;

/// Type declaration after we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct FatTypeDecl {
    /// [TypeIdent]s with this name in this scope will point to this (unless shadowed)
    pub name: TypeName,
    /// Type parameters which are bound and used in `inherited` (supertypes)
    pub type_params: Vec<TypeParam<FatType>>,
    /// Supertypes, guards, etc.
    ///
    /// Boxed because it's large
    pub inherited: Box<FatTypeInherited>
}

/// Fat type = type after we've resolved the supertypes so that they are also in this structure,
/// and fat types can be compared / unified directly
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatType {
    /// Top = type of untyped values
    #[default]
    Any,
    /// *non-nullable* `Never` = bottom = type of values which cannot be produced (e.g. loops)
    /// *nullable* `Never` = `Null` = only null
    Never {
        nullability: Nullability,
    },
    /// Type with no nominal id, so a non-nominally wrapped structure can be an instance.
    /// This also means there is no typescript type or guard
    Structural {
        nullability: Nullability,
        structure: TypeStructure<FatType>,
    },
    /// Type with at least one nominal id, so all instances are nominally wrapped.
    /// There may be an additional typescript type and guards as well
    Nominal {
        nullability: Nullability,
        id: TypeIdent<FatType>,
        /// Boxed because it's large
        inherited: Box<FatTypeInherited>
    },
    /// Uninstantiated generic: equivalent to `Never` if never unified,
    /// but when unified, it becomes the type it was unified with and stays that way.
    ///
    /// Note that it is not `==` to the equivalent fat type even after unification.
    /// It will be a subtype / supertype though
    Hole {
        nullability: Nullability,
        hole: FatTypeHole
    }
}

/// Everything a fat type can inherit:
///
/// - super nominal types
/// - super structural type (only one)
/// - super typescript types
/// - guards
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeInherited {
    pub super_ids: VecDeque<TypeIdent<FatType>>,
    pub structure: Option<TypeStructure<FatType>>,
    pub typescript_types: Vec<SubTree>,
    pub guards: Vec<NominalGuard>,
    pub is_never: bool,
}

/// Fat type argument: has the type identifier's variance as well as the actual argument
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeArg {
    pub variance_bound: Variance,
    pub type_: FatType,
}

/// Possible rest argument in a fat function type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatRestArgType {
    #[default]
    None,
    Array { element: FatType },
    Illegal { intended_type: FatType },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum FatRestArgKind {
    Any,
    Tuple,
    Array,
    Illegal
}

/// Uninstantiated generic: equivalent to `Never` if never unified,
/// but when unified, it becomes the type it was unified with and stays that way.
///
/// Internally, this has a reference-counter pointer to the unified upper bound,
/// so if you clone and then unify one instance, it affects the other.
#[derive(Debug, Clone)]
pub struct FatTypeHole {
    /// What the value is currently unified as
    pub(super) upper_bound: Rc<RefCell<FatType>>
}