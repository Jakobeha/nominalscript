use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use crate::analyses::types::{IdentType, NominalGuard, Nullability, StructureType, TypeParam, Variance};
use crate::analyses::types::generic::{IdentType, NominalGuard, Nullability, StructureType, TypeParam, TypescriptType, Variance};
use crate::ast::ann::Ann;
use crate::{impl_has_ann_enum, impl_has_ann_record_struct, impl_has_ann_wrapper_struct};
use crate::analyses::bindings::TypeIdent;

/// Type declaration after we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct FatTypeDecl<'tree> {
    pub ann: Ann<'tree>,
    /// [IdentType]s with this name in this scope will point to this (unless shadowed)
    pub name: TypeIdent<'tree>,
    /// Type parameters which are bound and used in `inherited` (supertypes)
    pub type_params: Vec<TypeParam<'tree, FatType<'tree>>>,
    /// Supertypes, guards, etc.
    ///
    /// Boxed because it's large
    pub inherited: Box<FatTypeInherited<'tree>>
}
impl_has_ann_record_struct!(FatTypeDecl);

/// Fat type = type after we've resolved the supertypes so that they are also in this structure,
/// and fat types can be compared / unified directly
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatType<'tree> {
    /// Top = type of untyped values
    #[default]
    Any { ann: Ann<'tree> },
    /// *non-nullable* `Never` = bottom = type of values which cannot be produced (e.g. loops)
    /// *nullable* `Never` = `Null` = only null
    Never {
        ann: Ann<'tree>,
        nullability: Nullability,
    },
    /// Type with no nominal id, so a non-nominally wrapped structure can be an instance.
    /// This also means there is no typescript type or guard
    Structural {
        ann: Ann<'tree>,
        nullability: Nullability,
        structure: StructureType<'tree, FatType<'tree>>,
    },
    /// Type with at least one nominal id, so all instances are nominally wrapped.
    /// There may be an additional typescript type and guards as well
    Nominal {
        ann: Ann<'tree>,
        nullability: Nullability,
        id: IdentType<'tree, FatType<'tree>>,
        /// Boxed because it's large
        inherited: Box<FatTypeInherited<'tree>>
    },
    /// Uninstantiated generic: equivalent to `Never` if never unified,
    /// but when unified, it becomes the type it was unified with and stays that way.
    ///
    /// Note that it is not `==` to the equivalent fat type even after unification.
    /// It will be a subtype / supertype though
    Hole {
        ann: Ann<'tree>,
        nullability: Nullability,
        hole: FatTypeHole<'tree>
    }
}
impl_has_ann_enum!(FatType { Any, Never, Structural, Nominal, Hole });

/// Everything a fat type can inherit:
///
/// - super nominal types
/// - super structural type (only one)
/// - super typescript types
/// - guards
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeInherited<'tree> {
    pub super_ids: VecDeque<IdentType<'tree, FatType<'tree>>>,
    pub structure: Option<StructureType<'tree, FatType<'tree>>>,
    pub typescript_types: Vec<TypescriptType<'tree>>,
    pub guards: Vec<NominalGuard<'tree>>,
    pub is_never: bool,
}

/// Fat type argument: has the type identifier's variance as well as the actual argument
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeArg<'tree> {
    pub variance_bound: Variance,
    pub type_: FatType<'tree>,
}
impl_has_ann_wrapper_struct!(FatTypeArg by type_);

/// Possible rest argument in a fat function type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatRestArgType<'tree> {
    #[default]
    None,
    Array { ann: Ann<'tree>, element: FatType<'tree> },
    Illegal { intended_type: FatType<'tree> },
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
pub struct FatTypeHole<'tree> {
    pub ann: Ann<'tree>,
    /// What the value is currently unified as
    pub(super) upper_bound: Rc<RefCell<FatType<'tree>>>
}
impl_has_ann_record_struct!(FatTypeHole);
