use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use smallvec::SmallVec;

use crate::analyses::bindings::{FieldName, TypeName};
use crate::analyses::types::{NominalGuard, Nullability, TypeIdent, TypeParam, TypeStructure};
use crate::ast::tree_sitter::SubTree;

/// Type declaration after we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct FatTypeDecl {
    pub name: TypeName,
    pub type_params: Vec<TypeParam<FatType>>,
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

#[derive(Debug, Clone)]
pub struct FatTypeHole {
    pub(super) upper_bound: Rc<RefCell<FatType>>
}

/// Index path to an identifier inside a fat type (e.g. to get from `({ foo: Foo<Bar<Baz>>[] }) -> Void` to `Bar<Baz>`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FatIdentIndexPath {
    pub(super) steps: SmallVec<[FatIdentIndexStep; 8]>,
}

/// Part of a [FatIdentIndexPath] to go one step into a fat type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FatIdentIndexStep {
    InIdentTypeArg { index: usize },
    InFn(FatIdentIndexStepInFn),
    InArray,
    InTuple { index: usize },
    InObject { field_name: FieldName },
}

/// [FatIdentIndexStep] into a function
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FatIdentIndexStepInFn {
    TypeParamSuper { index: usize },
    ThisArg,
    Arg { index: usize },
    RestArg(FatIdentIndexStepInRestArg),
    ReturnValue,
}

/// [FatIdentIndexStep] into a rest argument
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FatIdentIndexStepInRestArg {
    ArrayElement,
    IllegalIntendedType,
}

impl FatIdentIndexPath {
    /// Index path which points to the type itself
    pub fn empty() -> Self {
        Self { steps: SmallVec::new() }
    }

    /// Index path which follows `step` and then the remaining steps in `Self`
    pub fn prepend_step(mut self, step: FatIdentIndexStep) -> Self {
        self.steps.push(step);
        self
    }

    /// Iterates steps, from outermost to innermost
    pub fn steps(&self) -> impl Iterator<Item = &'_ FatIdentIndexStep> + '_ {
        self.steps.iter().rev()
    }
}