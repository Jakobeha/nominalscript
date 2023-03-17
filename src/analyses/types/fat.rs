use std::cell::RefCell;
use std::rc::Rc;

use replace_with::replace_with_or_default;

use crate::analyses::bindings::TypeName;
use crate::analyses::types::{impl_structural_type_constructors, NominalGuard, Nullability, StructureKind, TypeIdent, TypeLoc, TypeParam, TypeStructure, Variance};
use crate::ast::tree_sitter::{TSSubTree, TSTree};
use crate::diagnostics::TypeLogger;
use crate::{error, note};

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
pub enum FatType<Hole: FatTypeHoleTrait = NoHole> {
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
        structure: TypeStructure<FatType<Hole>>,
    },
    /// Type with at least one nominal id, so all instances are nominally wrapped.
    /// There may be an additional typescript type and guards as well
    Nominal {
        nullability: Nullability,
        id: TypeIdent<FatType<Hole>>,
        /// Boxed because it's large
        inherited: Box<FatTypeInherited<Hole>>
    },
    /// Uninstantiated generic: equivalent to `Never` if never unified,
    /// but when unified, it becomes the type it was unified with and stays that way.
    ///
    /// Note that it is not `==` to the equivalent fat type even after unification.
    /// It will be a subtype / supertype though
    Hole {
        nullability: Nullability,
        hole: Hole
    }
}

/// Everything a fat type can inherit:
///
/// - super nominal types
/// - super structural type (only one)
/// - super typescript types
/// - guards
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FatTypeInherited<Hole: FatTypeHoleTrait = NoHole> {
    pub super_ids: Vec<TypeIdent<FatType<Hole>>>,
    pub structure: Option<TypeStructure<FatType<Hole>>>,
    pub typescript_types: Vec<TSSubTree>,
    pub guards: Vec<NominalGuard>,
}

#[derive(Debug, Clone)]
pub struct FatTypeHole {
    upper_bound: Rc<RefCell<FatType<FatTypeHole>>>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoHole {}

pub trait FatTypeHoleTrait {}

/// [FatType] which supports type-holes, for local expressions.
///
/// Holes are not thread-safe.
/// Global (exported) types cannot have holes and they are thread-safe.
pub type LocalFatType = FatType<FatTypeHole>;

/// [FatTypeInherited] which supports type-holes, for local expressions.
///
/// Holes are not thread-safe.
/// Global (exported) types cannot have holes and they are thread-safe.
pub type LocalFatTypeInherited = FatTypeInherited<FatTypeHole>;

impl FatTypeDecl {
    pub fn missing() -> Self {
        Self {
            name: TypeName::MISSING,
            type_params: Vec::new(),
            inherited: Box::new(FatTypeInherited::empty())
        }
    }
}

impl Default for FatTypeDecl {
    fn default() -> Self {
        Self::missing()
    }
}

impl<Hole: FatTypeHoleTrait> FatType<Hole> {
    pub const NEVER: Self = Self::Never { nullability: Nullability::NonNullable };

    pub const NULL: Self = Self::Never { nullability: Nullability::Nullable };

    impl_structural_type_constructors!();

    pub fn nullable(self) -> Self {
        match self {
            Self::Never { nullability: _ } => Self::Never {
                nullability: Nullability::Nullable
            },
            Self::Any => Self::Any,
            Self::Structural { nullability: _, structure } => Self::Structural {
                nullability: Nullability::Nullable,
                structure
            },
            Self::Nominal { nullability: _, id, inherited } => Self::Nominal {
                nullability: Nullability::Nullable,
                id,
                inherited
            },
            Self::Hole { nullability: _, hole } => Self::Hole {
                nullability: Nullability::Nullable,
                hole
            }
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

    pub fn collapse_supers(
        supers: impl IntoIterator<Item=Self>,
        mut e: TypeLogger<'_, '_, '_>
    ) -> FatTypeInherited {
        let mut inherited = FatTypeInherited::empty();
        for super_ in supers {
            match super_ {
                // Do nothing
                FatType::Any => {}
                FatType::Never { nullability } => {
                    error!(e, "Can't extend `{}`", match nullability {
                        Nullability::NonNullable => "Never",
                        Nullability::Nullable => "Null"
                    });
                }
                FatType::Structural { nullability, structure } => {
                    if nullability == Nullability::Nullable {
                        error!(e, "Can't extend nullable type";
                            note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                    }
                    FatType::unify_structure(
                        &mut inherited.structure,
                        Some(structure),
                        Variance::Bivariant,
                        e.with_context(TypeLoc::Supertypes)
                    );
                }
                FatType::Nominal { nullability, id: super_id, inherited: super_inherited } => {
                    if nullability == Nullability::Nullable {
                        error!(e, "Can't extend nullable type";
                            note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                    }
                    inherited.super_ids.push(super_id);
                    inherited.merge(
                        *super_inherited,
                        e.with_context(TypeLoc::Supertypes)
                    );
                }
                FatType::Hole { .. } => {
                    log::error!("Tried to extend hole! This should never happen!");
                    error!(e, "Can't extend hole (though you shouldn't be able to cause this...)");
                }
            }
        }
        return inherited
    }

    pub fn unify_structure(
        this: &mut Option<TypeStructure<Self>>,
        other: Option<TypeStructure<Self>>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        let Some(other) = other else {
            if !bias.is_contravariant() {
                error!(e, "assigned type input must be a structure but required type does not");
            }
            return
        };
        let Some(this) = this else {
            if !bias.is_covariant() {
                error!(e, "assigned type has no structure but required type does");
            }
            *this = Some(other);
            return
        };
        Self::unify_structure2(this, other, bias, e);
    }

    pub fn unify_structure2(
        this: &mut TypeStructure<Self>,
        mut other: TypeStructure<Self>,
        bias: Variance,
        mut e: TypeLogger<'_, '_, '_>
    ) {
        // Do whole structure conversions to get equivalent kinds of possible
        match (this.kind(), other.kind()) {
            (StructureKind::Tuple, StructureKind::Array) => {
                if !bias.is_covariant() {
                    error!(e, "an array is not a subtype of a tuple");
                }
                replace_with_or_default(this, |this| {
                    let TypeStructure::Tuple { element_types } = this else { unreachable!() };
                    TypeStructure::Array {
                        element_type: FatType::unify_all(element_types, TypeLogger::ignore())
                    }
                });
            }
            (StructureKind::Array, StructureKind::Tuple) => {
                if !bias.is_contravariant() {
                    error!(e, "a tuple is not a supertype of an array");
                }
                let TypeStructure::Tuple { element_types } = other else { unreachable!() };
                other = TypeStructure::Array {
                    element_type: FatType::unify_all(element_types, TypeLogger::ignore())
                }
            }
            _ => {}
        }
        // Merge if equivalent kinds or log error
        match (this, other) {
            (TypeStructure::Fn { fn_type }, TypeStructure::Fn { fn_type: other_fn_type }) => {
                Self::unify_fn(fn_type, other_fn_type, bias, e);
            }
            (TypeStructure::Array { element_type }, TypeStructure::Array { element_type: other_element_type }) => {
                Self::unify(
                    element_type,
                    other_element_type,
                    bias,
                    e.with_context(TypeLoc::ArrayElem)
                );
            }
            (TypeStructure::Tuple { element_types }, TypeStructure::Tuple { element_types: other_element_types }) => {
                Self::unify_optionals(
                    element_types,
                    other_element_types,
                    bias,
                    |index | e.with_context(TypeLoc::TupleElem { index })
                );
            }
            (TypeStructure::Object { field_types }, TypeStructure::Object { field_types: other_field_types }) => {
                Self::unify_fields(
                    field_types,
                    other_field_types,
                    bias,
                    |name| e.with_context(TypeLoc::ObjectField { name })
                );
            }
            (this, other) => {
                error!(e, "can't unify {} and {}", this.kind().a_display(), other.kind().a_display());
            }
        }
    }
}

impl<Hole: Default + FatTypeHoleTrait> FatType<Hole> {
    pub fn hole() -> Self {
        Self::Hole {
            nullability: Nullability::NonNullable,
            hole: Hole::default()
        }
    }
}

impl<Hole: FatTypeHoleTrait> FatTypeInherited<Hole> {
    /// Also = `default()`
    pub const fn empty() -> Self {
        Self {
            super_ids: Vec::new(),
            structure: None,
            typescript_types: Vec::new(),
            guards: Vec::new()
        }
    }

    pub fn merge(&mut self, other: Self<>, e: TypeLogger<'_, '_, '_>) {
        FatType::unify_structure(&mut self.structure, other.structure, Variance::Bivariant, e);
        self.super_ids.extend(other.super_ids);
        self.typescript_types.extend(other.typescript_types);
        self.guards.extend(other.guards);
    }
}

impl PartialEq<FatTypeHole> for FatTypeHole {
    fn eq(&self, other: &FatTypeHole) -> bool {
        Rc::ptr_eq(&self.upper_bound, &other.upper_bound)
    }
}

impl Eq for FatTypeHole {}

impl<Hole: FatTypeHoleTrait> TypeParam<FatType<Hole>> {
    pub fn into_type(self, e: TypeLogger<'_, '_, '_>) -> FatType<Hole> {
        let inherited = FatType::collapse_supers(self.supers, e);
        FatType::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name: self.name,
                generic_args: Vec::new()
            },
            inherited: Box::new(inherited)
        }
    }
}

impl TypeParam<FatType> {
    pub fn into_decl(self, e: TypeLogger<'_, '_, '_>) -> FatTypeDecl {
        let inherited = FatType::collapse_supers(self.supers, e);
        FatTypeDecl {
            name: self.name,
            // No higher-kinded types
            type_params: Vec::new(),
            inherited: Box::new(inherited)
        }
    }
}

impl FatTypeHole {
    pub fn new() -> Self {
        Self {
            upper_bound: Rc::new(RefCell::new(LocalFatType::NEVER)),
        }
    }
}

impl Default for FatTypeHole {
    fn default() -> Self {
        Self::new()
    }
}

impl FatTypeHoleTrait for FatTypeHole {}

impl NoHole {
    pub fn unreachable<T>(self) -> T {
        match self {}
    }
}

impl FatTypeHoleTrait for NoHole {}