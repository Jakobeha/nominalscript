use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::io::Read;
use std::iter::{once, repeat};
use std::rc::Rc;

use replace_with::{replace_with, replace_with_or_default};

use crate::analyses::bindings::{FieldName, TypeName};
use crate::analyses::types::{Field, FnType, HasNullability, impl_structural_type_constructors, NominalGuard, Nullability, Optionality, OptionalType, ReturnType, StructureKind, ThinType, TypeIdent, TypeLoc, TypeParam, TypeStructure, TypeTrait, TypeTraitMapsFrom, Variance};
use crate::ast::tree_sitter::SubTree;
use crate::diagnostics::TypeLogger;
use crate::{error, note};
use crate::misc::{once_if, VecFilter};

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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
enum FatRestArgKind {
    Any,
    Tuple,
    Array,
    Illegal
}

#[derive(Debug, Clone)]
pub struct FatTypeHole {
    upper_bound: Rc<RefCell<FatType>>
}

struct TypeParamSubsts {
    /// Type parameters (nominal types) with the name at index `i` (if `Some`)
    /// must be replaced with the actual type parameter name at index `i`.
    old_names_to_replace: Vec<Option<TypeName>>
}

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

impl FatType {
    pub const NEVER: Self = Self::Never { nullability: Nullability::NonNullable };

    pub const NULL: Self = Self::Never { nullability: Nullability::Nullable };

    impl_structural_type_constructors!();

    pub fn hole() -> Self {
        Self::Hole {
            nullability: Nullability::NonNullable,
            hole: FatTypeHole::new()
        }
    }

    /// Make nullable. Makes type holes' upper bound nullable, but not outer nullable.
    pub fn make_nullable_intrinsically(&mut self) {
        match self {
            Self::Any => {},
            Self::Never { nullability } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Hole { nullability: _, hole } => {
                hole.upper_bound.borrow_mut().make_internally_nullable();
            },
        }
    }

    /// Make non-nullable. Make type holes' upper bound non-nullable *and* outer non-nullable
    pub fn make_non_nullable_intrinsically(&mut self) {
        match self {
            Self::Any => {},
            Self::Never { nullability } => *nullability = Nullability::NonNullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::NonNullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::NonNullable,
            Self::Hole { nullability, hole } => {
                *nullability = Nullability::NonNullable;
                hole.upper_bound.borrow_mut().make_internally_non_nullable();
            },
        }
    }

    /// Structure kind, if there is a structure, otherwise `None`
    pub fn structure_kind(&self) -> Option<StructureKind> {
        match self {
            Self::Any => None,
            Self::Never { nullability: _ } => None,
            Self::Structural { nullability: _, structure } => Some(structure.kind()),
            Self::Nominal { nullability: _, id: _, inherited } => inherited.structure.map(|s| s.kind()),
            Self::Hole { nullability: _, hole } => hole.upper_bound.borrow().structure_kind()
        }
    }

    /// Mutable references to ids and structure
    ///
    /// No nullability because you can only `make_nullable`, otherwise problems arise from type
    /// holes.
    pub fn mut_ids_and_structure(&mut self) -> (impl Iterator<Item=&mut TypeIdent<FatType>>, Option<&mut TypeStructure<FatType>>) {
        // Handle type holes first because...
        if let Self::Hole { nullability: _, hole } = self {
            return hole.upper_bound.get_mut().mut_ids_and_structure()
        }
        // ...use Some(iterable) and flatten so that we don't have to use auto_enum or something else
        let (ids, structure) = match self {
            Self::Any => (None, None),
            Self::Never { nullability: _ } => (None, None),
            Self::Structural { nullability: _, structure } => (None, Some(structure)),
            Self::Nominal { nullability: _, id, inherited } => {
                (Some(once(id).chain(inherited.super_ids.iter_mut())), inherited.structure.as_mut())
            },
            Self::Hole { nullability: _, hole: _ } => unreachable!("Should've been handled in prior if")
        };
        (ids.into_iter().flatten(), structure)
    }

    /// Destructs this and returns its nullability, ids and structure
    pub fn into_nullability_ids_and_structure(self) -> (Nullability, Vec<TypeIdent<FatType>>, Option<TypeStructure<FatType>>) {
        match self {
            Self::Any => (Nullability::Nullable, Vec::new(), None),
            Self::Never { nullability } => (nullability, Vec::new(), None),
            Self::Structural { nullability, structure } => (nullability, Vec::new(), Some(structure)),
            Self::Nominal { nullability, id, inherited } => {
                let mut ids = vec![id];
                ids.extend(inherited.super_ids.into_iter());
                (nullability, ids, inherited.structure)
            },
            Self::Hole { nullability, hole } => {
                let (inner_nullability, ids, structure) = hole.upper_bound.into_inner().into_nullability_ids_and_structure();
                (nullability | inner_nullability, ids, structure)
            }
        }
    }

    /// Destructs this and returns its ids
    pub fn into_ids(self) -> Vec<TypeIdent<FatType>> {
        let (_, ids, _) = self.into_nullability_ids_and_structure();
        ids
    }

    /// Destructs this and returns its structure
    pub fn into_structure(self) -> Option<TypeStructure<FatType>> {
        let (_, _, structure) = self.into_nullability_ids_and_structure();
        structure
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Returns if the structures are disjoint *and* there's an intersection, a case which must be
    /// explicitly handled (if disjoint and union, will handle by making `this` `None`)
    #[must_use("check if the unified structure is `Never`")]
    pub fn unify_structure(
        this: &mut Option<TypeStructure<Self>>,
        other: Option<TypeStructure<Self>>,
        bias: Variance,
        mut e: TypeLogger<'_, '_, '_>
    ) -> bool {
        let Some(other2) = other else {
            if !bias.is_contravariant() {
                error!(e, "assigned type input must be a structure but required type does not");
            }
            return false
        };
        let Some(this2) = this else {
            if !bias.is_covariant() {
                error!(e, "assigned type has no structure but required type does");
            }
            *this = Some(other2);
            return false
        };

        let is_disjoint = Self::unify_structure2(this2, other2, bias, e);

        if is_disjoint && bias.do_union() {
            *this = None;
            false
        } else {
            is_disjoint
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Returns if the structures are disjoint, so the union is `Any` and intersection is `Never`.
    #[must_use("check if disjoint")]
    pub fn unify_structure2(
        this: &mut TypeStructure<Self>,
        mut other: TypeStructure<Self>,
        bias: Variance,
        mut e: TypeLogger<'_, '_, '_>
    ) -> bool {
        // Do whole structure conversions to get equivalent kinds of possible
        match (this.kind(), other.kind()) {
            (StructureKind::Tuple, StructureKind::Array) => {
                if !bias.is_covariant() {
                    error!(e, "an array is not a subtype of a tuple");
                }

                if bias.do_union() {
                    replace_with(this, || TypeStructure::Array { element_type: Box::default() }, |this| {
                        let TypeStructure::Tuple { element_types } = this else { unreachable!() };
                        let element_types = element_types.into_iter().map(|x| x.collapse_optionality_into_nullability());
                        TypeStructure::Array {
                            element_type: Box::new(FatType::unify_all(element_types, Variance::Bivariant, TypeLogger::ignore()))
                        }
                    });
                } else {
                    let len = match &this {
                        TypeStructure::Tuple { element_types } => element_types.len(),
                        _ => unreachable!()
                    };
                    let TypeStructure::Array { element_type } = other else { unreachable!() };
                    other = TypeStructure::Tuple {
                        element_types: vec![OptionalType::optional(*element_type); len]
                    }
                }
            }
            (StructureKind::Array, StructureKind::Tuple) => {
                if !bias.is_contravariant() {
                    error!(e, "a tuple is not a supertype of an array");
                }
                if bias.do_union() {
                    let TypeStructure::Tuple { element_types } = other else { unreachable!() };
                    let element_types = element_types.into_iter().map(|x| x.collapse_optionality_into_nullability());
                    other = TypeStructure::Array {
                        element_type: Box::new(FatType::unify_all(element_types, Variance::Bivariant, TypeLogger::ignore()))
                    }
                } else {
                    let len = match &other {
                        TypeStructure::Tuple { element_types } => element_types.len(),
                        _ => unreachable!()
                    };
                    replace_with(this, || TypeStructure::Tuple { element_types: Vec::default() }, |this| {
                        let TypeStructure::Array { element_type } = this else { unreachable!() };
                        TypeStructure::Tuple {
                            element_types: vec![OptionalType::optional(*element_type); len]
                        }
                    });
                }
            }
            _ => {}
        }
        // Merge if equivalent kinds or log error
        match (this, other) {
            (TypeStructure::Fn { fn_type }, TypeStructure::Fn { fn_type: other_fn_type }) => {
                Self::unify_fn(fn_type, *other_fn_type, bias, e);
                false
            }
            (TypeStructure::Array { element_type }, TypeStructure::Array { element_type: other_element_type }) => {
                Self::unify(
                    element_type,
                    *other_element_type,
                    bias,
                    e.with_context(TypeLoc::ArrayElem)
                );
                false
            }
            (TypeStructure::Tuple { element_types }, TypeStructure::Tuple { element_types: other_element_types }) => {
                Self::unify_optionals(
                    "tuple element",
                    element_types,
                    other_element_types,
                    bias,
                    |index | e.with_context(TypeLoc::TupleElem { index })
                );
                false
            }
            (TypeStructure::Object { field_types }, TypeStructure::Object { field_types: other_field_types }) => {
                Self::unify_fields(
                    field_types,
                    other_field_types,
                    bias,
                    |name| e.with_context(TypeLoc::ObjectField { name })
                );
                false
            }
            (this, other) => {
                error!(e, "different structure-kinds: {} and {}", this.kind().a_display(), other.kind().a_display());
                true
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_fn(
        this: &mut FnType<Self>,
        mut other: FnType<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        Self::unify_type_parameters(
            &mut this.type_params,
            other.type_params,
            (&mut this.this_type, &mut this.arg_types, &mut this.rest_arg_type, &mut this.return_type),
            (&mut other.this_type, &mut other.arg_types, &mut other.rest_arg_type, &mut other.return_type),
            bias,
        );
        Self::unify(
            &mut this.this_type,
            other.this_type,
            bias.reversed(),
            e.with_context(TypeLoc::FunctionThisParam)
        );
        Self::unify_regular_and_rest_parameters(
            (&mut this.arg_types, &mut this.rest_arg_type),
            (other.arg_types, other.rest_arg_type),
            bias.reversed(),
            |index| e.with_context(TypeLoc::FunctionParam { index })
        );
        Self::unify_return(
            &mut this.return_type,
            other.return_type,
            bias,
            e.with_context(TypeLoc::FunctionReturn)
        );
        this.remove_and_inline_type_params_where_possible();
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_return(
        this: &mut ReturnType<Self>,
        other: ReturnType<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        match (this, other) {
            (ReturnType::Void, ReturnType::Void) => {}
            (ReturnType::Type(this), ReturnType::Type(other)) => {
                Self::unify(this, other, bias, e);
            }
            (this @ ReturnType::Void, other @ ReturnType::Type(_)) => {
                error!(e, "assigned returns void but required returns a type");
                if !bias.do_union() {
                    *this = other;
                }
            },
            (this @ ReturnType::Type(_), other @ ReturnType::Void) => {
                error!(e, "assigned returns a type but required returns void");
                if bias.do_union() {
                    *this = other;
                }
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_optionals<'a, 'b: 'a, 'tree: 'a>(
        elem_desc: &'static str,
        this: &mut Vec<OptionalType<Self>>,
        other: Vec<OptionalType<Self>>,
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        Self::unify_optionals2(
            elem_desc,
            (this, None),
            other.into_iter(),
            bias,
            e
        );
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// This unifies both the parameter types (optional types) and rest parameter types. However, the
    /// rest types will affect the parameter types' unification if there is an array rest type, and
    /// its corresponding parameter type list doesnt't have as much elements as the other parameter
    /// type list. Also be aware that the function's `this` parameter types and type parameters must
    /// be unified, but in separate functions, since they are not affected by regular or rest
    /// parameter types (well, type parameters are, but [Self::unify_type_parameters] handles this).
    pub fn unify_regular_and_rest_parameters<'a, 'b: 'a, 'tree: 'a>(
        this: (&mut Vec<OptionalType<Self>>, &mut FatRestArgType),
        other: (Vec<OptionalType<Self>>, FatRestArgType),
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        let (this_regular, this_rest) = this;
        let (other_regular, other_rest) = other;
        let other_iter = other_regular.into_iter().chain(other_rest.into_iter());
        Self::unify_optionals2(
            "parameter",
            (this_regular, Some(this_rest.iter())),
            other_iter,
            bias,
            e
        );
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_optionals2<'a, 'b: 'a, 'tree: 'a>(
        elem_desc: &'static str,
        this: (&mut Vec<OptionalType<Self>>, Option<&mut FatRestArgType>),
        other_iter: impl Iterator<Item = OptionalType<Self>>,
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        let (this_vec, this_rest) = this;
        let mut this_rest_iter = this_rest.into_iter().flat_map(|x| x.iter()).cloned().fuse();
        let mut other_iter = other_iter.fuse();
        for i in 0.. {
            let other = other_iter.next();
            if i >= this.len() && other.is_some() {
                if let Some(this_rest) = this_rest_iter.next() {
                    this.push(OptionalType::optional(this_rest));
                }
            }
            let this = this_vec.get_mut(i);

            let push_to_this = match (this, other) {
                (None, None) => break,
                (Some(this), Some(other)) => {
                    Self::unify_optional(this, other, bias, e(index));
                    None
                }
                // None = ?Never, but better error message by checking directly
                (Some(this), None) => {
                    // Remember: bias is already reversed from fn, so bias = contravariant <=> fn bias = covariant
                    if !bias.is_contravariant() {
                        error!(e(index), "assigned has more {}s than required", elem_desc);
                    }
                    this.optionality = Optionality::from(bias.do_union());
                    None
                }
                (None, Some(mut other)) => {
                    // Remember: bias is already reversed from fn, so bias = covariant <=> fn bias = contravariant
                    if !bias.is_covariant() {
                        error!(e(index), "assigned has less {}s than required", elem_desc);
                    }
                    other.optionality = Optionality::from(bias.do_union());
                    Some(other)
                }
            };

            // Remove trailing "Never" args which can't actually be filled
            if let Some(this_rest) = this_rest {
                if matches!(this_rest, FatRestArgType::Array { element: FatType::NEVER }) {
                    *this_rest = FatRestArgType::None
                }
            }
            if let Some(other) = push_to_this {
                if let Some(this_rest) = this_rest {
                    *this_rest = FatRestArgType::None;
                }
                this_vec.push(other);
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_optional<'a, 'b: 'a, 'tree: 'a>(
        this: &mut OptionalType<Self>,
        other: OptionalType<Self>,
        bias: Variance,
        e: impl Fn(usize) -> TypeLogger<'a, 'b, 'tree>
    ) {
        if this.optionality > other.optionality && !bias.is_covariant() {
            error!(e, "assigned is not optional but required is");
        } else if other.optionality < this.optionality && !bias.is_contravariant() {
            error!(e, "assigned is optional but required is not");
        }
        if bias.do_union() {
            this.optionality |= other.optionality;
        } else {
            this.optionality &= other.optionality;
        }
        Self::unify(&mut this.type_, other.type_, bias, e);
    }

    //noinspection DuplicatedCode
    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Type parameter unification involves substituting `other` parameters with the same occurrences
    /// as `this` parameters with the `this` parameter's name, and substituting the `other` parameters
    /// with the same names (but different occurrences) as `this` parameters with fresh ones. Thus we
    /// also take `other`'s regular parameter types and return type. At the end of type parameter
    /// unification, both type parameters are appended together. At the end of function unification,
    /// we remove type parameters with no occurrences, and inline type parameters whose only
    /// occurrences are supertypes of other parameters.
    pub fn unify_type_parameters<'a, 'b: 'a, 'tree: 'a>(
        this: &mut Vec<TypeParam<Self>>,
        mut other: Vec<TypeParam<Self>>,
        (this_this_param, this_params, this_rest_param, this_return): (
            &mut Self,
            &mut Vec<OptionalType<Self>>,
            &mut FatRestArgType,
            &mut ReturnType<Self>
        ),
        (other_this_param, other_params, other_rest_param, other_return): (
            &mut Self,
            &mut Vec<OptionalType<Self>>,
            &mut FatRestArgType,
            &mut ReturnType<Self>
        ),
        bias: Variance,
    ) {
        let this_type_params = this;
        let mut other_type_params = other;

        let subst_this_with_never = move |this: &TypeParam<Self>| {
            // Subst name
            this_this_param.subst_name_with_never(&this.name);
            for this_param in this_params.iter_mut() {
                this_param.subst_name_with_never(&this.name);
            }
            this_rest_param.subst_name_with_never(&this.name);
            this_return.subst_name_with_never(&this.name);
        };
        let subst_other = move |other: &mut TypeParam<Self>, new_name: TypeName| {
            // Subst name
            other_this_param.subst_name(&other.name, &new_name);
            for other_param in other_params.iter_mut() {
                other_param.subst_name(&other.name, &new_name);
            }
            other_rest_param.subst_name(&other.name, &new_name);
            other_return.subst_name(&other.name, &new_name);
            // Change name
            other.name = new_name;
        };

        // Remove and subst other params with the same occurrences as this params, and merge
        // the bounds
        let this_occurrences = this_type_params.iter().map(|this| {
            this_this_param.occurrences_of(this)
                .chain(this_params.iter().flat_map(|param| param.occurrences_of(this)))
                .chain(this_rest_param.occurrences_of(this))
                .chain(this_return.occurrences_of(this))
                .collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        let mut i = 0;
        while i < other_type_params.len() {
            let remove = {
                let other = &mut other_type_params[i];
                let other_occurrences = other_this_param.occurrences_of(other)
                    .chain(other_params.iter().flat_map(|param| param.occurrences_of(other)))
                    .chain(other_rest_param.occurrences_of(other))
                    .chain(other_return.occurrences_of(other));
                if let Some(j) = this_occurrences.iter().position(|this_occurrences| this_occurrences == other_occurrences) {
                    // Subst other with this
                    let this = &mut this_type_params[j];
                    if bias.do_union() {
                        this.variance_bound |= other.variance_bound
                    } else {
                        this.variance_bound &= other.variance_bound
                    }
                    // We are about to delete other anyways
                    let other_supers = std::mem::take(&mut other.supers);
                    // TODO: Subst all with Never type if applicable
                    Self::unify_inherited(&mut this.supers, other_supers, bias, TypeLogger::ignore());
                    let is_never = this.supers.is_never;
                    if is_never {
                        subst_this_with_never(this);
                    } else {
                        subst_other(other, this.name.clone());
                    }
                    if is_never {
                        this_type_params.remove(j);
                    }
                    true
                } else {
                    false
                }
            };
            if remove {
                other_type_params.remove(i)
            } else {
                i += 1
            }
        }

        // Change other params with same name (and not same occurrences) as this params
        for other in other_type_params.iter_mut() {
            if this_type_params.iter().any(|this_param| this_param.name == other.name) {
                // Need to switch name
                let new_name = TypeName::fresh(&other.name, |new_name| {
                    this_type_params.iter().any(|this_param| this_param.name == new_name)
                        || other.iter().any(|other| other.name == new_name)
                });
                subst_other(other, new_name);
            }
        }

        // Add other params (without same occurrences, and some with changed names)
        this_type_params.extend(other_type_params);
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// In the subtype-checking field order isn't important and field keys are unified like sets.
    /// However, this still tries to preserve field order in `this` as much as possible for better
    /// user-facing support.
    pub fn unify_fields<'a, 'b: 'a, 'tree: 'a>(
        this: &mut Vec<Field<OptionalType<Self>>>,
        mut other: Vec<Field<OptionalType<Self>>>,
        bias: Variance,
        e: impl Fn(FieldName) -> TypeLogger<'a, 'b, 'tree>
    ) {
        // Check for subtype/supertype issues from names. Subtype = more fields
        let iter_this_field_names = || this.iter().map(|field| &field.name);
        let iter_other_field_names = || other.iter().map(|field| &field.name);
        if !bias.is_covariant() {
            let this_field_names = iter_this_field_names().collect::<HashSet<_>>();
            for other_field_name in iter_other_field_names().filter(|other_field_name| !this_field_names.contains(other_field_name)) {
                error!(e, "missing field `{}`", other_field_name)
            }
        }
        if !bias.is_contravariant() {
            let other_field_names = iter_other_field_names().collect::<HashSet<_>>();
            for this_field_name in iter_this_field_names().filter(|this_field_name| !other_field_names.contains(this_field_name)) {
                error!(e, "extra field `{}`", this_field_name)
            }
        }

        // Unify and check for same-field subtype/supertype issues
        if bias.do_union() {
            this.retain_mut(|this_field| {
                match other.find_remove(|other_field| this_field.name == other_field.name) {
                    None => false,
                    Some(other_field) => {
                        let name = other_field.name;
                        Self::unify_optional(&mut this_field.type_, other_field.type_, bias, e(name));
                        true
                    }
                }
            });
        } else {
            this.reserve(other.len());
            for other_field in other {
                let push_other_field = match this.iter_mut().find(|this_field| this_field.name == other_field.name) {
                    None => Some(other_field),
                    Some(this_field) => {
                        let name = other_field.name;
                        Self::unify_optional(&mut this_field.type_, other_field.type_, bias, e(name));
                        None
                    }
                };
                if let Some(push_other_field) = push_other_field {
                    this.push(push_other_field)
                }
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Note that the unify "intersection" contains *all* super-ids, which is actually a union
    /// (inherit intersection = inherit all - remember that the more types are inherited, the less
    /// instances exist, and a type union has more instances).
    // This function is essentially exact same as unify_fields...is there a good way to abstract?
    pub fn unify_super_ids(
        this: &mut VecDeque<TypeIdent<FatType>>,
        mut other: VecDeque<TypeIdent<FatType>>,
        bias: Variance,
        e: impl Fn(TypeName) -> TypeLogger<'_, '_, '_>
    ) {
        // Check for subtype/supertype issues from names. Subtype = more identifiers
        let iter_this_super_ids = || this.iter().map(|id| &id.name);
        let iter_other_super_ids = || other.iter().map(|id| &id.name);
        if !bias.is_covariant() {
            let this_super_ids = iter_this_super_ids().collect::<HashSet<_>>();
            for other_super_id in iter_other_super_ids().filter(|other_super_id| !this_super_ids.contains(other_super_id)) {
                error!(e, "missing supertype `{}`", other_super_id)
            }
        }
        if !bias.is_contravariant() {
            let other_super_ids = iter_other_super_ids().collect::<HashSet<_>>();
            for this_super_id in iter_this_super_ids().filter(|this_super_id| !other_super_ids.contains(this_super_id)) {
                error!(e, "extra supertype `{}`", this_super_id)
            }
        }

        // Unify and check for same-field subtype/supertype issues
        if bias.do_union() {
            this.retain_mut(|this| {
                match other.find_remove(|other| this.name == other.name) {
                    None => false,
                    Some(other) => {
                        let name = other.name;
                        Self::unify_generic_args(&mut this.generic_args, other.generic_args, bias, e(name));
                        true
                    }
                }
            });
        } else {
            this.reserve(other.len());
            for other in other {
                let push_other = match this.iter_mut().find(|this| this.name == other.name) {
                    None => Some(other),
                    Some(this) => {
                        let name = other.name;
                        Self::unify_generic_args(&mut this.generic_args, other.generic_args, bias, e(name));
                        None
                    },
                };
                if let Some(other) = push_other {
                    this.push_back(other);
                }
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Note that an "intersection" of inherited types contains *all* inherited supertypes (inherit
    /// intersection = inherit `this` and `other`), while a "union" of inherited types contains
    /// *only common* inherited supertypes (inherit union = inherit `this` or `other`).
    pub fn unify_inherited(this: &mut FatTypeInherited, other: FatTypeInherited, bias: Variance, e: TypeLogger<'_, '_, '_>) {
        Self::unify_super_ids(&mut this.super_ids, other.super_ids, bias, |name| e.with_context(TypeLoc::SuperIdGeneric { name }));
        let is_never = FatType::unify_structure(&mut this.structure, other.structure, bias, e.with_context(TypeLoc::SuperStructure));
        if bias.do_union() {
            this.is_never &= other.is_never & is_never;
            this.typescript_types.retain(|t| other.typescript_types.contains(t));
            this.guards.retain(|g| other.guards.contains(g));
        } else {
            this.is_never |= other.is_never | is_never;
            this.typescript_types.extend(other.typescript_types.into_iter().filter(|g| !this.typescript_types.contains(g)));
            this.guards.extend(other.guards.into_iter().filter(|g| !this.guards.contains(g)));
        }
    }

    /// **Unifies** `self` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// The unification is either union or intersection depending on `bias`:
    /// - If `bias` is bivariant or covariant, `self` will be mutated into the union type
    ///   `self ⋃ other`: a value is an instance of this type if it's an instance of the original
    ///   `self` *or* `other`.
    /// - If `bias` is contravariant or invariant, `self` will be mutated into the intersection type
    ///  `self ⋂ other`: a value is an instance of this type if it's an instance of the original
    ///   `self` *and* `other`.
    ///
    /// Besides creating the union orintersection type, this also logs "type mismatch" errors
    /// depending on `bias:
    ///
    /// - If `bias` is covariant, then `self` must be a subtype of `other` (i.e. all instances of
    ///   `self` must also be instances of `other`).
    /// - If `bias` is contravariant, then `self` must be a supertype of `other` (i.e. all instances
    ///   of `other` must also be instances of `self`).
    /// - If `bias` is invariant, `self` must be equivalent to `other` (i.e. all instances of `self`
    ///   must also be instances of `other` and vice versa).
    /// - If `bias` is bivariant, `self` must be a subtype *or* supertype of `other` (i.e. there
    ///   must exist at least one possible value which is an instance of both `self` and `other`,
    ///   *if* there exists at least one possible value which is an instance of `self` *and* there
    ///   exists at least one possible value which is an instance of `other`).
    ///
    /// If you are only creating a union and don't care about type mismatches, even if `self` and
    /// `other` are disjoint, you can provide `TypeLogger::ignore` for `e`, which skips logging.
    fn unify(
        &mut self,
        other: Self,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        // Check nullability
        let original_nullability = self.nullability();
        let original_other_nullability = other.nullability();
        match (self.nullability(), other.nullability()) {
            (Nullability::Nullable, Nullability::Nullable) |
            (Nullability::NonNullable, Nullability::NonNullable) => (),
            (Nullability::Nullable, Nullability::NonNullable) => {
                if !bias.is_covariant() {
                    error!(e, "assigned a nullable type but required a non-nullable type");
                }
                if !bias.do_union() {
                    self.make_non_nullable_intrinsically()
                }
            }
            (Nullability::NonNullable, Nullability::Nullable) => {
                if !bias.is_contravariant() {
                    error!(e, "assigned a non-nullable type but required a nullable type");
                }
                if bias.do_union() {
                    self.make_nullable_intrinsically()
                }
            }
        }

        // Easy case
        if self == other {
            return
        }

        // Type holes, Any and Never cases (nullability already handled)
        let (this, other) = match (self, other) {
            (FatType::Hole { nullability: _, hole: this_hole }, other) => {
                this_hole.upper_bound.get_mut().unify(other, bias, e);
                return
            }
            (this, FatType::Hole { nullability: _, hole: other_hole }) => {
                replace_with(this, FatType::hole, |this| FatType::Hole {
                    nullability: Nullability::NonNullable,
                    hole: FatTypeHole::from(this),
                });
                // Could call this.unify(other_hole.upper_bound.into_inner(), bias, e),
                //     but it just does this but slower and with redundancies
                let FatType::Hole { nullability: _, hole: this_hole } = this else {
                    unreachable!("replace_with should have replaced this with a hole")
                };
                this_hole.upper_bound.get_mut().unify(other_hole.upper_bound.into_inner(), bias, e);
                return
            }
            (this @ FatType::Any, other) => {
                if !bias.is_contravariant() {
                    error!(e, "assigned Any but required a specific type");
                }
                if !bias.do_union() {
                    *this = other;
                }
                return
            },
            (this, other @ FatType::Any) => {
                if !bias.is_covariant() {
                    error!(e, "assigned a specific type but required Any");
                }
                if bias.do_union() {
                    *this = other;
                }
                return
            }
            (this @ FatType::Never { nullability: _ }, other) => {
                if !bias.is_covariant() {
                    error!(e, "{}", match original_nullability {
                        Nullability::NonNullable => "assigned Never but required an inhabited type",
                        Nullability::Nullable => "assigned Null but required a tyoe inhabited by more than just null"
                    });
                }
                if bias.do_union() {
                    *this = other;
                }
                return
            }
            (this, other @ FatType::Never { nullability: _ }) => {
                if !bias.is_contravariant() {
                    error!(e, "{}", match original_other_nullability {
                        Nullability::NonNullable => "assigned an inhabited type but required Never",
                        Nullability::Nullable => "assigned a type inhabited by more than just null but required Null"
                    });
                }
                if !bias.do_union() {
                    *this = other;
                }
                return
            }
            (this, other) => (this, other)
        };

        // Non-trivial cases
        replace_with_or_default(this, |this| {
            match (this, other) {
                (FatType::Structural {
                    nullability,
                    structure: mut this_structure
                }, FatType::Structural {
                    nullability: _,
                    structure: other_structure
                }) => {
                    let is_disjoint = Self::unify_structure2(&mut this_structure, other_structure, bias, e);
                    if is_disjoint {
                        if bias.do_union() {
                            FatType::Any
                        } else {
                            FatType::Never { nullability }
                        }
                    } else {
                        FatType::Structural {
                            nullability,
                            structure: this_structure
                        }
                    }
                },
                (FatType::Nominal {
                    nullability,
                    id: mut this_id,
                    inherited: mut this_inherited
                }, FatType::Structural {
                    nullability: _,
                    structure: other_structure
                }) => {
                    if !bias.is_covariant() {
                        error!(e, "assigned a nominal type but required a structural type");
                    }
                    if bias.do_union() {
                        let false = Self::unify_structure(
                            &mut this_inherited.structure,
                            Some(other_structure),
                            bias,
                            e
                        ) else {
                            unreachable!("unify_structure always returns false when bias.do_union() is true")
                        };
                        if let Some(structure) = this_inherited.structure {
                            FatType::Structural {
                                nullability,
                                structure
                            }
                        } else {
                            FatType::Any
                        }
                    } else {
                        let is_never = Self::unify_structure(
                            &mut this_inherited.structure,
                            Some(other_structure),
                            bias,
                            e
                        );
                        if is_never {
                            FatType::Never { nullability }
                        } else {
                            FatType::Nominal {
                                nullability,
                                id: this_id,
                                inherited: this_inherited
                            }
                        }
                    }
                }
                (FatType::Structural {
                    nullability,
                    structure: this_structure
                }, FatType::Nominal {
                    nullability: _,
                    id: other_id,
                    inherited: other_inherited
                }) => {
                    if !bias.is_contravariant() {
                        error!(e, "assigned a structural type but required a nominal type");
                    }
                    let mut this_structure = Some(this_structure);
                    if bias.do_union() {
                        let false = Self::unify_structure(
                            &mut this_structure,
                            other_inherited.structure,
                            bias,
                            e
                        ) else {
                            unreachable!("unify_structure always returns false when bias.do_union() is true")
                        };
                        if let Some(structure) = this_structure {
                            FatType::Structural {
                                nullability,
                                structure
                            }
                        } else {
                            FatType::Any
                        }
                    } else {
                        let is_never = Self::unify_structure(
                            &mut this_structure,
                            other_inherited.structure,
                            bias,
                            e
                        );
                        if is_never {
                            FatType::Never { nullability }
                        } else {
                            FatType::Nominal {
                                nullability,
                                id: other_id,
                                inherited: Box::new(FatTypeInherited {
                                    super_ids: other_inherited.super_ids,
                                    structure: this_structure,
                                    typescript_types: other_inherited.typescript_types,
                                    guards: other_inherited.guards,
                                    is_never: other_inherited.is_never,
                                })
                            }
                        }
                    }
                }
                (FatType::Nominal {
                    nullability,
                    id: this_id,
                    inherited: mut this_inherited
                }, FatType::Nominal {
                    nullability: _,
                    id: other_id,
                    inherited: mut other_inherited
                }) => {
                    this_inherited.super_ids.push_front(this_id);
                    other_inherited.super_ids.push_front(other_id);
                    Self::unify_inherited(&mut this_inherited, *other_inherited, bias, e);
                    if this_inherited.is_never {
                        FatType::Never { nullability }
                    } else if let Some(id) = this_inherited.super_ids.pop_front() {
                        FatType::Nominal {
                            nullability,
                            id,
                            inherited: this_inherited
                        }
                    } else if let Some(structure) = this_inherited.structure {
                        if !this_inherited.guards.is_empty() || !this_inherited.typescript_types.is_empty() {
                            log::error!("type has no nominal id but guards and typescript types? This shouldn't be possible! (structural type)");
                        }
                        FatType::Structural {
                            nullability,
                            structure
                        }
                    } else {
                        if !this_inherited.guards.is_empty() || !this_inherited.typescript_types.is_empty() {
                            log::error!("type has no nominal id but guards and typescript types? This shouldn't be possible! (Any type)");
                        }
                        FatType::Any
                    }
                },
                (_, _) => unreachable!("unhandled FatType variant combination, should've been handled in above blocks")
            }
        });
    }

    /// Create a [FatTypeInherited] which is the inherits all `supers`.
    ///
    /// This is [FatTypeInherited] because it will be the inherited of a type declaration or
    /// parameter, which is a nominal type with its own id. Also, the returned value may not have
    /// any structure or identifiers if `supers` is empty.
    ///
    /// "bias" is always `Invariant`. This is the *intersection* of `supers`. Remember, intersection
    /// = less instances inhabit, and the more inherited types, the less instances inhabit, because
    /// an inhabited instance must be an instance of all inherited types.
    pub fn unify_all_supers(
        supers: impl IntoIterator<Item=Self>,
        mut e: TypeLogger<'_, '_, '_>
    ) -> FatTypeInherited {
        let mut inherited = FatTypeInherited::empty();
        for (index, super_) in supers.enumerate() {
            let e = e.with_context(TypeLoc::Supertype { index });
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
                    let is_never = FatType::unify_structure(
                        &mut inherited.structure,
                        Some(structure),
                        Variance::Invariant,
                        e
                    );
                    if is_never {
                        inherited.is_never = true;
                    }
                }
                FatType::Nominal { nullability, id: super_id, inherited: super_inherited } => {
                    if nullability == Nullability::Nullable {
                        error!(e, "Can't extend nullable type";
                            note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                    }
                    inherited.super_ids.push(super_id);
                    Self::unify_inherited(
                        &mut inherited,
                        *super_inherited,
                        Variance::Invariant,
                        e
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

    /// [Unifies](FatType::unify) all types and logs subtype/disjoint errors based on `bias`.
    ///
    /// `bias` is transitive, so e.g. covariant bias would mean every types must be a subtype of
    /// types which come afterward.
    ///
    /// If there are no types, returns `FatType::Never` for union, and `FatType::Any` for intersection
    /// (remember forall ∅ = True and exists ∅ = False)
    pub fn unify_all(types: impl IntoIterator<Item=Self>, bias: Variance, e: TypeLogger<'_, '_, '_>) -> Self {
        let mut types = types.into_iter();
        let Some(mut result) = types.next() else {
            return if bias.do_union() {
                FatType::NEVER
            } else {
                FatType::Any
            }
        };
        for (index, other) in types.enumerate() {
            result = result.unify(other, bias, e.with_context(TypeLoc::Position { index }));
        }
        result
    }
}

impl HasNullability for FatType {
    fn nullability(&self) -> Nullability {
        match self {
            Self::Any => Nullability::Nullable,
            Self::Never { nullability } => *nullability,
            Self::Structural { nullability, .. } => *nullability,
            Self::Nominal { nullability, .. } => *nullability,
            Self::Hole { nullability, hole } => *nullability | || hole.upper_bound.borrow().nullability()
        }
    }

    fn make_nullable(&mut self) {
        match self {
            Self::Any => {},
            Self::Never { nullability } => *nullability = Nullability::Nullable,
            Self::Structural { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Nominal { nullability, .. } => *nullability = Nullability::Nullable,
            Self::Hole { nullability, hole: _ } => *nullability = Nullability::Nullable,
        }
    }
}

impl TypeTrait for FatType {
    type Inherited = FatTypeInherited;
    type RestArgType = FatRestArgType;

    fn map_inherited(inherited: Self::Inherited, f: impl FnMut(Self) -> Self) -> Self::Inherited {
        inherited.map(f)
    }

    fn map_ref_inherited(inherited: &Self::Inherited, f: impl FnMut(&Self) -> Self) -> Self::Inherited {
        inherited.map_ref(f)
    }

    fn map_rest_arg_type(rest_arg_type: Self::RestArgType, f: impl FnMut(Self) -> Self) -> Self::RestArgType {
        rest_arg_type.map(f)
    }

    fn map_ref_rest_arg_type(rest_arg_type: &Self::RestArgType, f: impl FnMut(&Self) -> Self) -> Self::RestArgType {
        rest_arg_type.map_ref(f)
    }
}

impl TypeTraitMapsFrom<ThinType> for FatType {
    fn map_inherited(inherited: ThinType::Inherited, f: impl FnMut(ThinType) -> Self) -> Self::Inherited {
        let inherited = inherited.into_iter().map(f).collect();
        Self::unify_all_supers(inherited, TypeLogger::ignore())
    }

    fn map_ref_inherited(inherited: &ThinType::Inherited, f: impl FnMut(&ThinType) -> Self) -> Self::Inherited {
        let inherited = inherited.iter().map(f).collect();
        Self::unify_all_structures(inherited, Bias::Invariant, TypeLogger::ignore())
    }

    fn map_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: ThinType,
        return_type: ReturnType<Self>,
        f: impl FnMut(ThinType) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(rest_arg_type);
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        mut arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &ThinType,
        return_type: ReturnType<Self>,
        f: impl FnMut(&ThinType) -> Self
    ) -> FnType<Self> {
        let intended_rest_arg_type = f(rest_arg_type);
        let rest_arg_type = FatRestArgType::from(intended_rest_arg_type, &mut arg_types);
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type,
            return_type
        }
    }
}

impl TypeTraitMapsFrom<FatType> for ThinType {
    fn map_inherited(inherited: FatType::Inherited, f: impl FnMut(FatType) -> Self) -> Self::Inherited {
        inherited.into_bare_fat_types().map(f).collect()
    }

    fn map_ref_inherited(inherited: &FatType::Inherited, f: impl FnMut(Cow<'_, FatType>) -> Self) -> Self::Inherited {
        inherited.clone().into_bare_fat_types().map(|x| f(Cow::Owned(f))).collect()
    }

    fn map_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: FatRestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(FatType) -> Self
    ) -> FnType<Self> {
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type: f(rest_arg_type.into()),
            return_type
        }
    }

    fn map_ref_rest_arg_type_in_fn(
        type_params: Vec<TypeParam<Self>>,
        this_type: Self,
        arg_types: Vec<OptionalType<Self>>,
        rest_arg_type: &FatRestArgType,
        return_type: ReturnType<Self>,
        f: impl FnMut(Cow<'_, FatType>) -> Self
    ) -> FnType<Self> {
        FnType {
            type_params,
            this_type,
            arg_types,
            rest_arg_type: f(rest_arg_type.as_cow()),
            return_type
        }
    }
}

impl FatRestArgType {
    /// Creates from the given [FatType].
    ///
    /// Nominal info is stripped, only the structure if it's `Any` or a tuple or array matters:
    ///
    /// - If it's a tuple, this will return [FatRestArgType::None] but append the elems to the end
    ///   of the args array.
    /// - If it's an array, this will return [FatRestArgType::Array] with the element type,
    ///   meaning the function can take zero or more extre elements of this type
    /// - If it's `Any`, that is equivalent to an array of `Any` (can take extra elements of any type)
    /// - Otherwise, this will return [FatRestArgType::Illegal], which will log an error later if
    ///   the type is actually used somewhere.
    pub fn from(intended_type: FatType, arg_types: &mut Vec<OptionalType<FatType>>) -> Self {
        let kind = FatRestArgKind::of(&intended_type);
        match kind {
            FatRestArgKind::Any => Self::Array { element: FatType::Any },
            FatRestArgKind::Tuple => {
                let mut elements = intended_type.into_structure().and_then(|x| x.into_tuple_element_types())
                    .expect("FatRestArgType::from: intended_type is not a tuple but FatRestArgKind::of returned FatRestArgKind::Tuple");
                arg_types.append(&mut elements);
                Self::None
            }
            FatRestArgKind::Array => {
                let element = intended_type.into_structure().and_then(|x| x.into_array_element_type())
                    .expect("FatRestArgType::from: intended_type is not an array but FatRestArgKind::of returned FatRestArgKind::Array");
                Self::Array { element }
            }
            FatRestArgKind::Illegal => Self::Illegal { intended_type }
        }
    }

    //Can't really abstract (I mean we could proc-macro this whole map/map_ref thing...)
    //noinspection DuplicatedCode
    pub fn map(self, f: impl FnMut(FatType) -> FatType) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { element } => Self::Array { element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }

    //noinspection DuplicatedCode
    pub fn map_ref(&self, f: impl FnMut(&FatType) -> FatType) -> Self {
        match self {
            Self::None => Self::None,
            Self::Array { element } => Self::Array { element: f(element) },
            Self::Illegal { intended_type } => Self::Illegal { intended_type: f(intended_type) }
        }
    }

    /// Iterates the rest arguments: returns infinite element types if this is an array, otherwise empty
    pub fn iter(&self) -> impl Iterator<Item = &FatType> {
        // Use option/flatten to avoid auto_enum
        match self {
            Self::None => None,
            Self::Array { element } => Some(repeat(element)),
            Self::Illegal { intended_type: _ } => None
        }.into_iter().flatten()
    }

    /// Iterates the rest arguments: returns infinite element types if this is an array, otherwise empty
    pub fn into_iter(self) -> impl Iterator<Item = FatType> {
        // Use option/flatten to avoid auto_enum
        match self {
            Self::None => None,
            Self::Array { element } => Some(repeat(element)),
            Self::Illegal { intended_type: _ } => None
        }.into_iter().flatten()
    }

    /// Converts into a [FatType]. If possible thie will use a static or internal reference.
    pub fn as_cow(&self) -> Cow<'_, FatType> {
        match self {
            Self::None => Cow::Borrowed(&FatType::EMPTY_REST_ARG),
            Self::Array { element } => Cow::Owned(FatType::array(element.clone())),
            Self::Illegal { intended_type } => Cow::Borrowed(intended_type)
        }
    }
}

impl Into<FatType> for FatRestArgType {
    fn into(self) -> FatType {
        match self {
            Self::None => FatType::EMPTY_REST_ARG,
            Self::Array { element } => FatType::array(element),
            Self::Illegal { intended_type } => intended_type
        }
    }
}

impl FatRestArgKind {
    pub fn of(intended_type: &FatType) -> Self {
        if matches!(intended_type, FatType::Any) {
            return Self::Any;
        }
        match intended_type.structure_kind() {
            Some(StructureKind::Tuple) => Self::Tuple,
            Some(StructureKind::Array) => Self::Array,
            _ => Self::Illegal
        }
    }
}

impl FatTypeInherited {
    pub const EMPTY: Self = Self::empty();

    /// Also = `default()`
    pub const fn empty() -> Self {
        Self {
            super_ids: VecDeque::new(),
            structure: None,
            typescript_types: Vec::new(),
            guards: Vec::new(),
            is_never: false
        }
    }

    pub fn map(self, f: impl FnMut(FatType) -> FatType) -> Self {
        Self {
            super_ids: self.super_ids,
            structure: self.structure.map(|x| x.map(f)),
            typescript_types: self.typescript_types,
            guards: self.guards,
            is_never: self.is_never
        }
    }

    pub fn map_ref(&self, f: impl FnMut(&FatType) -> FatType) -> Self {
        Self {
            super_ids: self.super_ids.clone(),
            structure: self.structure.as_ref().map(|x| x.map_ref(f)),
            typescript_types: self.typescript_types.clone(),
            guards: self.guards.clone(),
            is_never: self.is_never
        }
    }

    /// Converts into bare fat types, i.e. without the inherited parts.
    /// Just a type for never, each identifier, and each structure.
    /// These types convert into thin types but should not be used anything else,
    /// as they have ids with incorrect supertypes.
    fn into_bare_fat_types(self) -> impl Iterator<Item=FatType> {
        once_if(self.is_never, FatType::NEVER)
            .chain(self.super_ids.into_iter().map(|id| FatType::Nominal {
                nullability: Nullability::NonNullable,
                id,
                inherited: Box::default()
            }))
            .chain(self.structure.map(|structure| FatType::Structural {
                nullability: Nullability::NonNullable,
                structure
            }).into_iter())
    }
}


impl FatTypeHole {
    pub fn new() -> Self {
        Self {
            upper_bound: Rc::new(RefCell::new(FatType::NEVER)),
        }
    }
}

impl PartialEq<FatTypeHole> for FatTypeHole {
    fn eq(&self, other: &FatTypeHole) -> bool {
        Rc::ptr_eq(&self.upper_bound, &other.upper_bound)
    }
}

impl Eq for FatTypeHole {}

impl Default for FatTypeHole {
    fn default() -> Self {
        Self::new()
    }
}

impl From<FatType> for FatTypeHole {
    fn from(t: FatType) -> Self {
        Self {
            upper_bound: Rc::new(RefCell::new(t))
        }
    }
}

impl TypeParamSubsts {
    pub fn new() -> Self {
        Self {
            old_names_to_replace: Vec::new()
        }
    }

    pub fn push(&mut self, index: usize, subst: Option<TypeName>) {
        if let Some(subst) = subst {
            while self.old_names_to_replace.len() < index {
                self.old_names_to_replace.push(None);
            }
            self.old_names_to_replace.push(Some(subst));
        }
    }
}

impl TypeParam<FatType> {
    pub fn into_type(self, e: TypeLogger<'_, '_, '_>) -> FatType {
        let inherited = FatType::unify_all_supers(self.supers, e);
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
        let inherited = FatType::unify_all_supers(self.supers, e);
        FatTypeDecl {
            name: self.name,
            // No higher-kinded types
            type_params: Vec::new(),
            inherited: Box::new(inherited)
        }
    }
}