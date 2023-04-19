use std::collections::{HashSet, VecDeque};
use std::iter::zip;

use replace_with::{replace_with, replace_with_or_default};

use crate::{error, note};
use crate::analyses::bindings::TypeName;
use crate::analyses::types::{FatRestArgType, FatType, FatTypeHole, FatTypeInherited, Field, FnType, HasNullability, Nullability, Optionality, OptionalType, ReturnType, StructureKind, TypeIdent, TypeLoc, TypeParam, TypeStructure, Variance};
use crate::analyses::types::fat::occurrences::FatIdentIndexStepInFn;
use crate::diagnostics::TypeLogger;
use crate::misc::{iter_eq, rc_unwrap_or_clone, VecExtendNoDup, VecFilter};

impl FatType {
    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    ///
    /// Returns if the structures are disjoint *and* there's an intersection, a case which must be
    /// explicitly handled (if disjoint and union, will handle by making `this` `None`)
    #[must_use = "check if the unified structure is `Never`"]
    pub fn unify_structure(
        this: &mut Option<TypeStructure<Self>>,
        other: Option<TypeStructure<Self>>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
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
    #[must_use = "check if disjoint"]
    pub fn unify_structure2(
        this: &mut TypeStructure<Self>,
        mut other: TypeStructure<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
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
                    e.with_context(TypeLoc::ArrayElement)
                );
                false
            }
            (TypeStructure::Tuple { element_types }, TypeStructure::Tuple { element_types: other_element_types }) => {
                Self::unify_optionals(
                    "tuple element",
                    element_types,
                    other_element_types,
                    bias,
                    |index | e.with_context(TypeLoc::TupleElement { index })
                );
                false
            }
            (TypeStructure::Object { field_types }, TypeStructure::Object { field_types: other_field_types }) => {
                Self::unify_fields(
                    field_types,
                    other_field_types,
                    bias,
                    &e
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
            bias.reversed(),
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
    pub fn unify_generic_args(
        this: &mut Vec<Self>,
        other: Vec<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
    ) {
        if this.len() != other.len() {
            error!(e, "different number of generic arguments: {} and {}", this.len(), other.len());
            return;
        }
        for (index, (this, other)) in zip(this, other).enumerate() {
            Self::unify(this, other, bias, e.with_context(TypeLoc::TypeArgument { index }));
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
        let other_iter = other_regular.into_iter().chain(other_rest.into_iter().map(OptionalType::optional));
        Self::unify_optionals2(
            "parameter",
            (this_regular, Some(this_rest)),
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
        {
            let mut this_rest_iter = this_rest.iter().flat_map(|x| x.iter()).cloned().fuse();
            let mut other_iter = other_iter.fuse();
            for index in 0.. {
                let other = other_iter.next();
                if index >= this_vec.len() && other.is_some() {
                    if let Some(this_rest) = this_rest_iter.next() {
                        this_vec.push(OptionalType::optional(this_rest));
                    }
                }
                let this = this_vec.get_mut(index);

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
                if let Some(other) = push_to_this {
                    this_vec.push(other);
                }
            }
        }

        // Remove trailing "Never" args which can't actually be filled
        if let Some(this_rest) = this_rest {
            if matches!(this_rest, FatRestArgType::Array { element: FatType::NEVER }) {
                *this_rest = FatRestArgType::None
            }
        }
    }

    /// [Unifies](FatType::unify) `this` with `other` and logs subtype/disjoint errors based on `bias`.
    pub fn unify_optional(
        this: &mut OptionalType<Self>,
        other: OptionalType<Self>,
        bias: Variance,
        e: TypeLogger<'_, '_, '_>
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
        other: Vec<TypeParam<Self>>,
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

        let this_occurrences = this_type_params.iter().map(|this| {
            this_this_param.occurrence_paths_of(&this.name).map(|x| (FatIdentIndexStepInFn::ThisParam, x))
                .chain(this_params.iter().enumerate().flat_map(|(index, param)| param.occurrence_paths_of(&this.name).map(move |x| (FatIdentIndexStepInFn::Param { index }, x))))
                .chain(this_rest_param.occurrence_paths_of(&this.name).map(|(step, x)| (FatIdentIndexStepInFn::RestParam(step), x)))
                .chain(this_return.occurrence_paths_of(&this.name).map(|x| (FatIdentIndexStepInFn::ReturnValue, x)))
                .collect::<Vec<_>>()
        }).collect::<Vec<_>>();
        let other_occurrences = other_type_params.iter().map(|other| {
            other_this_param.occurrence_paths_of(&other.name).map(|x| (FatIdentIndexStepInFn::ThisParam, x))
                .chain(other_params.iter().enumerate().flat_map(|(index, param)| param.occurrence_paths_of(&other.name).map(move |x| (FatIdentIndexStepInFn::Param { index }, x))))
                .chain(other_rest_param.occurrence_paths_of(&other.name).map(|(step, x)| (FatIdentIndexStepInFn::RestParam(step), x)))
                .chain(other_return.occurrence_paths_of(&other.name).map(|x| (FatIdentIndexStepInFn::ReturnValue, x)))
                .collect::<Vec<_>>()
        }).collect::<Vec<_>>();

        let mut subst_this_with_never = move |this: &TypeParam<Self>| {
            // Subst name
            this_this_param.subst_name_with_never(&this.name);
            for this_param in this_params.iter_mut() {
                this_param.subst_name_with_never(&this.name);
            }
            this_rest_param.subst_name_with_never(&this.name);
            this_return.subst_name_with_never(&this.name);
        };
        let mut subst_other = move |other: &mut TypeParam<Self>, new_name: TypeName| {
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
        let mut i = 0;
        while i < other_type_params.len() {
            let remove = {
                let other = &mut other_type_params[i];
                let other_occurrences = &other_occurrences[i];
                if let Some(j) = this_occurrences.iter().position(|this_occurrences| iter_eq(this_occurrences, other_occurrences)) {
                    // Subst other with this
                    let this = &mut this_type_params[j];
                    if bias.do_union() {
                        this.variance_bound |= other.variance_bound
                    } else {
                        this.variance_bound &= other.variance_bound
                    }
                    // We are about to delete other anyways
                    let other_supers = *std::mem::take(&mut other.supers);
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
                other_type_params.remove(i);
            } else {
                i += 1;
            }
        }

        // Change other params with same name (and not same occurrences) as this params
        for i in 0..other_type_params.len() {
            if this_type_params.iter().any(|this_param| &this_param.name == &other_type_params[i].name) {
                // Need to switch name
                let other_param = &other_type_params[i];
                let new_name = TypeName::fresh(&other_param.name, |new_name| {
                    this_type_params.iter().any(|this_param| &this_param.name == new_name)
                        || other_type_params.iter().any(|other_param| &other_param.name == new_name)
                });
                let other_param = &mut other_type_params[i];
                subst_other(other_param, new_name);
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
    pub fn unify_fields(
        this: &mut Vec<Field<OptionalType<Self>>>,
        mut other: Vec<Field<OptionalType<Self>>>,
        bias: Variance,
        e: &TypeLogger<'_, '_, '_>
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
                        Self::unify_optional(&mut this_field.type_, other_field.type_, bias, e.with_context(TypeLoc::ObjectField { name }));
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
                        Self::unify_optional(&mut this_field.type_, other_field.type_, bias, e.with_context(TypeLoc::ObjectField { name }));
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
        e: &TypeLogger<'_, '_, '_>
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
                        Self::unify_generic_args(&mut this.generic_args, other.generic_args, bias, e.with_context(TypeLoc::SuperIdGeneric { name }));
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
                        Self::unify_generic_args(&mut this.generic_args, other.generic_args, bias, e.with_context(TypeLoc::SuperIdGeneric { name }));
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
        Self::unify_super_ids(&mut this.super_ids, other.super_ids, bias, &e);
        let is_never = FatType::unify_structure(&mut this.structure, other.structure, bias, e.with_context(TypeLoc::SuperStructure));
        if bias.do_union() {
            this.is_never &= other.is_never & is_never;
            this.typescript_types.retain(|t| other.typescript_types.contains(t));
            this.guards.retain(|g| other.guards.contains(g));
        } else {
            this.is_never |= other.is_never | is_never;
            this.typescript_types.extend_no_dup(other.typescript_types);
            this.guards.extend_no_dup(other.guards);
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
    pub fn unify(
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
        if self == &other {
            return
        }

        // Type holes, Any and Never cases (nullability already handled)
        let (this, other) = match (self, other) {
            (FatType::Hole { nullability: _, hole: this_hole }, other) => {
                this_hole.upper_bound.borrow_mut().unify(other, bias, e);
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
                this_hole.upper_bound.borrow_mut().unify(rc_unwrap_or_clone(other_hole.upper_bound).into_inner(), bias, e);
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
                    id: this_id,
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
        supers: impl IntoIterator<Item = Self>,
        e: TypeLogger<'_, '_, '_>
    ) -> FatTypeInherited {
        let mut inherited = FatTypeInherited::empty();
        for (index, super_) in supers.into_iter().enumerate() {
            inherited.unify_with_super(super_, e.with_context(TypeLoc::Supertype { index }));
        }
        inherited
    }

    /// [Unifies](FatType::unify_return) all types and logs subtype/disjoint errors based on `bias`.
    ///
    /// `bias` is transitive, so e.g. covariant bias would mean every types must be a subtype of
    /// types which come afterward.
    ///
    /// **Panics** if there are no types, since there should at least be the implicit void return
    /// if there are no return statements anywhere in-scope
    pub fn unify_all_returns(types: impl IntoIterator<Item=ReturnType<Self>>, bias: Variance, e: TypeLogger<'_, '_, '_>) -> ReturnType<Self> {
        let mut types = types.into_iter();
        let Some(mut result) = types.next() else {
            panic!("unify_all_returns called with no return types")
        };
        for (index, other) in types.enumerate() {
            Self::unify_return(&mut result, other, bias, e.with_context(TypeLoc::Position { index }));
        }
        result
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
            result.unify(other, bias, e.with_context(TypeLoc::Position { index }));
        }
        result
    }
}

impl FatTypeInherited {
    /// Make the [FatTypeInherited] extend the supertype and report and errors.
    pub fn unify_with_super(&mut self, super_: FatType, e: TypeLogger<'_, '_, '_>) {
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
                    &mut self.structure,
                    Some(structure),
                    Variance::Invariant,
                    e
                );
                if is_never {
                    self.is_never = true;
                }
            }
            FatType::Nominal { nullability, id: super_id, inherited: super_inherited } => {
                if nullability == Nullability::Nullable {
                    error!(e, "Can't extend nullable type";
                        note!("instead you must extend the non-nullable type and annotate null at all of your uses"));
                }
                self.super_ids.push_back(super_id);
                FatType::unify_inherited(
                    self,
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
}

