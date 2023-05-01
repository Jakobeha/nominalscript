use std::cmp::Ordering;
use std::collections::HashSet;
use std::iter::{empty, zip};
use log::warn;
use replace_with::replace_with_or_default;
use smallvec::SmallVec;

use crate::analyses::bindings::{FieldName, TypeName};
use crate::analyses::types::{FatRestArgType, FatType, FatTypeArg, FatTypeHole, FatTypeInherited, Field, FnType, OptionalType, ReturnType, IdentType, TypeLoc, TypeParam, StructureType};
use crate::diagnostics::TypeLogger;
use crate::{error, issue};
use crate::misc::{iter_if, once_if, RefIterator};

macro_rules! check_num_arguments {
    ($self:ident, $parameters:expr, $arguments:expr, $e:expr) => {
        if !_check_num_arguments($parameters, $arguments, &$e) {
            let len = usize::min($parameters.len(), $arguments.len());
            return $self.subst_parameters(&$parameters[..len], &$arguments[..len], $e);
        }
    };
}

macro_rules! impl_subst_parameters {
    ($($ref_:tt)?) => {
    /// Substitute type parameters with the arguments, and report any errors
    pub fn subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        e: $($ref_)? TypeLogger<'_, '_, '_>
    ) {
        check_num_arguments!(self, parameters, arguments, e);
        self._subst_parameters(parameters, arguments, &mut HashSet::new(), $($ref_)? e)
    }
    };
}

/// Index path to an identifier inside a fat type (e.g. to get from `({ foo: Foo<Bar<Baz>>[] }) -> Void` to `Bar<Baz>`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FatIdentIndexPath {
    rev_steps: SmallVec<[FatIdentIndexStep; 8]>,
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
    ThisParam,
    Param { index: usize },
    RestParam(FatIdentIndexStepInRestArg),
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
        Self { rev_steps: SmallVec::new() }
    }

    /// Index path which follows `step` and then the remaining steps in `Self`
    pub fn in_step(mut self, step: FatIdentIndexStep) -> Self {
        self.rev_steps.push(step);
        self
    }

    /// Iterates steps, from outermost to innermost
    pub fn steps(&self) -> impl Iterator<Item = &'_ FatIdentIndexStep> + '_ {
        self.rev_steps.iter().rev()
    }

    /// Number of steps
    pub fn num_steps(&self) -> usize {
        self.rev_steps.len()
    }
}

impl PartialOrd for FatIdentIndexPath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Compare steps from outermost to innermost, "lexicographically"
        zip(self.steps(), other.steps())
            .map(|(a, b)| a.partial_cmp(b))
            .find(|cmp| cmp != &Some(Ordering::Equal))
            .unwrap_or(self.num_steps().partial_cmp(&other.num_steps()))
    }
}

impl FatType {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        match self {
            Self::Any => {
                Box::new(empty()) as Box<dyn Iterator<Item = _>>
            },
            Self::Never { nullability: _ } => {
                Box::new(empty()) as Box<dyn Iterator<Item = _>>
            },
            Self::Structural { nullability: _, structure } => {
                Box::new(structure.occurrence_paths_of(name).map(|(step, x)| x.in_step(step))) as Box<dyn Iterator<Item = _>>
            },
            Self::Nominal { nullability: _, id, inherited } => {
                Box::new(id.occurrence_paths_of(name).chain(inherited.occurrence_paths_of(name))) as Box<dyn Iterator<Item = _>>
            }
            Self::Hole { nullability: _, hole } => {
                Box::new(hole.occurrence_paths_of(name)) as Box<dyn Iterator<Item = _>>
            }
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        match self {
            Self::Any => {},
            Self::Never { nullability: _ } => {},
            Self::Structural { nullability: _, structure } => structure.subst_name(old_name, new_name),
            Self::Nominal { nullability: _, id, inherited } => {
                id.subst_name(old_name, new_name);
                inherited.subst_name(old_name, new_name);
            }
            Self::Hole { nullability: _, hole } => hole.subst_name(old_name, new_name)
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        let mut is_never = false;
        match self {
            Self::Any => {},
            Self::Never { nullability: _ } => {},
            Self::Structural { nullability: _, structure } => structure.subst_name_with_never(name),
            Self::Nominal { nullability: _, id, inherited } => {
                if id.subst_name_with_never(name) {
                    is_never = true;
                    // Not worth it to subsitute more children, since we're already Never
                } else {
                    inherited.subst_name_with_never(name);
                    // Inherited might be set to Never
                    is_never = inherited.is_never;
                }
            }
            Self::Hole { nullability: _, hole } => hole.subst_name_with_never(name)
        }
        if is_never {
            *self = Self::NEVER;
        }
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        let mut new_base = None;
        match self {
            Self::Any => {},
            Self::Never { nullability: _ } => {},
            Self::Structural { nullability: _, structure } => structure._subst_parameters(parameters, arguments, shadows, e),
            Self::Nominal { nullability: _, id, inherited } => {
                inherited._subst_parameters(parameters, arguments, shadows, &e);
                new_base = id._subst_parameters(parameters, arguments, shadows, &e);
            }
            Self::Hole { nullability: _, hole } => hole._subst_parameters(parameters, arguments, shadows, e)
        }
        if let Some(new_base) = new_base {
            replace_with_or_default(self, |this| {
                let Self::Nominal { nullability, id: _, mut inherited } = this else {
                    unreachable!("new_base was set, but self is not Nominal")
                };
                // Don't log errors when extending type parameters' supertypes
                inherited.unify_with_super(new_base, TypeLogger::ignore());
                inherited.into_fat_type(nullability)
            })
        }
    }
}

impl FatTypeInherited {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        // Remember: we only prepend the step in ident type-params, there is no step for supers
        self.super_ids.iter().flat_map(|super_id| super_id.occurrence_paths_of(name))
            .chain(self.structure.iter().flat_map(|structure| structure.occurrence_paths_of(name))
                .map(|(step, x)| x.in_step(step)))
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        for super_id in &mut self.super_ids {
            super_id.subst_name(old_name, new_name);
        }
        for structure in &mut self.structure {
            structure.subst_name(old_name, new_name);
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        if self.is_never {
            // Not worth it to try since we are already never
            return
        }
        for super_id in &mut self.super_ids {
            if super_id.subst_name_with_never(name) {
                self.is_never = true;
                // Not worth it to substitute other types
                return
            }
        }
        for structure in &mut self.structure {
            structure.subst_name_with_never(name);
        }
    }

    impl_subst_parameters!(&);
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: &TypeLogger<'_, '_, '_>
    ) {
        let mut super_ids_to_remove = SmallVec::<[(usize, FatType); 1]>::new();
        for (index, super_id) in self.super_ids.iter_mut().enumerate() {
            if let Some(new_super) = super_id._subst_parameters(parameters, arguments, shadows, &e) {
                super_ids_to_remove.push((index, new_super));
            }
        }
        // Do it a bit faster this way since we are shifting a lot of stuff in the vector
        for (index, _) in super_ids_to_remove.iter().rev() {
            self.super_ids.remove(*index);
        }
        for (_, new_super) in super_ids_to_remove {
            // Don't log errors when extending type parameters' supertypes
            self.unify_with_super(new_super, TypeLogger::ignore());
        }
        for structure in &mut self.structure {
            structure._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::SuperStructure));
        }
    }
}

impl FatTypeHole {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        RefIterator::new(&self.upper_bound, |x| x.occurrence_paths_of(name))
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        self.upper_bound.borrow_mut().subst_name(old_name, new_name);
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        self.upper_bound.borrow_mut().subst_name_with_never(name);
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        self.upper_bound.borrow_mut()._subst_parameters(parameters, arguments, shadows, e);
    }
}

impl IdentType<FatType> {
    /// Paths to occurrences of the name (including in this type itself).
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        once_if(&self.name == name, FatIdentIndexPath::empty)
            .chain(self.generic_args.iter().enumerate().flat_map(move |(index, targ)|
                targ.occurrence_paths_of(name).map(move |x|
                    x.in_step(FatIdentIndexStep::InIdentTypeArg { index }))))
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        if self.name == *old_name {
            self.name = new_name.clone();
        }
        for targ in &mut self.generic_args {
            targ.subst_name(old_name, new_name);
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type,
    /// and return if this `name` matches the name to replace.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    #[must_use = "If this returns true, you must replace the outer type with Never"]
    pub fn subst_name_with_never(&mut self, name: &TypeName) -> bool {
        if self.name == *name {
            true
            // Not worth it to check children since this entire type is getting substituted
        } else {
            for targ in &mut self.generic_args {
                targ.subst_name_with_never(name);
            }
            false
        }
    }

    /// Substitute type parameters with the arguments, report any errors;
    /// and if this is one of the substituted parameters, return with the substituted type
    #[must_use = "If this returns Some, you must replace the outer type with the result"]
    pub fn subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        e: &TypeLogger<'_, '_, '_>
    ) -> Option<FatType> {
        check_num_arguments!(self, parameters, arguments, e);
        self._subst_parameters(parameters, arguments, &mut HashSet::new(), e)
    }

    #[must_use = "If this returns Some, you must replace the outer type with the result"]
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: &TypeLogger<'_, '_, '_>
    ) -> Option<FatType> {
        for (index, targ) in self.generic_args.iter_mut().enumerate() {
            targ._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::TypeArgument { index }));
        }

        // Do substitute if necessary
        parameters.iter().position(|x| x.name == self.name)
            .filter(|index| !shadows.contains(index))
            .map(|index| {
                if !self.generic_args.is_empty() {
                    // This is a higher-kinded type!
                    // (We still want to substitute targs even if we don't use them to report their
                    // errors as well)
                    error!(e, "Higher-kinded types are not supported")
                }
                let argument = &arguments[index];
                if argument.variance_bound != parameters[index].variance_bound {
                    warn!("arg/param variance mismatch for ident named {}", self.name);
                }
                argument.type_.clone()
            })
    }
}

impl StructureType<FatType> {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = (FatIdentIndexStep, FatIdentIndexPath)> + 'a {
        match self {
            Self::Fn { fn_type } => {
                Box::new(fn_type.occurrence_paths_of(name).map(|(step, x)|
                    (FatIdentIndexStep::InFn(step), x))) as Box<dyn Iterator<Item = _>>
            },
            Self::Array { element_type } => {
                Box::new(element_type.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStep::InArray, x))) as Box<dyn Iterator<Item = _>>
            },
            Self::Tuple { element_types } => {
                Box::new(element_types.iter().enumerate().flat_map(|(index, element_type)|
                    element_type.occurrence_paths_of(name).map(move |x|
                        (FatIdentIndexStep::InTuple { index }, x)))) as Box<dyn Iterator<Item = _>>
            },
            Self::Object { field_types } => {
                Box::new(field_types.iter().flat_map(|Field { name: field_name, type_ }|
                    type_.occurrence_paths_of(name).map(|x|
                        (FatIdentIndexStep::InObject { field_name: field_name.clone() }, x)))) as Box<dyn Iterator<Item = _>>
            }
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        match self {
            Self::Fn { fn_type } => fn_type.subst_name(old_name, new_name),
            Self::Array { element_type } => element_type.subst_name(old_name, new_name),
            Self::Tuple { element_types } => {
                for element_type in element_types {
                    element_type.subst_name(old_name, new_name);
                }
            },
            Self::Object { field_types } => {
                for Field { type_, .. } in field_types {
                    type_.subst_name(old_name, new_name);
                }
            }
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        match self {
            Self::Fn { fn_type } => fn_type.subst_name_with_never(name),
            Self::Array { element_type } => element_type.subst_name_with_never(name),
            Self::Tuple { element_types } => {
                for element_type in element_types {
                    element_type.subst_name_with_never(name);
                }
            },
            Self::Object { field_types } => {
                for Field { name: _field_name, type_ } in field_types {
                    type_.subst_name_with_never(name);
                }
            }
        }
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        match self {
            Self::Fn { fn_type } => fn_type._subst_parameters(parameters, arguments, shadows, e),
            Self::Array { element_type } => element_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::ArrayElement)),
            Self::Tuple { element_types } => {
                for (index, element_type) in element_types.iter_mut().enumerate() {
                    element_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::TupleElement { index }));
                }
            },
            Self::Object { field_types } => {
                for Field { name: field_name, type_ } in field_types {
                    type_._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::ObjectField { name: field_name.clone() }));
                }
            }
        }
    }
}

impl FnType<FatType> {
    /// Whether the function has a type parameter with the given name. If so, other occurrences of
    /// the name inside of the function refer to the type parameter and not any outside name.
    pub fn shadows(&self, name: &TypeName) -> bool {
        self.type_params.iter().any(|type_param| type_param.name == *name)
    }

    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences
    /// (this includes this type itself).
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = (FatIdentIndexStepInFn, FatIdentIndexPath)> + 'a {
        iter_if(!self.shadows(name), move || {
            self.type_params.iter().enumerate().flat_map(|(index, type_param)|
                type_param.supers.occurrence_paths_of(name).map(move |x|
                    (FatIdentIndexStepInFn::TypeParamSuper { index }, x)))
                .chain(self.this_type.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStepInFn::ThisParam, x)))
                .chain(self.arg_types.iter().enumerate().flat_map(|(index, arg_type)|
                    arg_type.occurrence_paths_of(name).map(move |x|
                        (FatIdentIndexStepInFn::Param { index }, x))))
                .chain(self.rest_arg_type.occurrence_paths_of(name).map(|(step, x)|
                    (FatIdentIndexStepInFn::RestParam(step), x)))
                .chain(self.return_type.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStepInFn::ReturnValue, x)))
        })
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected
    /// (this includes this type itself).
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        if !self.shadows(old_name) {
            for type_param in &mut self.type_params {
                type_param.supers.subst_name(old_name, new_name);
            }
            self.this_type.subst_name(old_name, new_name);
            for arg_type in &mut self.arg_types {
                arg_type.subst_name(old_name, new_name);
            }
            self.rest_arg_type.subst_name(old_name, new_name);
            self.return_type.subst_name(old_name, new_name);
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected
    /// (this includes this type itself).
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        if !self.shadows(name) {
            for type_param in &mut self.type_params {
                type_param.supers.subst_name_with_never(name);
            }
            self.this_type.subst_name_with_never(name);
            for arg_type in &mut self.arg_types {
                arg_type.subst_name_with_never(name);
            }
            self.rest_arg_type.subst_name_with_never(name);
            self.return_type.subst_name_with_never(name);
        }
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        // Shadow type parameters with the same name
        for type_param in &self.type_params {
            if let Some(shadowed) = parameters.iter().position(|parameter| type_param.name == parameter.name) {
                shadows.insert(shadowed);
            }
        }

        // Do the subst
        for type_param in &mut self.type_params {
            type_param.supers._subst_parameters(
                parameters,
                arguments,
                shadows,
                &e.with_context(TypeLoc::FunctionTypeParam { name: type_param.name.clone() })
            );
        }
        self.this_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::FunctionThisParam));
        for (index, arg_type) in self.arg_types.iter_mut().enumerate() {
            arg_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::FunctionParam { index }));
        }
        self.rest_arg_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::FunctionRestParam));
        self.return_type._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::FunctionReturn));

        // Unshadow type parameters with the same name
        for type_param in &self.type_params {
            if let Some(shadowed) = parameters.iter().position(|parameter| type_param.name == parameter.name) {
                shadows.remove(&shadowed);
            }
        }
    }
}

impl OptionalType<FatType> {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        self.type_.occurrence_paths_of(name)
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        self.type_.subst_name(old_name, new_name);
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        self.type_.subst_name_with_never(name);
    }


    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        self.type_._subst_parameters(parameters, arguments, shadows, e);
    }
}

impl ReturnType<FatType> {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = FatIdentIndexPath> + 'a {
        match self {
            ReturnType::Void => None,
            ReturnType::Type(type_) => Some(type_)
        }.into_iter().flat_map(|type_| type_.occurrence_paths_of(name))
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        match self {
            ReturnType::Void => {}
            ReturnType::Type(type_) => type_.subst_name(old_name, new_name),
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        match self {
            ReturnType::Void => {}
            ReturnType::Type(type_) => type_.subst_name_with_never(name),
        }
    }


    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        match self {
            ReturnType::Void => {}
            ReturnType::Type(type_) => type_._subst_parameters(parameters, arguments, shadows, e)
        }
    }
}

impl FatTypeArg {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences
    /// (this includes this type itself).
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item=FatIdentIndexPath> + 'a {
        self.type_.occurrence_paths_of(name)
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        self.type_.subst_name(old_name, new_name)
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        self.type_.subst_name_with_never(name)
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        self.type_._subst_parameters(parameters, arguments, shadows, e)
    }
}

impl FatRestArgType {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences
    /// (this includes this type itself).
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of<'a>(&'a self, name: &'a TypeName) -> impl Iterator<Item = (FatIdentIndexStepInRestArg, FatIdentIndexPath)> + 'a {
        match self {
            FatRestArgType::None => None,
            FatRestArgType::Array { element } => Some((FatIdentIndexStepInRestArg::ArrayElement, element)),
            FatRestArgType::Illegal { intended_type } => Some((FatIdentIndexStepInRestArg::IllegalIntendedType, intended_type))
        }.into_iter().flat_map(|(step, type_)| type_.occurrence_paths_of(name).map(move |x| (step.clone(), x)))
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        match self {
            FatRestArgType::None => {}
            FatRestArgType::Array { element } => element.subst_name(old_name, new_name),
            FatRestArgType::Illegal { intended_type } => intended_type.subst_name(old_name, new_name)
        }
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        match self {
            FatRestArgType::None => {}
            FatRestArgType::Array { element } => element.subst_name_with_never(name),
            FatRestArgType::Illegal { intended_type } => intended_type.subst_name_with_never(name)
        }
    }

    impl_subst_parameters!();
    fn _subst_parameters(
        &mut self,
        parameters: &[TypeParam<FatType>],
        arguments: &[FatTypeArg],
        shadows: &mut HashSet<usize>,
        e: TypeLogger<'_, '_, '_>
    ) {
        match self {
            FatRestArgType::None => {}
            FatRestArgType::Array { element } => element._subst_parameters(parameters, arguments, shadows, e.with_context(TypeLoc::ArrayElement)),
            FatRestArgType::Illegal { intended_type } => intended_type._subst_parameters(parameters, arguments, shadows, e)
        }
    }
}

fn _check_num_arguments(
    parameters: &[TypeParam<FatType>],
    arguments: &[FatTypeArg],
    e: &TypeLogger<'_, '_, '_>
) -> bool {
    if parameters.len() == arguments.len() {
        true
    } else {
        error!(e, "Wrong number of type arguments";
            issue!("the type has {} type parameters, but {} were given", parameters.len(), arguments.len()));
        false
    }
}