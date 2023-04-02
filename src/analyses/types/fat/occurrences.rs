use std::cell::Ref;
use std::cmp::Ordering;
use std::io::Read;
use std::iter::{empty, zip};
use auto_enums::auto_enum;
use smallvec::SmallVec;

use crate::analyses::bindings::{FieldName, TypeName};
use crate::analyses::types::{FatRestArgType, FatType, FatTypeHole, FatTypeInherited, Field, FnType, TypeIdent, TypeStructure};
use crate::misc::{iter_if, once_if, RefIterator};

/// Index path to an identifier inside a fat type (e.g. to get from `({ foo: Foo<Bar<Baz>>[] }) -> Void` to `Bar<Baz>`)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FatIdentIndexPath {
    pub(in crate::analyses::types::fat) rev_steps: SmallVec<[FatIdentIndexStep; 8]>,
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
        Self { rev_steps: SmallVec::new() }
    }

    /// Index path which follows `step` and then the remaining steps in `Self`
    pub fn prepend_step(&mut self, step: FatIdentIndexStep) {
        self.rev_steps.push(step)
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
    #[auto_enum(Iterator)]
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item = FatIdentIndexPath> {
        match self {
            Self::Any => empty(),
            Self::Never { nullability: _ } => empty(),
            Self::Structural { nullability: _, structure } => structure.occurrence_paths_of(name),
            Self::Nominal { nullability: _, id, inherited } => {
                id.occurrence_paths_of(name).chain(inherited.occurrence_paths_of(name))
            }
            Self::Hole { nullability: _, hole } => hole.occurrence_paths_of(name)
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
}

impl FatTypeInherited {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=FatIdentIndexPath> {
        // Remember: we only prepend the step in ident type-params, there is no step for supers
        self.super_ids.iter().flat_map(|super_id| super_id.occurrence_paths_of(name))
            .chain(self.structure.iter().flat_map(|structure| structure.occurrence_paths_of(name)))
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
}

impl FatTypeHole {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=FatIdentIndexPath> {
        RefIterator::new(Ref::map(hole.upper_bound.borrow(), |x| x.occurrence_paths_of(name)))
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
}

impl TypeIdent<FatType> {
    /// Paths to occurrences of the name (including in this type itself).
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=FatIdentIndexPath> {
        once_if(self.name == name, || FatOccurrence::new(&self.name))
            .chain(self.generic_args.iter().enumerate().flat_map(|(index, targ)|
                targ.occurrence_paths_of(name).map(|x|
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
}

impl TypeStructure<FatType> {
    /// Paths to occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    ///
    /// **Panics** if you are iterating within a type hole and then mutate the type hole in another
    /// instance (edge case which is very unlikely but still maybe possible to trigger in
    /// well-crafted source...)
    #[auto_enum(Iterator)]
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStep, FatIdentIndexPath)> {
        match self {
            Self::Fn { fn_type } => {
                fn_type.occurrence_paths_of(name).map(|(step, x)|
                    (FatIdentIndexStep::InFn(step), x))
            },
            Self::Array { element_type } => {
                element_type.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStep::InArray, x))
            },
            Self::Tuple { element_types } => {
                element_types.iter().enumerate().flat_map(|(index, element_type)|
                    element_type.occurrence_paths_of(name).map(|x|
                        (FatIdentIndexStep::InTuple { index }, x)))
            },
            Self::Object { field_types } => {
                field_types.iter().flat_map(|Field { name: field_name, type_ }|
                    type_.occurrence_paths_of(name).map(|x|
                        (FatIdentIndexStep::InObject { field_name: field_name.clone() }, x)))
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
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStepInFn, FatIdentIndexPath)> {
        iter_if(!self.shadows(name), || {
            self.type_params.iter().enumerate().map(|(index, type_param)|
                type_param.supers.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStepInFn::TypeParamSuper { index }, x)))
                .chain(self.this_type.occurrence_paths_of(name).map(|x|
                    (FatIdentIndexStepInFn::ThisArg, x)))
                .chain(self.arg_types.iter().enumerate().flat_map(|(index, arg_type)|
                    arg_type.occurrence_paths_of(name).map(|x|
                        (FatIdentIndexStepInFn::Arg { index }, x))))
                .chain(self.rest_arg_type.occurrence_paths_of(name).map(|(step, x)|
                    (FatIdentIndexStepInFn::RestArg(step)), x))
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
    pub fn occurrence_paths_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStepInRestArg, FatIdentIndexPath)> {
        match self {
            FatRestArgType::None => None,
            FatRestArgType::Array { element } => Some((FatIdentIndexStepInRestArg::ArrayElement, element)),
            FatRestArgType::Illegal { intended_type } => Some((FatIdentIndexStepInRestArg::IllegalIntendedType, intended_type))
        }.into_iter().flat_map(|(step, type_)| type_.occurrence_paths_of(name).map(|x| (step, x)))
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
}
