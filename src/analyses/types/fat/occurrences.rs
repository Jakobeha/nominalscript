use std::io::Read;
use std::iter::empty;
use auto_enums::auto_enum;

use crate::analyses::bindings::TypeName;
use crate::analyses::types::{FatIdentIndexPath, FatIdentIndexStepInRestArg, FatRestArgType, FatType, FatTypeInherited, Field, FnType, TypeIdent, TypeStructure};
use crate::analyses::types::fat::data::{FatIdentIndexStep, FatIdentIndexStepInFn};
use crate::misc::{iter_if, once_if};

impl FatType {
    /// Replace all occurrences of `name` in this type (identifiers) with the new name.
    ///
    /// Type params of those identifiers are preserved (subst occurs in them as well).
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name(&mut self, old_name: &TypeName, new_name: &TypeName) {
        todo!()
    }

    /// Replace all occurrences of `name` in this type (identifiers) with the never type.
    ///
    /// Type params of those identifiers are deleted.
    /// Function types which declare a parameter shadowing the old name aren't affected.
    pub fn subst_name_with_never(&mut self, name: &TypeName) {
        todo!()
    }

    /// Occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    #[auto_enum(Iterator)]
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item = FatIdentIndexPath> {
        match self {
            Self::Any => empty(),
            Self::Never { nullability: _ } => empty(),
            Self::Structural { nullability: _, structure } => structure.occurrences_of(name),
            Self::Nominal { nullability: _, id, inherited } => {
                id.occurrences_of(name).chain(inherited.occurrences_of(name))
            }
            Self::Hole { nullability: _, hole } => hole.upper_bound.borrow().occurrences_of(name)
        }
    }
}

impl FatTypeInherited {
    /// Occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item=FatIdentIndexPath> {
        // Remember: we only prepend the step in ident type-params, there is no step for supers
        self.super_ids.iter().flat_map(|super_id| super_id.occurrences_of(name))
            .chain(self.structure.iter().flat_map(|structure| structure.occurrences_of(name)))
    }
}

impl TypeIdent<FatType> {
    /// Occurrences of the name (including in this type itself).
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item=FatIdentIndexPath> {
        once_if(self.name == name, FatIdentIndexPath::empty)
            .chain(self.generic_args.iter().enumerate().flat_map(|(index, targ)|
                targ.occurrences_of(name).map(|x|
                    x.prepend_step(FatIdentIndexStep::InIdentTypeArg { index }))))
    }
}

impl TypeStructure<FatType> {
    /// Occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences.
    #[auto_enum(Iterator)]
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStep, FatIdentIndexPath)> {
        match self {
            Self::Fn { fn_type } => {
                fn_type.occurrences_of(name).map(|(step, x)|
                    (FatIdentIndexStep::InFn(step), x))
            },
            Self::Array { element_type } => {
                element_type.occurrences_of(name).map(|x|
                    (FatIdentIndexStep::InArray, x))
            },
            Self::Tuple { element_types } => {
                element_types.iter().enumerate().flat_map(|(index, element_type)|
                    element_type.occurrences_of(name).map(|x|
                        (FatIdentIndexStep::InTuple { index }, x)))
            },
            Self::Object { field_types } => {
                field_types.iter().flat_map(|Field { name: field_name, type_ }|
                    type_.occurrences_of(name).map(|x|
                        (FatIdentIndexStep::InObject { field_name: field_name.clone() }, x)))
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

    /// Occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences
    /// (this includes this type itself).
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStepInFn, FatIdentIndexPath)> {
        iter_if(!self.shadows(name), || {
            self.type_params.iter().enumerate().map(|(index, type_param)|
                type_param.supers.occurrences_of(name).map(|x|
                    (FatIdentIndexStepInFn::TypeParamSuper { index }, x)))
                .chain(self.this_type.occurrences_of(name).map(|x|
                    (FatIdentIndexStepInFn::ThisArg, x)))
                .chain(self.arg_types.iter().enumerate().flat_map(|(index, arg_type)|
                    arg_type.occurrences_of(name).map(|x|
                        (FatIdentIndexStepInFn::Arg { index }, x))))
                .chain(self.rest_arg_type.occurrences_of(name).map(|(step, x)|
                    (FatIdentIndexStepInFn::RestArg(step)), x))
                .chain(self.return_type.occurrences_of(name).map(|x|
                    (FatIdentIndexStepInFn::ReturnValue, x)))
        })
    }
}

impl FatRestArgType {
    /// Occurrences of the name
    ///
    /// Function types which declare a parameter shadowing the old name don't have occurrences
    /// (this includes this type itself).
    pub fn occurrences_of(&self, name: &TypeName) -> impl Iterator<Item=(FatIdentIndexStepInRestArg, FatIdentIndexPath)> {
        match self {
            FatRestArgType::None => None,
            FatRestArgType::Array { element } => Some((FatIdentIndexStepInRestArg::ArrayElement, element)),
            FatRestArgType::Illegal { intended_type } => Some((FatIdentIndexStepInRestArg::IllegalIntendedType, intended_type))
        }.into_iter().flat_map(|(step, type_)| type_.occurrences_of(name).map(|x| (step, x)))
    }
}