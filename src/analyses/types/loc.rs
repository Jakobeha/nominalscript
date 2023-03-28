use std::fmt::Display;
use std::ptr::NonNull;
use crate::analyses::bindings::ValueName;

/// Step to get from one type to one of its subtypes, or decl to supertypes
pub enum TypeLoc<'a> {
    Supertypes,
    SuperIdGeneric,
    SuperStructure,
    ArrayElem,
    TupleElem { index: usize },
    ObjectField { name: &'a ValueName },
    FunctionThisParam,
    FunctionParam { index: usize },
    FunctionRestParam,
    FunctionReturn,
}

/// Pointer equivalent of [TypeLoc]
#[derive(Debug, Clone, Copy)]
pub enum TypeLocPtr {
    Supertypes,
    SuperIds,
    SuperStructure,
    ArrayElem,
    TupleElem { index: usize },
    ObjectField { name: NonNull<ValueName> },
    FunctionThisParam,
    FunctionParam { index: usize },
    FunctionRestParam,
    FunctionReturn,
}

impl<'a> Display for TypeLoc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeLoc::Supertypes => write!(f, "supertypes"),
            TypeLoc::SuperIdGeneric => write!(f, "super id"),
            TypeLoc::SuperStructure => write!(f, "super structure"),
            TypeLoc::ArrayElem => write!(f, "array element"),
            TypeLoc::TupleElem { index } => write!(f, "element {}", index),
            TypeLoc::ObjectField { name } => write!(f, "field {}", name),
            TypeLoc::FunctionThisParam => write!(f, "this parameter"),
            TypeLoc::FunctionParam { index } => write!(f, "parameter {}", index),
            TypeLoc::FunctionRestParam => write!(f, "rest parameter"),
            TypeLoc::FunctionReturn => write!(f, "return type"),
        }
    }
}

impl<'a> TypeLoc<'a> {
    pub fn as_ptr(&self) -> TypeLocPtr {
        match *self {
            TypeLoc::Supertypes => TypeLocPtr::Supertypes,
            TypeLoc::SuperIdGeneric => TypeLocPtr::SuperIds,
            TypeLoc::SuperStructure => TypeLocPtr::SuperStructure,
            TypeLoc::ArrayElem => TypeLocPtr::ArrayElem,
            TypeLoc::TupleElem { index } => TypeLocPtr::TupleElem { index },
            TypeLoc::ObjectField { name } => TypeLocPtr::ObjectField { name: NonNull::from(name) },
            TypeLoc::FunctionThisParam => TypeLocPtr::FunctionThisParam,
            TypeLoc::FunctionParam { index } => TypeLocPtr::FunctionParam { index },
            TypeLoc::FunctionRestParam => TypeLocPtr::FunctionRestParam,
            TypeLoc::FunctionReturn => TypeLocPtr::FunctionReturn,
        }
    }
}

impl TypeLocPtr {
    // SAFETY: inner pointers must be alive and stay alive for the supplied lifetime
    pub unsafe fn as_ref<'a>(&self) -> TypeLoc<'a> {
        match *self {
            TypeLocPtr::Supertypes => TypeLoc::Supertypes,
            TypeLocPtr::SuperIds => TypeLoc::SuperIdGeneric,
            TypeLocPtr::SuperStructure => TypeLoc::SuperStructure,
            TypeLocPtr::ArrayElem => TypeLoc::ArrayElem,
            TypeLocPtr::TupleElem { index } => TypeLoc::TupleElem { index },
            TypeLocPtr::ObjectField { name } => TypeLoc::ObjectField { name: unsafe { name.as_ref() } },
            TypeLocPtr::FunctionThisParam => TypeLoc::FunctionThisParam,
            TypeLocPtr::FunctionParam { index } => TypeLoc::FunctionParam { index },
            TypeLocPtr::FunctionRestParam => TypeLoc::FunctionRestParam,
            TypeLocPtr::FunctionReturn => TypeLoc::FunctionReturn,
        }
    }
}