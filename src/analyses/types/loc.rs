use std::fmt::Display;
use crate::analyses::bindings::{FieldName, TypeName};

/// Step to get from one type to one of its subtypes, or decl to supertypes
#[derive(Debug, Clone, Copy)]
pub enum TypeLoc {
    Supertype { index: usize },
    SuperIdGeneric { name: TypeName },
    SuperStructure,
    ArrayElem,
    TupleElem { index: usize },
    ObjectField { name: FieldName },
    FunctionThisParam,
    FunctionParam { index: usize },
    FunctionRestParam,
    FunctionReturn,
}

impl Display for TypeLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeLoc::Supertype { index } => write!(f, "supertype {}", index),
            TypeLoc::SuperIdGeneric { name } => write!(f, "super id type argument {}", name),
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