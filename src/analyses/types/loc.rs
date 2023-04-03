use std::fmt::Display;
use crate::analyses::bindings::{FieldName, TypeName};

/// Step to get from one type to one of its subtypes, or decl to supertypes
#[derive(Debug, Clone)]
pub enum TypeLoc {
    Supertype { index: usize },
    SuperIdGeneric { name: TypeName },
    SuperStructure,
    TypeArgument { index: usize },
    FunctionTypeParam { name: TypeName },
    FunctionThisParam,
    FunctionParam { index: usize },
    FunctionRestParam,
    FunctionReturn,
    ArrayElement,
    TupleElement { index: usize },
    ObjectField { name: FieldName },
    /// For misc position
    Position { index: usize },
}

impl Display for TypeLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeLoc::Supertype { index } => write!(f, "supertype {}", index),
            TypeLoc::SuperIdGeneric { name } => write!(f, "super id type argument {}", name),
            TypeLoc::SuperStructure => write!(f, "super structure"),
            TypeLoc::TypeArgument { index } => write!(f, "type argument {}", index),
            TypeLoc::ArrayElement => write!(f, "array element"),
            TypeLoc::TupleElement { index } => write!(f, "element {}", index),
            TypeLoc::ObjectField { name } => write!(f, "field {}", name),
            TypeLoc::FunctionTypeParam { name } => write!(f, "type parameter {}", name),
            TypeLoc::FunctionThisParam => write!(f, "this parameter"),
            TypeLoc::FunctionParam { index } => write!(f, "parameter {}", index),
            TypeLoc::FunctionRestParam => write!(f, "rest parameter"),
            TypeLoc::FunctionReturn => write!(f, "return type"),
            TypeLoc::Position { index } => write!(f, "position {}", index),
        }
    }
}