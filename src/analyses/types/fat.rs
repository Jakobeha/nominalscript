use crate::analyses::types::{Nullability, TypeIdent, TypeStructure};

/// Fat type = type after we've resolved the supertypes so that they are also in this structure,
/// and fat types can be compared / unified directly
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum FatType {
    #[default]
    Any,
    Never {
        nullability: Nullability,
    },
    Structural {
        nullability: Nullability,
        structure: TypeStructure<FatType>,
    },
    Nominal {
        nullability: Nullability,
        id: TypeIdent<FatType>,
        super_ids: Vec<TypeIdent<FatType>>,
        structure: Option<TypeStructure<FatType>>,
    }
}