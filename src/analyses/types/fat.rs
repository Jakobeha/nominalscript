use crate::analyses::bindings::ValueName;
use crate::analyses::types::{Nullability, TypeIdent, TypeParam, TypeStructure};
use crate::ast::tree_sitter::TSTree;

/// A nominal guard is a special function which is called after a nominal wrap expression,
/// and checks that the value is of the correct type.
/// If it returns false, the program will throw a TypeError.
pub struct NominalGuard {
    /// The parameter binding
    pub param: ValueName,
    /// The body of the guard.
    pub body: TSTree
}

/// Type declaration after we've resolved the supertypes
pub struct FatTypeDecl {
    type_params: Vec<TypeParam<FatType>>,
    type_: FatType,
    guard: NominalGuard
}

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

impl FatType {
    pub fn nullable(self) -> Self {
        match self {
            Self::Never { .. } => self,
            Self::Any => Self::Never { nullability: Nullability::Nullable },
            Self::Structural { nullability: _, structure } => Self::Structural {
                nullability: Nullability::Nullable,
                structure
            },
            Self::Nominal { nullability: _, id, super_ids, structure } => Self::Nominal {
                nullability: Nullability::Nullable,
                id,
                super_ids,
                structure
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
}