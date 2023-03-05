use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{Nullability, TypeIdent, TypeParam, TypeStructure};
use crate::ast::tree_sitter::TSTree;

/// A nominal guard is a special function which is called after a nominal wrap expression,
/// and checks that the value is of the correct type.
/// If it returns false, the program will throw a TypeError.
#[derive(Debug, Clone)]
pub struct NominalGuard {
    /// The parameter binding
    pub param: ValueName,
    /// The body of the guard.
    pub body: TSTree
}

/// Type declaration after we've resolved the supertypes
#[derive(Debug, Clone)]
pub struct FatTypeDecl {
    type_params: Vec<TypeParam<FatType>>,
    type_: FatType,
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
        typescript_type: Option<TSTree>,
        guard: Option<NominalGuard>,
    }
}

impl FatType {
    pub fn never() -> Self {
        Self::Never { nullability: Nullability::NonNullable }
    }

    pub fn null() -> Self {
        Self::Never { nullability: Nullability::Nullable }
    }

    pub fn nullable(self) -> Self {
        match self {
            Self::Never { .. } => self,
            Self::Any => Self::Never { nullability: Nullability::Nullable },
            Self::Structural { nullability: _, structure } => Self::Structural {
                nullability: Nullability::Nullable,
                structure
            },
            Self::Nominal { nullability: _, id, super_ids, structure, typescript_type, guard } => Self::Nominal {
                nullability: Nullability::Nullable,
                id,
                super_ids,
                structure,
                typescript_type,
                guard,
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

impl TypeParam<FatType> {
    pub fn into_type(self) -> FatType {
        let (super_ids, structure, typescript_type, guard) = FatType::collapse_supers(self.supers);
        FatType::Nominal {
            nullability: Nullability::NonNullable,
            id: TypeIdent {
                name: self.name,
                generic_args: Vec::new()
            },
            super_ids,
            structure,
            typescript_type,
            guard
        }
    }

    pub fn into_decl(self) -> FatTypeDecl {
        FatTypeDecl {
            // No higher-kinded types
            type_params: Vec::new(),
            type_: self.into_type(),
        }
    }
}