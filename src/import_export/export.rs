use std::collections::HashMap;
use std::sync::Arc;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{ResolveCtx, RlImportedTypeDecl, RlImportedValueType, RlType, RlTypeDecl};

use derive_more::{From, Into, AsRef, Deref, DerefMut};
use crate::compile::FinishTranspile;

#[derive(Debug)]
pub struct TranspileOutput {
    pub exports: Exports,
    pub source_code: String
}

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
#[derive(Debug, Default)]
pub struct TranspileOutHeader {
    pub exports: Exports,
    pub rest: FinishTranspile
}

#[derive(Debug, Default)]
pub struct Exports {
    module_id: ModuleId,
    values: HashMap<ValueName, RlType>,
    types: HashMap<TypeName, RlTypeDecl>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, AsRef, Deref, DerefMut)]
pub struct ModulePath(String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct ModuleId(usize);

impl TranspileOutHeader {
    pub fn finish(self, ctx: &mut ResolveCtx<'_>) -> TranspileOutput {
        TranspileOutput {
            exports: self.exports,
            source_code: self.rest.finish(ctx)
        }
    }
}

impl Exports {
    pub fn new(module_id: ModuleId) -> Exports {
        Exports {
            module_id,
            values: HashMap::new(),
            types: HashMap::new()
        }
    }

    pub fn add_value(&mut self, alias: ValueName, type_: &RlType) {
        self.values.insert(alias, type_.imported_from(self.module_id));
    }

    pub fn add_type(&mut self, alias: TypeName, decl: &RlTypeDecl) {
        self.types.insert(alias, decl.imported_from(self.module_id));
    }

    pub fn value_type(&self, name: &ValueName) -> Option<&RlType> {
        self.values.get(name)
    }

    pub fn type_decl(&self, name: &TypeName) -> Option<&RlTypeDecl> {
        self.types.get(name)
    }
}

impl ModuleId {
    pub const ZERO: ModuleId = ModuleId(0);

    pub fn next(self) -> ModuleId {
        ModuleId(self.0 + 1)
    }
}