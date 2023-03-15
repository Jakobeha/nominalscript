use std::collections::HashMap;
use std::sync::Arc;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{DynResolvedLazy, DynRlType, DynRlTypeDecl, ResolveCtx, RlImportedTypeDecl, RlImportedValueType};

use derive_more::{From, Into, AsRef, Deref, DerefMut};
use crate::compile::FinishTranspile;

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, AsRef, Deref, DerefMut)]
pub struct ModulePath(String);


#[derive(Debug, Default)]
pub struct Exports {
    values: HashMap<ValueName, Box<DynRlType>>,
    types: HashMap<TypeName, Box<DynRlTypeDecl>>
}

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
#[derive(Debug, Default)]
pub struct TranspileOutHeader {
    pub exports: Exports,
    pub rest: FinishTranspile
}

#[derive(Debug)]
pub struct TranspileOutput {
    pub exports: Exports,
    pub source_code: String
}

impl Exports {
    pub fn new() -> Exports {
        Exports {
            values: HashMap::new(),
            types: HashMap::new()
        }
    }

    pub fn add_value(&mut self, alias: ValueName, type_: &DynRlType) {
        self.values.insert(alias, Box::new(type_.clone()));
    }

    pub fn add_type(&mut self, alias: TypeName, decl: &DynRlTypeDecl) {
        self.types.insert(alias, Box::new(decl.clone()));
    }

    pub fn value(&self, name: &ValueName) -> Option<&DynRlType> {
        self.values.get(name).as_deref()
    }

    pub fn type_(&self, name: &TypeName) -> Option<&DynRlTypeDecl> {
        self.types.get(name).as_deref()
    }
}

impl TranspileOutHeader {
    pub fn finish(self, ctx: &mut ResolveCtx<'_>) -> TranspileOutput {
        TranspileOutput {
            exports: self.exports,
            source_code: self.rest.finish(ctx)
        }
    }
}