use std::collections::HashMap;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{FatType, FatTypeDecl};
use crate::misc::lazy::DynLazy;

pub struct Exports<'a> {
    value: HashMap<ValueName, DynLazy<'a, FatType>>,
    nominal: HashMap<TypeName, DynLazy<'a, FatTypeDecl>>
}

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
pub struct TranspileOutHeader<'a> {
    exports: Exports<'a>,
    source_code: DynLazy<'a, String>
}

pub struct TranspileOutput {
    exports: Exports<'static>,
    source_code: String
}

impl<'tree> Exports<'tree> {
    pub fn new() -> Exports<'tree> {
        Exports {
            value: HashMap::new(),
            nominal: HashMap::new()
        }
    }
}