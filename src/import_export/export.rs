use std::collections::HashMap;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{FatType, FatTypeDecl};
use crate::misc::lazy::RcLazy;

#[derive(Debug)]
pub struct Exports {
    value: HashMap<ValueName, RcLazy<FatType>>,
    nominal: HashMap<TypeName, RcLazy<FatTypeDecl>>
}

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
#[derive(Debug)]
pub struct TranspileOutHeader {
    exports: Exports,
    source_code: RcLazy<String>
}

#[derive(Debug)]
pub struct TranspileOutput {
    exports: Exports,
    source_code: String
}

impl Exports {
    pub fn new() -> Exports {
        Exports {
            value: HashMap::new(),
            nominal: HashMap::new()
        }
    }
}