use std::collections::HashMap;
use crate::analyses::types::TypeName;
use crate::misc::lazy::DynLazy;

pub struct Exports<'a> {
    value: HashMap<ValueName, DynLazy<'a, NominalTypeShape>>,
    nominal: HashMap<TypeName, DynLazy<'a, NominalTypeDeclShape>>
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