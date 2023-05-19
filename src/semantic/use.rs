use crate::semantic::def::{TypeDef, ValueDef};

/// Value reference
pub struct ValueUse<'tree> {
    /// Referenced declaration
    decl: &'tree ValueDef<'tree>
}

/// Type reference
pub struct TypeUse<'tree> {
    /// Referenced declaration
    decl: &'tree TypeDef<'tree>
}