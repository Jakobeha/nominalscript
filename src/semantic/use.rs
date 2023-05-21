use crate::semantic::def::{TypeDef, ValueDef};

/// Value reference
#[repr(transparent)]
pub struct ValueUse<'tree> {
    /// Referenced declaration
    def: ValueDef<'tree>
}

/// Type reference
#[repr(transparent)]
pub struct TypeUse<'tree> {
    /// Referenced declaration
    def: TypeDef<'tree>
}