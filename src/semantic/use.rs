use crate::semantic::def::{TypeDef, ValueDef};

/// Value reference
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueUse<'tree> {
    /// Referenced declaration
    def: ValueDef<'tree>
}

/// Type reference
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeUse<'tree> {
    /// Referenced declaration
    def: TypeDef<'tree>
}