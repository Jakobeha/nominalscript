use crate::semantic::def::{TypeDef, ValueDef};

/// Value reference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ValueUse<'tree> {
    /// Referenced declaration
    def: ValueDef<'tree>
}

/// Type reference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct TypeUse<'tree> {
    /// Referenced declaration
    def: TypeDef<'tree>
}