use crate::analyses::bindings::{TypeNameStr, ValueNameStr};
use crate::impl_has_name;
use crate::misc::arena::IdentityRef;
use crate::semantic::expr::{Expr, Type};

/// Value declaration
pub type ValueDef<'tree> = IdentityRef<'tree, OwnedValueDef<'tree>>;
/// Owned [ValueDef]
#[derive(Debug)]
pub struct OwnedValueDef<'tree> {
    /// Name
    pub name: &'tree ValueNameStr,
    /// Type
    pub type_: Type<'tree>,
    /// Initial value
    pub init: Option<Expr<'tree>>
}

/// Type declaration
pub type TypeDef<'tree> = IdentityRef<'tree, OwnedTypeDef<'tree>>;
/// Owned [TypeDef]
#[derive(Debug)]
pub struct OwnedTypeDef<'tree> {
    /// Name
    pub name: &'tree TypeNameStr,
    /// Type this is defined as
    pub value: Type<'tree>
}

impl_has_name!(('tree) &'tree ValueNameStr for OwnedValueDef<'tree>);
impl_has_name!(('tree) &'tree TypeNameStr for OwnedTypeDef<'tree>);