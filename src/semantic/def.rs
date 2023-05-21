use crate::analyses::bindings::{TypeNameStr, ValueNameStr};
use crate::{impl_has_ann_record_struct, impl_has_name};
use crate::misc::arena::IdentityRef;
use crate::semantic::ann::Ann;
use crate::semantic::expr::{Expr, Type};

/// Value declaration
pub type ValueDef<'tree> = IdentityRef<'tree, OwnedValueDef<'tree>>;
/// Owned [ValueDef]
#[derive(Debug)]
pub struct OwnedValueDef<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
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
    /// Source location
    pub ann: Ann<'tree>,
    /// Name
    pub name: &'tree TypeNameStr,
    /// Type this is defined as
    pub value: Type<'tree>
}

impl_has_name!(('tree) &'tree ValueNameStr for OwnedValueDef<'tree>);
impl_has_name!(('tree) &'tree TypeNameStr for OwnedTypeDef<'tree>);
impl_has_ann_record_struct!(OwnedValueDef);
impl_has_ann_record_struct!(OwnedTypeDef);