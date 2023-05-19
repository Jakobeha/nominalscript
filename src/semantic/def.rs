use crate::analyses::bindings::{TypeNameStr, ValueNameStr};
use crate::impl_has_name;

/// Value declaration
pub struct ValueDef<'tree> {
    /// Name
    name: &'tree ValueNameStr,
    /// Type
    type_: Type<'tree>,
    /// Initial value
    init: Option<Expr<'tree>>
}

/// Type declaration
pub struct TypeDef<'tree> {
    /// Name
    name: &'tree TypeNameStr,
    /// Type this is defined as
    value: Type<'tree>
}

impl_has_name!(('tree) &'tree ValueNameStr for ValueDef<'tree>);
impl_has_name!(('tree) &'tree TypeNameStr for TypeDef<'tree>);