use once_cell::unsync::OnceCell;
use crate::impl_has_name;

use crate::semantic::expr::{Expr, Type};
use crate::semantic::name::{TypeIdent, ValueIdent, ValueName, TypeName};
use crate::semantic::storage::Id;

/// Value declaration
pub type ValueDef<'tree> = Id<'tree, ValueDefData<'tree>>;
/// [ValueDef] data
#[derive(Debug)]
pub struct ValueDefData<'tree> {
    /// Identifier
    pub ident: ValueIdent<'tree>,
    /// Type
    pub type_: OnceCell<Type<'tree>>,
    /// Initial value
    pub init: OnceCell<Option<Expr<'tree>>>
}

/// Type declaration
pub type TypeDef<'tree> = Id<'tree, TypeDefData<'tree>>;
/// [TypeDef] data
#[derive(Debug)]
pub struct TypeDefData<'tree> {
    /// Identifier
    pub ident: TypeIdent<'tree>,
    /// Type this is defined as
    pub value: OnceCell<Type<'tree>>
}

impl_has_name!(<'tree> ValueName for ValueDefData<'tree>);
impl_has_name!(<'tree> TypeName for TypeDefData<'tree>);