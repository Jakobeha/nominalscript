use once_cell::unsync::OnceCell;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::{impl_has_ann_record_struct, impl_has_name};
use crate::semantic::ann::Ann;
use crate::semantic::arena::{IdentityRef, impl_has_name};
use crate::semantic::expr::{Expr, Type};
use crate::semantic::name::{TypeIdent, ValueIdent};

/// Value declaration
pub type ValueDef<'tree> = IdentityRef<'tree, OwnedValueDef<'tree>>;
/// Owned [ValueDef]
#[derive(Debug)]
// #[derive(MultiPhase)] #[phase(SemanticPhase)]
pub struct OwnedValueDef<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Identifier
    pub ident: ValueIdent<'tree>,
    /// Type
    // #[phase(SemanticPhase::Expressions)]
    pub type_: OnceCell<Type<'tree>>,
    /// Initial value
    // #[phase(SemanticPhase::Expressions)]
    pub init: OnceCell<Option<Expr<'tree>>>
}

/// Type declaration
pub type TypeDef<'tree> = IdentityRef<'tree, OwnedTypeDef<'tree>>;
/// Owned [TypeDef]
#[derive(Debug)]
// #[derive(MultiPhase)] #[phase(SemanticPhase)]
pub struct OwnedTypeDef<'tree> {
    /// Source location
    pub ann: Ann<'tree>,
    /// Identifier
    pub ident: TypeIdent<'tree>,
    /// Type this is defined as
    // #[phase(SemanticPhase::Expressions)]
    pub value: OnceCell<Type<'tree>>
}

impl_has_name!(('tree) &'tree ValueName for OwnedValueDef<'tree>);
impl_has_name!(('tree) &'tree TypeName for OwnedTypeDef<'tree>);
impl_has_ann_record_struct!(OwnedValueDef);
impl_has_ann_record_struct!(OwnedTypeDef);