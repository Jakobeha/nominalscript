use std::collections::HashMap;
use std::sync::Arc;
use once_cell::sync::OnceCell;
use serde_json::Value;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::types::{FatType, FatTypeDecl, ReReturnType, ReturnType, ReType, ThinType, ThinTypeDecl, TypeParam};
use crate::ast::InProjectLoc;
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstType;
use crate::diagnostics::ProjectDiagnostics;
use crate::import_export::export::ModulePath;
use crate::import_export::import_ctx::ImportCtx;

/// Required or assigned type with information on what node it was determined from,
/// and whether it was explicitly provided and/or inferred. For diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedType<'tree> {
    pub type_: ReType,
    /// If the type is a literal, function, etc. and we determined from...
    /// TODO add more cases

    /// Last defined value before the use (the use is assigned the determined type, type_ is the
    /// value's type unless an explicit annotation is provided).
    pub defined_value: Option<TSNode<'tree>>,
    /// Explicit annotation for the expression, if provided (expression is assigned the explicit type)
    pub explicit_type: Option<TSNode<'tree>>,
}

/// Return type with information on where we determined it for diagnostics (see [DeterminedType]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedReturnType<'tree> {
    pub type_: ReReturnType,
    pub return_node: Option<TSNode<'tree>>,
    pub explicit_type: Option<TSNode<'tree>>,
}

impl<'tree> DeterminedType<'tree> {
    /// Type was determined from `inferred_from`.
    pub fn from_last_def(type_: ReType, inferred_from: TSNode<'tree>) -> Self {
        Self { type_, defined_value: Some(inferred_from), explicit_type: None }
    }

    /// Type
    pub fn explicit(annotation: &AstType<'tree>) -> Self {
        Self { type_: annotation.shape.clone(), defined_value: None, explicit_type: Some(annotation.node) }
    }
}