use crate::analyses::types::{RlReturnType, RlType};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstType;
use crate::diagnostics::FileLogger;

/// Required or assigned type with information on what node it was determined from,
/// and whether it was explicitly provided and/or inferred. For diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedType<'tree> {
    pub type_: RlType,
    /// Last defined value before the use (the use is assigned the determined type, type_ is the
    /// value's type unless an explicit annotation is provided).
    pub defined_value: Option<TSNode<'tree>>,
    /// Explicit annotation for the expression, if provided (expression is assigned the explicit type)
    pub explicit_type: Option<TSNode<'tree>>,
}

/// Return type with information on where we determined it for diagnostics (see [DeterminedType]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeterminedReturnType<'tree> {
    pub type_: RlReturnType,
    pub return_node: Option<TSNode<'tree>>,
    pub explicit_type: Option<TSNode<'tree>>,
}

impl<'tree> DeterminedType<'tree> {
    /// Type was inferred from the most recent initializer or assignment (def) before the location
    /// where its type is checked (use).
    ///
    /// Currently the "most recent initializer or assignment" is simply the initializer,
    /// and types don't get inferred from assignments. We don't support flow typing and don't even
    /// do control-flow analysis
    pub fn from_last_def(type_: RlType, inferred_from: TSNode<'tree>) -> Self {
        Self { type_, defined_value: Some(inferred_from), explicit_type: None }
    }

    /// Type was explicitly provided via type annotation
    pub fn explicit(annotation: &AstType<'tree>) -> Self {
        Self { type_: annotation.shape.clone(), defined_value: None, explicit_type: Some(annotation.node) }
    }

    /// Checks that `assigned` is a subtype of `required`. If not, emits an error at `loc_node`
    /// (the location where we want a type of `required` e.g. function argument)
    pub(crate) fn check_subtype(assigned: Option<&DeterminedType>, required: Option<&DeterminedType>, loc_node: &TSNode, e: &mut FileLogger<'_>) {
        assigned; required; loc_node; e; todo!()
    }

    /// Checks that `assigned` and `required` are not disjoint, i.e. `assigned` is a subtype *or*
    /// supertype of `required`.
    ///
    /// This is important for wrap expressions, which enable up-casting: if the wrapped type is a
    /// supertype of the wrapper type, a runtime guard is generated to check each individual value's
    /// type and ensure it is an instance. However, if the wrapped type and wrapper type are disjoint,
    /// a wrapped value can never be an instance, so the guard will always fail, and it makes sense
    /// to notify the user of this at compile-time.
    ///
    /// `loc_node` is where we emit the error, e.g. the location of the wrap expression.
    pub(crate) fn check_not_disjoint(assigned: Option<&DeterminedType>, required: Option<&DeterminedType>, loc_node: &TSNode, e: &mut FileLogger<'_>) {
        assigned; required; loc_node; e; todo!()
    }
}