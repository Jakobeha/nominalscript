use crate::analyses::types::{Nullability, TypescriptType};
use crate::semantic::r#use::{TypeUse, ValueUse};
use crate::syntax::nodes;

/// Value expression = either a value (identifier, builtin or structure) or operation which reduces to a value
pub enum Expr<'tree> {
    Identifier { r#use: &'tree ValueUse<'tree> },
    /// Misc operation, we only care about the sub-expressions
    Misc { children: &'tree Expr<'tree> }
}

/// Type = identifier with optional generics, builtin or structural type. A normalized (non-reducible) type expression, but currently all type expressions are normal.
pub struct Type<'tree> {
    nullability: Nullability,
    identifiers: Vec<TypeIdentifier<'tree>>,
    structure: Option<TypeStructure<'tree>>,
    guards: Vec<nodes::NominalTypeGuard<'tree>>,
    typescript_types: Vec<nodes::TypeAnnotation<'tree>>,
}

pub struct TypeIdentifier<'tree> {
    /// Type referenced by the identifier
    r#use: &'tree TypeUse<'tree>,
    /// Type arguments
    arguments: Vec<&'tree Type<'tree>>
}