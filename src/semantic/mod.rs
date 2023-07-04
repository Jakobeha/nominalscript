pub use package::*;

/// Annotations = source (syntax) info for semantic nodes
pub mod ann;
/// Scope, toplevel scope, etc.
pub mod scope;
/// Value and type declarations
pub mod def;
/// Value and type references
pub mod r#use;
/// Value and type expressions
pub mod expr;
mod package;
/// Custom arena types
// pub mod arena;
/// Binding identifiers
pub mod name;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SemanticPhase {
    Definitions,
    Expressions,
    TypeChecking,
    Removal,
}

/* trait _SemanticPhase {}
/// The semantic generation pass is divided into multiple phases. In later phases we add more
/// information to the semantic nodes
pub trait SemanticPhase: _SemanticPhase {
    /// `Some` if at or beyond the [Expressions] phase
    type Expressions<T>;
    fn expressions_as_option<T>(expressions: &Self::Expressions<T>) -> Option<&T>;
    fn expressions_as_option_mut<T>(expressions: &mut Self::Expressions<T>) -> Option<&mut T>;
}

/// Find definitions
pub enum Definitions {}
/// Find uses, types, and expressions
pub enum Expressions {}

impl _SemanticPhase for Definitions {}
impl SemanticPhase for Definitions {
    type Expressions<T> = ();
    #[inline] fn expressions_as_option<T>((): &Self::Expressions<T>) -> Option<&T> { None }
    #[inline] fn expressions_as_option_mut<T>((): &mut Self::Expressions<T>) -> Option<&mut T> { None }
}

impl _SemanticPhase for Expressions {}
impl SemanticPhase for Expressions {
    type Expressions<T> = T;
    #[inline] fn expressions_as_option<T>(expressions: &Self::Expressions<T>) -> Option<&T> { Some(expressions) }
    #[inline] fn expressions_as_option_mut<T>(expressions: &mut Self::Expressions<T>) -> Option<&mut T> { Some(expressions) }
} */