use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::Hash;

use derive_more::Display;

use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{DynRlType, DynRlTypeDecl};
use crate::ast::tree_sitter::TSNode;

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Locality {
    /// Binding is one of the builtin, implicitly-imported globals
    #[display(fmt = "global")]
    Global,
    /// Binding is explicitly imported
    #[display(fmt = "imported")]
    Imported,
    /// Binding is local to the file/syntax-tree
    #[display(fmt = "local")]
    Local
}

/// Declares a value identifier which can be referenced:
/// imports, declarations, parameters, predefined globals, etc.
pub trait ValueBinding<'tree>: Debug {
    /// Binding name
    fn name(&self) -> &ValueName;
    /// Where the binding was declared, if it was declared in a syntax-tree
    fn node(&self) -> Option<&TSNode<'tree>>;
    /// Semantic type of the value
    fn value_type(&self) -> &DynRlType<'tree>;
    /// Binding's locality
    fn locality(&self) -> Locality;
}
/// See [ValueBinding]
pub type DynValueBinding<'tree> = dyn ValueBinding<'tree> + 'tree;

/// Declares a type identifier which can be referenced:
/// imported types, type declarations, type parameters, predefined globals, etc.
pub trait TypeBinding<'tree>: Debug {
    /// Binding name
    fn name(&self) -> &TypeName;
    /// Where the binding was declared, if it was declared in a syntax-tree
    fn node(&self) -> Option<&TSNode<'tree>>;
    /// Semantic type declaration (semantic version of this).
    ///
    /// If local, the name and node will be the same.
    /// But if imported (or if we ever add type aliases), the name and node will be of the alias.
    fn type_decl(&self) -> &DynRlTypeDecl<'tree>;
    /// Binding's locality
    fn locality(&self) -> Locality;
}
/// See [TypeBinding]
pub type DynTypeBinding<'tree> = dyn TypeBinding<'tree> + 'tree;