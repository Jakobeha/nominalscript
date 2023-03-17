use std::fmt::{Debug, Display};
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::scopes::Scope;
use crate::analyses::types::{DynResolvedLazy, FatType, FatTypeDecl};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstImportPath;
use crate::import_export::export::Exports;

/// Index in a scope to an imported value or type identifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeImportIdx<Alias> {
    /// Index to the [AstImportPath]
    pub import_path_idx: usize,
    /// Imported name
    pub imported_name: Alias
}

/// Index in a scope to an imported value identifier
pub type ScopeValueImportIdx = ScopeImportIdx<ValueName>;
/// Index in a scope to an imported type identifier
pub type ScopeTypeImportIdx = ScopeImportIdx<TypeName>;

/// Name which is used to import data into the scope
pub trait ScopeImportAlias: Display {
    type Fat: Debug + Clone + Default + Eq;

    /// [Scope::import]
    fn _index_into_scope<'a, 'tree>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a AstImportPath<'tree>, TSNode<'tree>) where Self: Sized;
    /// [Exports::get]
    fn _index_into_exports<'a>(&self, exports: &'a Exports) -> Option<&'a DynResolvedLazy<Self::Fat>>;
    /// User-facing label
    fn capital_name_str() -> &'static str;
}

impl ScopeImportAlias for ValueName {
    type Fat = FatType;

    fn _index_into_scope<'a, 'tree>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a AstImportPath<'tree>, TSNode<'tree>) {
        let (import_path, import_ast) = scope.value_import(this);
        (import_path, import_ast.node())
    }

    fn _index_into_exports<'a>(&self, exports: &'a Exports) -> Option<&'a DynResolvedLazy<Self::Fat>> {
        exports.value_type(self)
    }

    fn capital_name_str() -> &'static str {
        "Value"
    }
}

impl ScopeImportAlias for TypeName {
    type Fat = FatTypeDecl;

    fn _index_into_scope<'a, 'tree>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a AstImportPath<'tree>, TSNode<'tree>) {
        let (import_path, import_ast) = scope.type_import(this);
        (import_path, import_ast.node())
    }

    fn _index_into_exports<'a>(&self, exports: &'a Exports) -> Option<&'a DynResolvedLazy<Self::Fat>> {
        exports.type_decl(self)
    }

    fn capital_name_str() -> &'static str {
        "Type"
    }
}
