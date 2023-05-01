use std::fmt::{Debug, Display};
use crate::analyses::bindings::{TypeIdent, TypeName, ValueIdent, ValueName};
use crate::analyses::scopes::Scope;
use crate::analyses::types::{DynResolvedLazy, FatTrait, FatType, FatTypeDecl};
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::ImportPathAst;
use crate::impl_has_eqv_ident_struct;
use crate::import_export::export::Exports;

/// Index in a scope to an imported value or type identifier
#[derive(Debug, Clone)]
pub struct ScopeImportIdx<Alias> {
    /// Index to the [ImportPathAst]
    pub import_path_idx: usize,
    /// Imported name
    pub imported_name: Alias
}
impl_has_eqv_ident_struct!(ScopeImportIdx<Alias> { import_path_idx ; ; imported_name });

/// Index in a scope to an imported value identifier
pub type ScopeValueImportIdx<'tree> = ScopeImportIdx<ValueIdent<'tree>>;
/// Index in a scope to an imported type identifier
pub type ScopeTypeImportIdx<'tree> = ScopeImportIdx<TypeIdent<'tree>>;

/// Name which is used to import data into the scope
pub trait ScopeImportAlias<'tree>: Display {
    type Fat: FatTrait + Default;

    /// [Scope::import]
    fn _index_into_scope<'a>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a ImportPathAst<'tree>, TSNode<'tree>) where Self: Sized;
    /// [Exports::get]
    fn _index_into_exports<'a>(&self, exports: &'a Exports) -> Option<&'a DynResolvedLazy<Self::Fat>>;
    /// User-facing label
    fn capital_name_str() -> &'static str;
}

impl<'tree> ScopeImportAlias<'tree> for ValueIdent<'tree> {
    type Fat = FatType<'tree>;

    fn _index_into_scope<'a, 'tree>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a ImportPathAst<'tree>, TSNode<'tree>) {
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

impl<'tree> ScopeImportAlias<'tree> for TypeIdent<'tree> {
    type Fat = FatTypeDecl<'tree>;

    fn _index_into_scope<'a, 'tree>(this: &ScopeImportIdx<Self>, scope: &'a Scope<'tree>) -> (&'a ImportPathAst<'tree>, TSNode<'tree>) {
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
