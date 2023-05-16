use once_cell::unsync::OnceCell;
use crate::analyses::bindings::{TypeIdent, ValueIdent};
use crate::analyses::types::{RlInferrableType, RlReturnType, RlType};
use crate::ast::ann::Ann;
use crate::ast::tree_sitter::TSNode;
use crate::{impl_has_ann_record_struct, impl_has_ann_wrapper_struct};
use crate::analyses::scopes::{ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::import_export::export::ImportPath;

/// Arbitrary expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueExpr<'tree>(TSNode<'tree>);

/// Formal parameter
#[derive(Debug)]
pub struct ValueParameter<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub is_this_param: bool,
    pub is_rest_param: bool,
    pub is_optional: bool,
    pub type_: Option<RlInferrableType<'tree>>,
    pub value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(ValueParameter);

/// "Parameter" bound in a `catch` clause
#[derive(Debug)]
pub struct CatchParameter<'tree> {
    pub name: AstValueIdent<'tree>,
}
impl_has_ann_wrapper_struct!(CatchParameter by name);

/// Return statement
#[derive(Debug, Clone, Copy)]
pub struct Return<'tree> {
    pub ann: Ann<'tree>,
    pub returned_value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(Return);

/// Throw statement
#[derive(Debug, Clone, Copy)]
pub struct Throw<'tree> {
    pub ann: Ann<'tree>,
    pub thrown_value: Option<ValueExpr<'tree>>,
}
impl_has_ann_record_struct!(Throw);

/// Value declaration
#[derive(Debug)]
pub struct ValueDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub type_: Option<RlType<'tree>>,
    pub value: Option<ValueExpr<'tree>>,
}

/// Function declaration
#[derive(Debug)]
pub struct FunctionDecl<'tree> {
    pub ann: Ann<'tree>,
    pub name: ValueIdent<'tree>,
    pub nominal_params: Vec<RlTypeParameter<'tree>>,
    pub formal_params: Vec<ValueParameter<'tree>>,
    pub return_type: Option<RlReturnType<'tree>>,
    fn_type: RlType<'tree>,
    custom_inferred_fn_type: OnceCell<RlType<'tree>>,
}

// TODO: Should we refactor the import specifier into RlImportedValueType?
//   Probably, but wait until we have a better idea of this design
/// Value import specifier (`bar as baz` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ValueImportSpecifier<'tree> {
    pub ann: Ann<'tree>,
    pub idx: ScopeValueImportIdx<'tree>,
    pub alias: ValueIdent<'tree>,
}

/// Type import specifier (`Bar as Baz` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct TypeImportSpecifier<'tree> {
    pub ann: Ann<'tree>,
    pub idx: ScopeTypeImportIdx<'tree>,
    pub alias: TypeIdent<'tree>,
}

/// Import path (`'foo'` in `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ImportPathAst<'tree> {
    pub ann: Ann<'tree>,
    pub module_path: ImportPath,
}

/// Import statement (the entire thing `import {bar as baz, ;Bar as Baz} from 'foo'`)
#[derive(Debug)]
pub struct ImportStatement<'tree> {
    pub ann: Ann<'tree>,
    pub path: ImportPathAst<'tree>,
    pub imported_values: Vec<ValueImportSpecifier<'tree>>,
    pub imported_types: Vec<TypeImportSpecifier<'tree>>,
    pub(crate) import_path_idx: usize
}

