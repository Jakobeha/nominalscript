use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomPinned;
use std::rc::Rc;

use indexmap::{Equivalent, IndexMap};
use once_cell::unsync::OnceCell;

use crate::{error, hint, issue};
use crate::analyses::bindings::{Locality, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::{ExprTypeMap, InactiveScopePtr, WeakScopePtr};
use crate::analyses::scopes::scope_imports::{ScopeImportAlias, ScopeImportIdx, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{DeterminedReturnType, FatType, FatTypeInherited, ResolveCache, ResolveCtx, ReturnType, RlReturnType, RlType, IdentType};
use crate::concrete::tree_sitter::TSNode;
use crate::concrete::typed_nodes::{AstCatchParameter, ImportPathAst, AstImportStatement, AstNode, AstParameter, AstReturn, AstThrow, AstTypeBinding, AstTypeIdent, AstTypeImportSpecifier, AstValueBinding, AstValueDecl, AstValueIdent, AstValueImportSpecifier, DynAstTypeBinding, DynAstValueBinding};
use crate::diagnostics::{FileLogger, TypeLogger};

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
#[derive(Debug)]
pub struct Scope<'tree> {
    pub node: TSNode<'tree>,
    did_set_params: bool,
    imported_script_paths: Vec<ImportPathAst<'tree>>,
    pub values: ValueScope<'tree>,
    pub types: TypeScope<'tree>,
    pub(crate) resolve_cache: ResolveCache,
    pub parent: WeakScopePtr<'tree>,
    pub(super) is_being_mutated: bool,
    _pinned: PhantomPinned
}