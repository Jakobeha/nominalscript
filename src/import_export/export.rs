use std::collections::HashMap;
use std::sync::Arc;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::types::{DynResolvedLazy, DynRlType, DynRlTypeDecl, FatType, FatTypeDecl, ResolveCtx, ResolvedLazyTrait, RlImportedTypeDecl, RlImportedValueType, RlType, RlTypeDecl};

use derive_more::{From, Into, AsRef, Deref, DerefMut};
use self_cell::self_cell;
use crate::analyses::scopes::{ModuleCtx, ScopeImportAlias};
use crate::ast::tree_sitter::TSTree;
use crate::compile::{finish_transpile, Module};
use crate::ProjectCtx;

#[derive(Debug)]
pub struct TranspiledModule {
    pub exports: Exports,
    pub source_code: String
}

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
#[derive(Debug)]
pub struct Module {
    pub exports: Exports,
    module_data: _Module
}

self_cell!(
    struct _Module {
        owner: TSTree,
        #[covariant]
        dependent: ModuleCtx,
    }

    impl {Debug}
);

#[derive(Debug, Default)]
pub struct Exports {
    values: HashMap<ValueName, Box<DynRlType>>,
    types: HashMap<TypeName, Box<DynRlTypeDecl>>
}

/// Path to import a module in an import statement, distinguished from [PathBuf] which is the
/// resolved path
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into, AsRef, Deref, DerefMut)]
pub struct ImportPath(String);

impl Module {
    pub fn new(ast: TSTree) -> Module {
        Module {
            exports: Exports::new(),
            module_data: _Module::new(ast, ModuleCtx::new)
        }
    }

    pub fn ast(&self) -> &TSTree {
        self.module_data.borrow_owner()
    }

    pub fn ctx(&self) -> &ModuleCtx<'_> {
        self.module_data.borrow_dependent()
    }

    pub fn with_module_data_mut<'outer, R>(&mut self, fun: impl for<'q> FnOnce(&'outer mut Exports, &'q TSTree, &'outer mut ModuleCtx<'q>) -> R) -> R {
        self.module_data.with_dependent_mut(|ast, module_ctx| fun(&mut self.exports, ast, module_ctx))
    }

    pub fn finish(mut self, ctx: &mut ProjectCtx<'_>) -> TranspiledModule {
        let source_code = self.module_data.with_dependent_mut(|ast, module_ctx| {
            finish_transpile(ast, module_ctx, ctx);
            ast.print()
        });
        TranspiledModule {
            exports: self.exports,
            source_code
        }
    }
}

impl Exports {
    pub fn new() -> Exports {
        Exports {
            values: HashMap::new(),
            types: HashMap::new()
        }
    }

    pub fn add_value(&mut self, alias: ValueName, type_: impl ResolvedLazyTrait<FatType>) {
        self.values.insert(alias, Box::new(type_));
    }

    pub fn add_type(&mut self, alias: TypeName, decl: impl ResolvedLazyTrait<FatTypeDecl>) {
        self.types.insert(alias, Box::new(decl));
    }

    pub fn value_type(&self, name: &ValueName) -> Option<&DynRlType> {
        self.values.get(name).as_deref()
    }

    pub fn type_decl(&self, name: &TypeName) -> Option<&DynRlTypeDecl> {
        self.types.get(name).as_deref()
    }

    pub fn get<Alias: ScopeImportAlias>(&self, name: &Alias) -> Option<&DynResolvedLazy<Alias::Fat>> {
        name._index_into_exports(self)
    }
}
