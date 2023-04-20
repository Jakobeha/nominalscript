use std::collections::HashMap;
use std::path::Path;

use derive_more::{AsRef, Deref, DerefMut, Display, From, Into};
use self_cell::self_cell;

use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::scopes::{ModuleCtx, ScopeImportAlias};
use crate::analyses::types::{DynResolvedLazy, DynRlType, DynRlTypeDecl, ResolveCtx};
use crate::ast::tree_sitter::TSTree;
use crate::compile::finish_transpile;
use crate::import_export::ModulePath;
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
    path: ModulePath,
    pub exports: Exports,
    module_data: _Module
}

self_cell!(
    struct _Module {
        owner: TSTree,
        #[not_covariant]
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
#[derive(Debug, Display, Clone, PartialEq, Eq, Hash, From, Into, AsRef, Deref, DerefMut)]
pub struct ImportPath(String);

impl Module {
    pub fn new(path: ModulePath, ast: TSTree) -> Module {
        Module {
            path,
            exports: Exports::new(),
            module_data: _Module::new(ast, |tree| ModuleCtx::new(tree))
        }
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn ast(&self) -> &TSTree {
        self.module_data.borrow_owner()
    }

    pub fn with_ctx<'outer, R>(&'outer self, fun: impl for<'q> FnOnce(&'outer ModuleCtx<'q>) -> R) -> R {
        self.module_data.with_dependent(|_ast, ctx| fun(ctx))
    }

    pub fn with_module_data_mut<'outer, R>(&'outer mut self, fun: impl for<'q> FnOnce(&'outer ModulePath, &'outer mut Exports, &'q TSTree, &'outer mut ModuleCtx<'q>) -> R) -> R {
        self.module_data.with_dependent_mut(|ast, module_ctx| fun(&self.path, &mut self.exports, ast, module_ctx))
    }

    pub fn finish(mut self, ctx: &ProjectCtx<'_>) -> TranspiledModule {
        let source_code = self.module_data.with_dependent_mut(|ast, module_ctx| {
            finish_transpile(ast, module_ctx, &ResolveCtx::new(ctx, &self.path));
            ast.display_unmarked().to_string()
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

    pub fn add_value(&mut self, alias: ValueName, type_: Box<DynRlType>) {
        self.values.insert(alias, type_);
    }

    pub fn add_type(&mut self, alias: TypeName, decl: Box<DynRlTypeDecl>) {
        self.types.insert(alias, decl);
    }

    pub fn value_type(&self, name: &ValueName) -> Option<&DynRlType> {
        self.values.get(name).map(|x| x.as_ref())
    }

    pub fn type_decl(&self, name: &TypeName) -> Option<&DynRlTypeDecl> {
        self.types.get(name).map(|x| x.as_ref())
    }

    pub fn get<Alias: ScopeImportAlias>(&self, name: &Alias) -> Option<&DynResolvedLazy<Alias::Fat>> {
        name._index_into_exports(self)
    }
}

impl AsRef<Path> for ImportPath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}