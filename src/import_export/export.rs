use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;

use derive_more::{AsRef, Deref, DerefMut, Display, Error, From, Into};
use indexmap::Equivalent;
use self_cell::self_cell;

use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::scopes::{ModuleCtx, ScopeImportAlias};
use crate::analyses::types::{DynResolvedLazy, DynRlType, DynRlTypeDecl, ResolveCtx};
use crate::concrete::tree_sitter::TSTree;
use crate::compile::finish_transpile;
use crate::import_export::ModulePath;
use crate::ProjectCtx;

/// Lazy transpile output which lets us access header information without transpiling the rest,
/// which is not only more efficient but solves import cycles
#[derive(Debug)]
pub struct Module {
    path: ModulePath,
    pub exports: Exports,
    module_data: RefCell<_Module>,
    did_transpile: Cell<bool>
}

self_cell!(
    struct _Module {
        owner: TSTree,
        #[not_covariant]
        dependent: ModuleCtx,
    }

    impl {Debug}
);

/// Already transpiled (if you want to you must manually remember the output associated with the path)
#[derive(Debug, Clone, Display, Error)]
#[display(fmt = "already transpiled")]
pub struct AlreadyTranspiled;

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
            module_data: RefCell::new(_Module::new(ast, |tree| ModuleCtx::new(tree))),
            did_transpile: Cell::new(false)
        }
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn with_module_data_mut<'outer, R>(&'outer mut self, fun: impl for<'q> FnOnce(&'outer ModulePath, &'outer mut Exports, &'q TSTree, &'outer mut ModuleCtx<'q>) -> R) -> R {
        self.module_data.get_mut().with_dependent_mut(|ast, module_ctx| fun(&self.path, &mut self.exports, ast, module_ctx))
    }

    /// Finishes transpiling. If called before, returns [AlreadyTranspiled]
    pub fn finish(&self, ctx: ProjectCtx<'_>) -> Result<String, AlreadyTranspiled> {
        if self.did_transpile.replace(true) {
            return Err(AlreadyTranspiled);
        }
        Ok(self.module_data.borrow_mut().with_dependent_mut(|ast, module_ctx| {
            finish_transpile(ast, module_ctx, &ResolveCtx::new(ctx, &self.path));
            ast.display_unmarked().to_string()
        }))
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

    pub fn value_type<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynRlType> where ValueName: Borrow<N> {
        self.values.get(name).map(|x| x.as_ref())
    }

    pub fn type_decl<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(&self, name: &N) -> Option<&DynRlTypeDecl> where TypeName: Borrow<N> {
        self.types.get(name).map(|x| x.as_ref())
    }

    pub fn get<Alias: ScopeImportAlias>(&self, name: &Alias) -> Option<&DynResolvedLazy<Alias::Fat>> {
        name._index_into_exports(self)
    }

    pub fn iter_value_types(&self) -> impl Iterator<Item = (&ValueName, &DynRlType)> {
        self.values.iter().map(|(k, v)| (k, v.as_ref()))
    }

    pub fn iter_type_decls(&self) -> impl Iterator<Item = (&TypeName, &DynRlTypeDecl)> {
        self.types.iter().map(|(k, v)| (k, v.as_ref()))
    }
}

impl AsRef<Path> for ImportPath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}