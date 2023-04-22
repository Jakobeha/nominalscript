use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;
use indexmap::Equivalent;
use crate::analyses::bindings::{HoistedValueBinding, Locality, TypeBinding, TypeName, ValueBinding, ValueName};
use crate::analyses::scopes::ExprTypeMap;
use crate::analyses::types::{DeterminedType, DynRlType, DynRlTypeDecl, FatType, FatTypeDecl, ResolveCtx, RlType, RlTypeDecl};
use crate::import_export::import_resolver::ImportResolver;
use crate::Project;

fn get_global_bindings() -> (HashMap<ValueName, GlobalValueBinding>, HashMap<TypeName, GlobalTypeBinding>) {
    let module_root_path = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/", file!())).parent().unwrap();
    let global_project = Project::new(
        ImportResolver {
            resolve_node_modules: false,
            absolute_import_source_paths: Default::default(),
            absolute_import_base_path: module_root_path.to_path_buf(),
            module_root_path: module_root_path.to_path_buf(),
        },
        false
    );
    let global_script_path = module_root_path.join("global_bindings.ns");
    let global_module = &global_project.begin_transpile_file(&global_script_path).expect("global bindings script should be valid");
    let global_exports = &global_module.exports;
    let ctx = ResolveCtx::new(global_project.ctx(), global_module.path());
    let global_value_types = global_exports.iter_value_types().map(|(name, type_)| {
        let binding = GlobalValueBinding {
            name: name.clone(),
            type_: type_.normalize_clone(&ctx),
        };
        (name.clone(), binding)
    }).collect::<HashMap<_, _>>();
    let global_type_decls = global_exports.iter_type_decls().map(|(name, decl)| {
        let binding = GlobalTypeBinding {
            name: name.clone(),
            decl: decl.normalize_clone(&ctx),
        };
        (name.clone(), binding)
    }).collect::<HashMap<_, _>>();
    (global_value_types, global_type_decls)
}

thread_local! {
    static GLOBAL_BINDINGS: &'static (HashMap<ValueName, GlobalValueBinding>, HashMap<TypeName, GlobalTypeBinding>) = Box::leak(Box::new(get_global_bindings()));
}

/// [ValueBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
#[derive(Debug, Clone)]
pub struct GlobalValueBinding {
    pub name: ValueName,
    pub type_: RlType,
}

/// [TypeBinding] which is implicitly available to the file (available and not imported).
///
/// There are 3 kinds of bindings: local, imported, and global.
#[derive(Debug, Clone)]
pub struct GlobalTypeBinding {
    pub name: TypeName,
    pub decl: RlTypeDecl,
}

impl GlobalValueBinding {
    pub fn new(name: ValueName, type_: FatType) -> Self {
        Self {
            name,
            type_: RlType::resolved(type_),
        }
    }

    pub fn has<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(name: &N) -> bool where ValueName: Borrow<N> {
        GLOBAL_BINDINGS.with(|(b, _)| b.contains_key(name))
    }

    pub fn get<N: Equivalent<ValueName> + Eq + Hash + ?Sized>(name: &N) -> Option<&'static GlobalValueBinding> where ValueName: Borrow<N> {
        GLOBAL_BINDINGS.with(|(b, _)| b.get(name))
    }
}

impl<'tree> ValueBinding<'tree> for GlobalValueBinding {
    fn name(&self) -> &ValueName {
        &self.name
    }

    fn value_type(&self) -> &DynRlType {
        &self.type_
    }

    fn locality(&self) -> Locality {
        Locality::Global
    }

    fn infer_type_det(&self, _typed_exprs: Option<&ExprTypeMap<'tree>>, _ctx: &ResolveCtx<'_>) -> DeterminedType<'tree> {
        DeterminedType::intrinsic( self.type_.clone())
    }
}

impl PartialEq<GlobalValueBinding> for GlobalValueBinding {
    fn eq(&self, other: &GlobalValueBinding) -> bool {
        // Name is the only thing that matters:
        // if 2 different global bindings had the same name, how would we resolve them?
        self.name == other.name
    }
}

impl Eq for GlobalValueBinding {}

impl Hash for GlobalValueBinding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl GlobalTypeBinding {
    pub fn new(name: TypeName, decl: FatTypeDecl) -> Self {
        Self {
            name,
            decl: RlTypeDecl::resolved(decl),
        }
    }

    pub fn has<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(name: &N) -> bool where TypeName: Borrow<N> {
        GLOBAL_BINDINGS.with(|(_, b)| b.contains_key(name))
    }

    pub fn get<N: Equivalent<TypeName> + Eq + Hash + ?Sized>(name: &N) -> Option<&'static GlobalTypeBinding> where TypeName: Borrow<N> {
        GLOBAL_BINDINGS.with(|(_, b)| b.get(name))
    }

    /// The declared type as a determined type
    pub fn type_det<'tree>(&self) -> DeterminedType<'tree> {
        DeterminedType::intrinsic( self.decl.clone().into_type())
    }
}

impl TypeBinding for GlobalTypeBinding {
    fn name(&self) -> &TypeName {
        &self.name
    }

    fn type_decl(&self) -> &DynRlTypeDecl {
        &self.decl
    }

    fn locality(&self) -> Locality {
        Locality::Global
    }
}

impl PartialEq<GlobalTypeBinding> for GlobalTypeBinding {
    fn eq(&self, other: &GlobalTypeBinding) -> bool {
        // Name is the only thing that matters:
        // if 2 different global bindings had the same name, how would we resolve them?
        self.name == other.name
    }
}

impl Eq for GlobalTypeBinding {}

impl Hash for GlobalTypeBinding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<'tree> HoistedValueBinding<'tree> for GlobalValueBinding {}
