use std::cell::{Cell, RefCell};
use once_cell::unsync::OnceCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use elsa::{FrozenMap, FrozenVec};
use crate::analyses::bindings::{TypeName, ValueName};
use crate::analyses::scopes::{ExprTypeMap, ScopeChain};
use crate::analyses::types::{FatType, FatTypeDecl, Nullability, OptionalType, ReturnType, ThinType, ThinTypeDecl, TypeIdent, TypeParam, TypeStructure};
use crate::ast::InProjectLoc;
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::{FileDiagnostics, ProjectDiagnostics};
use crate::import_export::export::ModuleId;
use crate::import_export::import_ctx::{FileImportCtx, ProjectImportCtx};
use crate::misc::{impl_by_map, OnceCellExt};

/// `ResolvedLazy` = lazy value which is "resolved" according to how NominalScript does unordered
/// cyclic resolution.
///
/// Datastructure for types and values which may depend on each other, have arbitrary order, be in
/// separate (lazily-loaded) modules, and contain cycles (which must be detected and avoided).
/// This datastructure consists of a `thin` (pre-resolved) and `fat` (post-resolved) version.
///
/// Consider the following:
///
/// ```ns
/// import { ;C } from 'external';
///
/// const a1; A = A;[{ field; 1, another; 2 }]
/// const a2; A = A;[{ field; 3, another; 4 }]
///
/// type; A <; B[]
///
/// type; B <; { field; A, another; C }
/// ```
///
/// `a1` and `a2` reference `A` before it is defined, and `A` references `B` before it is defined.
/// But `B` also references `A`, so the expanded definition of `A` contains itself. Furthermore,
/// `B` references `C` which is imported from `external`, and `external` may import `A` and `B` and
/// even define `C` so that it also references them! Assuming `C` is defined and valid, all of this
/// is valid code, and we must be able to resolve `A`, `B`, and `C` and check subtype relations.
///
/// This is where [ResolvedLazy] comes in. The compiler encodes `A`, `B`, and all other potentially
/// forward/circular data in [ResolvedLazy], which is essentially a fancy `Lazy` value. The data
/// starts out with only a "thin" definition, which is computed solely from what is parsed i.e.
/// local. Then, when information about its inner structure is needed (e.g. to confirm that
/// `[{ field; 1, another; 2 }]` can be wrapped by `A` in type-checking), the "fat" definition is
/// computed or *forced*, by running a static function on the thin definition and a **resolution
/// context**.
///
/// Importantly, the resolution context keeps track of the "thin" versions of outer data which is
/// currently being forced while forcing the inner data. So when forcing a value or type would
/// ordinarily cause the value or type to be forced again (leading to an infinite loop or deadlock
/// or some other issue), instead the forcing computation does something else. Like, for types:
///
/// - If a nested definition (like the above), simply leaves the type unexpanded.
///   The subtype checker provides a context of outer types, so when it sees the same subtype/unify
///   relation but with missing (recursive) information it will explicitly yield "is subtype".
/// - If a type is its own supertype, logs a diagnostic error because this is illegal
/// - (Other cases which may not be documented, the point is it does not try to force again)
///
/// The resolution context also contains diagnostic and import resolution capabilities, so that
/// errors can be logged and imported data can be looked up and resolved. And it contains a cache,
/// so that after a thin data is forced, it does not need to be forced again.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ResolvedLazy<Thin, Fat> {
    /// Module-id of the file that the value is declared in if it's not local
    imported_origin: Option<ModuleId>,
    /// Thin version of the resolved value
    pub thin: Thin,
    /// Fat version of the resolved value
    fat: OnceCell<Fat>,
    // compute: fn(&Thin, &mut ResolveCtx<'_>) -> Fat
}

/// Where a value or type was created, and whether it was explicitly provided or inferred
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Creation {
    /// `loc` points to a syntax node of the provided type
    ExplicitlyProvided { loc: InProjectLoc },
    /// Type was inferred from the expression at `loc`.
    Inferred { loc: InProjectLoc },
}

#[derive(Debug)]
/// Context containing imports, diagnostics, and cached resolved values.
/// This context is for a file - however, it has access to other file data and will create
/// [ResolveCtx] for other files to resolve imports.
///
/// We the [FileDiagnostics] and [FileResolvedCache] to prevent spurious lookups,
/// but also [ProjectDiagnostics] and [ProjectResolvedCache] because we need to.
/// [FileImportCtx] has a reference to [ProjectImportCtx] so that one is covered.
pub struct ResolveCtx<'a> {
    imports: FileImportCtx<'a>,
    diagnostics: &'a FileDiagnostics,
    project_diagnostics: &'a mut ProjectDiagnostics,
    resolved: &'a FileResolveCache,
    /// Resolved data
    project_resolved: &'a mut ProjectResolveCache,
}

#[derive(Debug)]
pub(crate) struct ProjectResolveCache {
    module_ids: FrozenVec<PathBuf>,
    by_file: FrozenMap<PathBuf, Box<FileResolveCache>>,
}

#[derive(Debug)]
/// Cache of resolved data *and* data currently being resolved, to avoid infinite recursion
pub(crate) struct FileResolveCache {
    module_id: ModuleId,
    types_in_progress: RefCell<HashSet<ThinType>>,
    types: FrozenMap<ThinType, Box<FatType>>,
}

/// Resolved value depends on itself. Not necessarily an error, but must be handled explicitly.
#[derive(Debug, Clone, Copy)]
struct RecursiveResolution;

/// Trait implemented by [ResolvedLazy] which lets you use `DynResolvedLazy` values with arbitrary thin versions.
pub trait ResolvedLazyTrait<Fat> {
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> &Fat;
}

/// Resolved lazy value where you don't know/care about the thin version (dynamically-sized)
pub type DynResolvedLazy<Fat> = dyn ResolvedLazyTrait<Fat>;

/// A resolved type. See [ResolvedLazy]
pub type RlType = ResolvedLazy<ThinType, FatType>;
/// A resolved type parameter. "fat" value is `TypeParam::into_decl` because that's all we care
/// about. See [ResolvedLazy]
pub type RlTypeParam = ResolvedLazy<TypeParam<ThinType>, FatTypeDecl>;
/// A resolved return type. See [ResolvedLazy]
pub type RlReturnType = ResolvedLazy<ReturnType<ThinType>, ReturnType<FatType>>;
/// A resolved type declaration. See [ResolvedLazy]
pub type RlTypeDecl = ResolvedLazy<ThinTypeDecl, FatTypeDecl>;
/// A resolved type from an imported value. See [ResolvedLazy]
pub type RlImportedValueType = RlType;
/// A resolved type declaration from an imported type. See [ResolvedLazy]
pub type RlImportedTypeDecl = RlTypeDecl;

/// A fat (with resolved context) version of data which can be stripped and converted back into the
/// thin (without resolved context) version.
pub trait IntoThin<Thin> {
    /// Strips excess data and converts back into the thin version.
    fn into_thin(self) -> Thin;

    /// Strips excess data and converts back into the thin version, cloning anything necessary.
    ///
    /// This defaults to cloning and calling `into_thin` but you may be able to implement without
    /// cloning the excess data.
    fn ref_into_thin(&self) -> Thin {
        self.clone().into_thin()
    }
}

/// A thin (without resolved context) version of data which can be resolved into the fat version
/// with [ResolveCtx].
pub trait ResolveInto<Fat> {
    /// Computes the fat version from the thin version and resolution context. Note that if the
    /// thin version was already computed before, it is cached and `resolve` is not skipped.
    /// Therefore `resolve` should have no side-effects except for ones that would be redundant
    /// (e.g. logging diagnostics and resolving imports is ok).
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> Fat;
}

impl<'a> ResolveCtx<'a> {
    fn shorten_lifetime(&mut self) -> ResolveCtx {
        ResolveCtx {
            imports: self.imports.shorten_lifetime(),
            diagnostics: self.diagnostics,
            project_diagnostics: self.project_diagnostics,
            resolved: self.resolved,
            project_resolved: self.project_resolved,
        }
    }

    fn other_file<'b>(&'b mut self, path: &'b Path) -> ResolveCtx<'b> {
        ResolveCtx {
            imports: self.imports.other_file(path),
            diagnostics: self.project_diagnostics.file(path),
            project_diagnostics: self.project_diagnostics,
            resolved: self.project_resolved.file(path),
            project_resolved: self.project_resolved,
        }
    }
}

impl ProjectResolveCache {
    pub(crate) fn new() -> Self {
        Self {
            module_ids: FrozenVec::new(),
            by_file: FrozenMap::new()
        }
    }

    pub(crate) fn module_id(&self, path: impl AsRef<Path>) -> ModuleId {
        self.file(path).module_id
    }

    fn path(&self, module_id: ModuleId) -> &Path {
        &self.module_ids[module_id.into()]
    }

    fn add_module_id(&self, path: PathBuf) -> ModuleId {
        let id = ModuleId(self.module_ids.len());
        self.module_ids.push(path);
        id
    }

    fn file(&self, path: impl AsRef<Path>) -> &FileResolveCache {
        if let Some(file) = self.by_file.get(path.as_ref()) {
            return file
        }
        let path = path.as_ref().to_owned();
        let module_id = self.add_module_id(path.clone());
        self.by_file.insert(path, Box::new(FileResolveCache::new(module_id)))
    }
}

impl FileResolveCache {
    fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            types_in_progress: RefCell::new(HashSet::new()),
            types: FrozenMap::new()
        }
    }

    fn get_or_compute(&self, key: &ThinType, compute: impl FnOnce() -> FatType) -> Result<&FatType, RecursiveResolution> {
        if let Some(value) = self.types.get(key) {
            return Ok(value)
        }
        let true = self.types_in_progress.borrow_mut().insert(key.clone()) else {
            return Err(RecursiveResolution)
        };
        Ok(self.types.insert(key.clone(), Box::new(compute())))
    }
}

impl<T> IntoThin<()> for T {
    fn into_thin(self) -> () { () }
}

impl<Thin, Fat: IntoThin<Thin>> ResolvedLazy<Thin, Fat> {
    pub fn resolved(fat: Fat) -> Self {
        Self {
            imported_origin: None,
            thin: fat.ref_into_thin(),
            fat: OnceCell::with_value(fat)
        }
    }
}

impl<Thin: ResolveInto<Fat>, Fat> ResolvedLazy<Thin, Fat> {
    pub fn new(thin: Thin) -> Self {
        Self {
            imported_origin: None,
            thin,
            fat: OnceCell::new()
        }
    }
}

impl<Thin: ResolveInto<Fat>, Fat> ResolvedLazyTrait<Fat> for ResolvedLazy<Thin, Fat> {
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> &Fat {
        self.fat.get_or_init(|| {
            let mut ctx = self.local_ctx(ctx);
            self.thin.resolve(&mut ctx)
        })
    }
}

impl<Thin, Fat> ResolvedLazy<Thin, Fat> {
    /// Clones a possibly local [ResolvedLazy] and converts into an imported one which can be used
    /// from another module.
    ///
    /// If the [ResolvedLazy] is already imported this will *not* change the imported origin,
    /// instead if won't do anything (we still want to resolve at the real origin)
    pub fn imported_from(&self, origin: ModuleId) -> Self {
        Self {
            imported_origin: Some(self.imported_origin.unwrap_or(origin)),
            thin: self.thin.clone(),
            fat: self.fat.clone()
        }
    }

    /// Transforms the thin value and, if computed, the fat value as well.
    ///
    /// **SAFETY:** The transformations must ensure that the following rules hold:
    ///
    /// ```coq
    /// (∀ thin fat,     thin == fat.into_thin()  => f_thin(thin)              == f_fat(fat).into_thin()) /\
    /// (∀ thin fat ctx, thin.resolve(ctx) == fat => f_thin(thin.resolve(ctx)) == f_fat(fat))
    /// ```
    ///
    /// This can't be checked by Rust, so we mark the function as `unsafe`. Note that there is not
    /// actually any UB if the transformation does not hold though.
    pub unsafe fn bimap<NewThin, NewFat>(
        self,
        f_thin: impl FnOnce(Thin) -> NewThin,
        f_fat: impl FnOnce(Fat) -> NewFat,
    ) -> ResolvedLazy<NewThin, NewFat> {
        ResolvedLazy {
            imported_origin: self.imported_origin,
            thin: f_thin(self.thin),
            fat: OnceCell::with_option(self.fat.into_inner().map(f_fat))
        }
    }

    fn local_ctx<'a: 'b, 'b>(&self, ctx: &'b mut ResolveCtx<'a>) -> ResolveCtx<'b> {
        if let Some(imported_origin) = self.imported_origin {
            let imported_origin_path = ctx.project_resolved.path(imported_origin);
            ctx.other_file(imported_origin_path)
        } else {
            ctx.clone()
        }
    }
}

impl IntoThin<ThinType> for FatType {
    fn into_thin(self) -> ThinType {
        match self {
            FatType::Any => ThinType::Any,
            FatType::Never { nullability } => ThinType::Never { nullability },
            FatType::Structural { nullability, structure } => {
                ThinType::Structural { nullability, structure: structure.into_thin() }
            },
            FatType::Nominal { nullability, id, inherited: _ } => {
                ThinType::Nominal { nullability, id: id.into_thin() }
            }
            FatType::Hole { nullability: _, hole } => hole.unreachable(),
        }
    }

    fn ref_into_thin(&self) -> ThinType {
        match self {
            FatType::Any => ThinType::Any,
            FatType::Never { nullability } => ThinType::Never { nullability: *nullability },
            FatType::Structural { nullability, structure } => {
                ThinType::Structural { nullability: *nullability, structure: structure.ref_into_thin() }
            },
            FatType::Nominal { nullability, id, inherited: _ } => {
                ThinType::Nominal { nullability: *nullability, id: id.ref_into_thin() }
            }
            FatType::Hole { nullability: _, hole } => hole.unreachable(),
        }
    }
}

impl_by_map!(IntoThin (fn into_thin(self)) for TypeIdent, TypeStructure, TypeParam, ReturnType);

impl ResolveInto<FatType> for ThinType {
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> FatType {
        ctx; todo!()
    }
}

impl ResolveInto<FatTypeDecl> for TypeParam<ThinType> {
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> FatTypeDecl {
        let fat_param = <TypeParam<ThinType> as ResolveInto<TypeParam<FatType>>>::resolve(self, ctx);
        fat_param.into_decl(ctx.type_logger())
    }
}

impl ResolveInto<FatTypeDecl> for ThinTypeDecl {
    fn resolve(&self, ctx: &mut ResolveCtx<'_>) -> FatTypeDecl {
        ctx; todo!()
    }
}

impl_by_map!(ResolveInto (fn resolve(&self, ctx: &mut ResolveCtx<'_>)) for TypeParam, ReturnType);

impl RlType {
    pub const ANY: Self = Self {
        imported_origin: None,
        thin: ThinType::Any,
        fat: OnceCell::with_value(FatType::Any)
    };

    pub const NEVER: Self = Self {
        imported_origin: None,
        thin: ThinType::NEVER,
        fat: OnceCell::with_value(FatType::NEVER)
    };

    pub const NULL: Self = Self {
        imported_origin: None,
        thin: ThinType::NEVER,
        fat: OnceCell::with_value(FatType::NEVER)
    };
}

impl RlTypeDecl {
    pub const MISSING: Self = Self {
        imported_origin: None,
        thin: ThinTypeDecl::MISSING,
        // Cannot create because FatTypeDecl has a Box, so we just leave empty
        fat: OnceCell::new()
    };
}

impl RlReturnType {
    pub fn resolved_void() -> Self {
        Self::resolved(ReturnType::Void)
    }

    pub fn resolved_type(type_: FatType) -> Self {
        Self::resolved(ReturnType::Type(type_))
    }
}