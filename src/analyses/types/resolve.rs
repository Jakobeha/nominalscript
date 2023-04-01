use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;

use elsa::FrozenMap;
use once_cell::unsync::OnceCell;

use crate::{error, issue};
use crate::analyses::scopes::{RawScopePtr, ScopeImportAlias, ScopeImportIdx, ScopePtr, ScopeTypeImportIdx, ScopeValueImportIdx};
use crate::analyses::types::{FatType, FatTypeDecl, Nullability, OptionalType, ReturnType, ThinType, ThinTypeDecl, TypeIdent, TypeParam, TypeStructure};
use crate::compile::begin_transpile_file_no_cache;
use crate::diagnostics::{FileDiagnostics, FileLogger, ProjectDiagnostics, TypeLogger};
use crate::import_export::import_ctx::{FileImportCtx, ImportError};
use crate::import_export::ModulePath;
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
    /// Scope-id to resolve the data
    ///
    /// This pointer is either null or guaranteed to be live when the [ResolveCtx] is in scope
    scope_origin: Option<RawScopePtr>,
    /// Thin version = unresolved data
    pub thin: Thin,
    /// Fat version = resolved data
    fat: OnceCell<Fat>,
    // compute: fn(&Thin, &ResolveCtx<'_>) -> Fat
}

#[derive(Debug)]
/// Context containing imports, diagnostics, and cached resolved values.
/// This context is for a file - however, it has access to other file data and will create
/// [ResolveCtx] for other files to resolve imports.
///
/// We the [FileDiagnostics] and [ModuleScopes] to prevent spurious lookups,
/// but also [ProjectDiagnostics] because we need to.
/// [FileImportCtx] has a reference to [ProjectImportCtx] so that one is covered.
pub struct ResolveCtx<'a> {
    imports: FileImportCtx<'a>,
    diagnostics: &'a FileDiagnostics,
    project_diagnostics: &'a ProjectDiagnostics,
}

/// Cache of resolved data *and* data currently being resolved, to avoid infinite recursion
pub(crate) struct ResolveCache {
    types_in_progress: RefCell<HashSet<ThinType>>,
    types: FrozenMap<ThinType, Box<FatType>>,
}

/// Resolved value depends on itself. Not necessarily an error, but must be handled explicitly.
#[derive(Debug, Clone, Copy)]
struct RecursiveResolution;

/// Trait implemented by [ResolvedLazy] which lets you use `DynResolvedLazy` values with arbitrary thin versions.
pub trait ResolvedLazyTrait<Fat>: Debug {
    fn resolve(&self, ctx: &ResolveCtx<'_>) -> &Fat;

    fn box_clone(&self) -> Box<dyn ResolvedLazyTrait<Fat>>;
}

/// Resolved lazy value where you don't know/care about the thin version (dynamically-sized)
pub type DynResolvedLazy<Fat> = dyn ResolvedLazyTrait<Fat>;

/// A resolved type. See [ResolvedLazy]
pub type RlType = ResolvedLazy<ThinType, FatType>;
/// A resolved optional type. See [ResolvedLazy]
pub type RlOptionalType = ResolvedLazy<OptionalType<ThinType>, OptionalType<FatType>>;
/// A resolved type parameter. "fat" value is `TypeParam::into_decl` because that's all we care
/// about. See [ResolvedLazy]
pub type RlTypeParam = ResolvedLazy<TypeParam<ThinType>, FatTypeDecl>;
/// A resolved return type. See [ResolvedLazy]
pub type RlReturnType = ResolvedLazy<ReturnType<ThinType>, ReturnType<FatType>>;
/// A resolved type declaration. See [ResolvedLazy]
pub type RlTypeDecl = ResolvedLazy<ThinTypeDecl, FatTypeDecl>;
/// A resolved type from an imported value. See [ResolvedLazy]
pub type RlImportedValueType = ResolvedLazy<ScopeValueImportIdx, FatType>;
/// A resolved type declaration from an imported type. See [ResolvedLazy]
pub type RlImportedTypeDecl = ResolvedLazy<ScopeTypeImportIdx, FatTypeDecl>;
/// [RlType] with arbitrary thin version (dynamically-sized)
pub type DynRlType = DynResolvedLazy<FatType>;
/// [RlTypeDecl] with arbitrary thin version (dynamically-sized)
pub type DynRlTypeDecl = DynResolvedLazy<FatTypeDecl>;

/// A fat (with resolved context) version of data which can be stripped and converted back into the
/// thin (without resolved context) version.
pub trait ToThin<Thin> {
    /// Strips excess data and converts back into the thin version, cloning anything necessary.
    fn thin(&self) -> Thin;
}

/// A thin (without resolved context) version of data which can be resolved into the fat version
/// with [ResolveCtx].
pub trait ResolveInto<Fat> {
    /// Computes the fat version from the thin version, scope, and resolution context. Note that if
    /// the thin version was already computed before, it is cached and `resolve` may be skipped.
    /// Therefore `resolve` should have no side-effects except for ones that would be redundant
    /// (e.g. logging diagnostics and going through imports is ok).
    fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) -> Fat;
}

impl<'a> ResolveCtx<'a> {
    fn other_file<'b>(&'b self, path: &'b ModulePath) -> ResolveCtx<'b> {
        ResolveCtx {
            imports: self.imports.other_file(path),
            diagnostics: self.project_diagnostics.file(path),
            project_diagnostics: self.project_diagnostics,
        }
    }

    pub(crate) fn type_logger(&self) -> TypeLogger<'_, '_, '_> {
        todo!()
    }
}

impl ResolveCache {
    pub(crate) fn new() -> Self {
        Self {
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

impl<Thin, Fat: ToThin<Thin>> ResolvedLazy<Thin, Fat> {
    pub fn resolved(fat: Fat) -> Self {
        Self {
            scope_origin: None,
            thin: fat.thin(),
            fat: OnceCell::with_value(fat)
        }
    }
}

impl<Thin: ResolveInto<Fat>, Fat> ResolvedLazy<Thin, Fat> {
    pub fn new(scope: &ScopePtr<'_>, thin: Thin) -> Self {
        Self {
            scope_origin: Some(ScopePtr::as_raw(scope)),
            thin,
            fat: OnceCell::new()
        }
    }
}

impl<Thin: ResolveInto<Fat>, Fat> ResolvedLazy<Thin, Fat> {
    fn resolve(&self, ctx: &ResolveCtx<'_>) -> &Fat {
        self.fat.get_or_init(|| {
            // SAFETY: ctx is alive so the scope pointer must also be
            let scope = self.scope_origin.map(|scope_origin| unsafe { scope_origin.upgrade() } );
            self.thin.resolve(scope.as_ref(), ctx)
        })
    }
}

impl<Thin: ResolveInto<Fat> + Debug + Clone + 'static, Fat: Debug + Clone + 'static> ResolvedLazyTrait<Fat> for ResolvedLazy<Thin, Fat> {
    fn resolve(&self, ctx: &ResolveCtx<'_>) -> &Fat {
        self.resolve(ctx)
    }

    fn box_clone(&self) -> Box<dyn ResolvedLazyTrait<Fat>> {
        Box::new(self.clone())
    }
}

impl<Thin, Fat> ResolvedLazy<Thin, Fat> {
    /// Transforms the thin value and, if computed, the fat value as well.
    ///
    /// **SAFETY:** The transformations must ensure that the following rules hold:
    ///
    /// ```coq
    /// (∀ thin fat,     thin == fat.thin()       => f_thin(thin)              == f_fat(fat).thin()) /\
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
            scope_origin: self.scope_origin,
            thin: f_thin(self.thin),
            fat: OnceCell::with_option(self.fat.into_inner().map(f_fat))
        }
    }
}

impl ToThin<ThinType> for FatType {
    fn thin(&self) -> ThinType {
        match self {
            FatType::Any => ThinType::Any,
            FatType::Never { nullability } => ThinType::Never { nullability: *nullability },
            FatType::Structural { nullability, structure } => {
                ThinType::Structural { nullability: *nullability, structure: structure.thin() }
            },
            FatType::Nominal { nullability, id, inherited: _ } => {
                ThinType::Nominal { nullability: *nullability, id: id.thin() }
            }
            FatType::Hole { nullability, hole: _ } => ThinType::dummy_for_hole(*nullability),
        }
    }
}

impl ToThin<ThinTypeDecl> for FatTypeDecl {
    fn thin(&self) -> ThinTypeDecl {
        ThinTypeDecl {
            name: self.name.clone(),
            type_params: self.type_params.iter().map(|p| p.thin()).collect(),
            supertypes: self.inherited.super_ids.iter()
                .map(|id| ThinType::Nominal {
                    nullability: Nullability::NonNullable,
                    id: id.thin(),
                })
                .chain(self.inherited.structure.as_ref().map(|structure| {
                    ThinType::Structural {
                        nullability: Nullability::NonNullable,
                        structure: structure.thin()
                    }
                }))
                .collect(),
            // TODO: typescript union of self.inherited.typescript_types
            typescript_supertype: None,
            // TODO: merge self.inherited.guards
            guard: None,
        }
    }
}

impl_by_map!(<crate::analyses::types::TypeTrait> ToThin (fn thin(&self) by map_ref) for TypeIdent, TypeStructure, TypeParam, OptionalType, ReturnType);

impl ResolveInto<FatType> for ThinType {
    fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) -> FatType {
        ctx; todo!()
    }
}

impl ResolveInto<FatTypeDecl> for TypeParam<ThinType> {
    fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) -> FatTypeDecl {
        let fat_param = <TypeParam<ThinType> as ResolveInto<TypeParam<FatType>>>::resolve(self, scope, ctx);
        fat_param.into_decl(ctx.type_logger())
    }
}

impl ResolveInto<FatTypeDecl> for ThinTypeDecl {
    fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) -> FatTypeDecl {
        ctx; todo!()
    }
}

impl<Alias: ScopeImportAlias> ResolveInto<Alias::Fat> for ScopeImportIdx<Alias> {
    fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) -> Alias::Fat {
        let e = FileLogger::new(ctx.diagnostics);
        let scope = scope.expect("ScopeValueImportIdx::resolve: scope must be Some");
        let (import_path, import_node) = scope.import(self);

        let (module_path, module) = match ctx.imports.resolve_and_cache_transpile(&import_path.module_path, |script_path, module_path| {
            begin_transpile_file_no_cache(script_path, ctx.project_diagnostics.file(module_path)).map_err(ImportError::from)
        }) {
            Err(import_error) => {
                error!(e,
                    "Could not resolve imported module '{}'", import_path.module_path => import_path.node;
                    issue!("{}", import_error));
                return Alias::Fat::default()
            }
            Ok(module) => module
        };
        let imported = match module.exports.get(&self.imported_name) {
            None => {
                error!(e, "{} not exported from module '{}': `{}`", Alias::capital_name_str(), import_path.module_path, self.imported_name => import_node);
                return Alias::Fat::default()
            }
            Some(imported) => imported
        };
        imported.resolve(&ctx.other_file(module_path)).clone()
    }
}

impl_by_map!(<crate::analyses::types::TypeTrait> ResolveInto (fn resolve(&self, scope: Option<&ScopePtr<'_>>, ctx: &ResolveCtx<'_>) by map_ref) for TypeParam, OptionalType, ReturnType);

impl RlType {
    pub const ANY: Self = Self {
        scope_origin: None,
        thin: ThinType::Any,
        fat: OnceCell::with_value(FatType::Any)
    };

    pub const NEVER: Self = Self {
        scope_origin: None,
        thin: ThinType::NEVER,
        fat: OnceCell::with_value(FatType::NEVER)
    };

    pub const NULL: Self = Self {
        scope_origin: None,
        thin: ThinType::NEVER,
        fat: OnceCell::with_value(FatType::NEVER)
    };

    pub fn any_ref() -> &'static Self {
        // SAFETY: THIS instance has no possible interior mutability (`fat` cell has a value)
        ANY_STATIC.as_ref()
    }

    pub fn never_ref() -> &'static Self {
        // SAFETY: THIS instance has no possible interior mutability (`fat` cell has a value)
        NEVER_STATIC.as_ref()
    }

    pub fn null_ref() -> &'static Self {
        // SAFETY: THIS instance has no possible interior mutability (`fat` cell has a value)
        NULL_STATIC.as_ref()
    }
}

/// [RlType] which is sync because the interior mutability is blocked ([OnceCell] is already filled).
/// It can be shared across threads, but the inner value can only be accessed if there is no
/// interior mutability, otherwise it will panic to prevent UB.
#[repr(transparent)]
struct SyncRlType(RlType);

impl AsRef<RlType> for SyncRlType {
    fn as_ref(&self) -> &RlType {
        assert!(self.0.fat.get().is_some(), "can't get inner value because there is possible unsafe interior mutability");
        // SAFETY: THIS instance is ok to be accessed across threads, because it has no interior mutability
        &self.0
    }
}

impl Deref for SyncRlType {
    type Target = RlType;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

unsafe impl Sync for SyncRlType {}

// SAFETY: THESE instances are sync because their `fat` cell has a value
static ANY_STATIC: SyncRlType = SyncRlType(RlType::ANY);
static NEVER_STATIC: SyncRlType = SyncRlType(RlType::NEVER);
static NULL_STATIC: SyncRlType = SyncRlType(RlType::NULL);

impl RlTypeDecl {
    pub const MISSING: Self = Self {
        scope_origin: None,
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

impl Debug for ResolveCache {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolveCache")
            .field("types_in_progress", &self.types_in_progress.borrow())
            .field("types", &"...")
            .finish()
    }
}