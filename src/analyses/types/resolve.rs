use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::iter::{repeat, zip};
use std::ops::Deref;

use elsa::FrozenMap;
use indexmap::IndexSet;
use once_cell::unsync::OnceCell;

use crate::{debug, error, issue, ProjectCtx};
use crate::analyses::bindings::TypeNameStr;
use crate::analyses::scopes::{ActiveScopePtr, InactiveScopePtr, ScopeImportAlias, ScopeImportIdx, ScopeTypeImportIdx, ScopeValueImportIdx, WeakScopePtr};
use crate::analyses::types::{FatType, FatTypeArg, FatTypeDecl, FatTypeInherited, IdentType, Nullability, OptionalType, ReturnType, StructureType, ThinType, ThinTypeDecl, TypeParam, Variance};
use crate::analyses::types::generic::{IdentType, Nullability, OptionalType, ReturnType, StructureType, TypeParam, Variance};
use crate::ast::ann::{Ann, HasAnn};
use crate::compile::begin_transpile_file_no_cache;
use crate::diagnostics::{FileDiagnostics, FileLogger, ProjectDiagnostics, TypeLogger};
use crate::import_export::import_ctx::{FileImportCtx, ImportError};
use crate::import_export::ModulePath;
use crate::misc::{downcast_ref_with_lifetime, impl_by_map, OnceCellExt};

/// `ResolvedLazy` = lazy value which is "resolved" (computed) according to how NominalScript does
/// unordered cyclic resolution. It contains a **thin** (pre-resolution, context-sensitive) version
/// of the data, a scope (the context), and a lazily-computed **fat** (post-resolution,
/// context-free) version of the data.
///
/// The data represented by this may depend on each other, have arbitrary order, be in separate
/// (lazily-loaded) modules, and contain cycles (which must be detected and avoided). Consider the
/// following:
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
/// forward/circular data in [ResolvedLazy]s which initially only have the thin definition (where
/// all references are unresolved) and scope. Then, when information about the references are needed
/// (e.g. to unfold `A` to confirm that `[{ field; 1, another; 2 }]` can be wrapped by it), the "fat"
/// definition is computed (*forced*), which will recursively force the thin definitions of the
/// references. Furthermore, this is done in a context which keeps track of which thin definitions
/// are currently being forced, so that if a thin definition is forced again, it can be handled
/// appropriately. Like, for types:
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
pub struct ResolvedLazy<'tree, Thin, Fat> {
    /// Scope where the thin version is located, or [None] iff it's intrinsic
    scope: WeakScopePtr<'tree>,
    /// Thin version = unresolved context-sensitive data
    pub thin: Thin,
    /// Fat version = resolved context-free data
    fat: OnceCell<Fat>,
}

#[derive(Debug)]
/// Context containing imports, diagnostics, and in-progress resolved identifiers.
/// This context is for a file - however, it has access to other file data and will create
/// [ResolveCtx] for other files to resolve imports.
///
/// We the [FileDiagnostics] and [ModuleScopes] to prevent spurious lookups,
/// but also [ProjectDiagnostics] because we need to.
/// [FileImportCtx] has a reference to [ProjectImportCtx] so that one is covered.
pub struct ResolveCtx<'a, 'tree> {
    idents_being_resolved: RefCell<IndexSet<&'tree TypeNameStr>>,
    imports: FileImportCtx<'a>,
    diagnostics: &'a FileDiagnostics,
    project_diagnostics: &'a ProjectDiagnostics,
}

/// Resolved value depends on itself. Not necessarily an error, but must be handled explicitly.
#[derive(Debug, Clone, Copy)]
struct RecursiveResolution;

/// Trait implemented by [ResolvedLazy] which lets you use `DynResolvedLazy` values with arbitrary thin versions.
pub trait ResolvedLazyTrait<'tree, Fat: FatTrait<'tree>>: Debug {
    fn resolve(&self, ctx: &ResolveCtx<'_, 'tree>) -> &Fat;

    fn box_clone(&self) -> Box<DynResolvedLazy<'tree, Fat>>;
    /// Resolve, then clone this as an [RlType] (we must resolve to get enough info)
    fn normalize_clone(&self, ctx: &mut ResolveCtx<'_, 'tree>) -> ResolvedLazy<'tree, Fat::NormalThin, Fat>;
}

/// Trait implemented by the fat version of a [ResolvedLazy] ADT
pub trait FatTrait<'tree>: Debug + Clone {
    /// Thin version which can be constructed from this
    type NormalThin: FromFat<Self>;

    /// Attempt to downcast the thin type into [Self::NormalThin]
    fn downcast_thin(thin: &DynResolveInto<'tree, Self>) -> Option<&Self::NormalThin>;
}

/// Resolved lazy value where you don't know/care about the thin version (dynamically-sized)
pub type DynResolvedLazy<'tree, Fat> = dyn ResolvedLazyTrait<'tree, Fat>;

/// A resolved type. See [ResolvedLazy]
pub type RlType<'tree> = ResolvedLazy<'tree, ThinType<'tree>, FatType<'tree>>;
/// A resolved optional type. See [ResolvedLazy]
pub type RlOptionalType<'tree> = ResolvedLazy<'tree, OptionalType<'tree, ThinType<'tree>>, OptionalType<'tree, FatType<'tree>>>;
/// A resolved type parameter. "fat" value is `TypeParam::into_decl` because that's all we care
/// about. See [ResolvedLazy]
pub type RlTypeParam<'tree> = ResolvedLazy<'tree, TypeParam<'tree, ThinType<'tree>>, FatTypeDecl<'tree>>;
/// A resolved return type. See [ResolvedLazy]
pub type RlReturnType<'tree> = ResolvedLazy<'tree, ReturnType<'tree, ThinType<'tree>>, ReturnType<'tree, FatType<'tree>>>;
/// A resolved type declaration. See [ResolvedLazy]
pub type RlTypeDecl<'tree> = ResolvedLazy<'tree, ThinTypeDecl<'tree>, FatTypeDecl<'tree>>;
/// A resolved type from an imported value. See [ResolvedLazy]
pub type RlImportedValueType<'tree> = ResolvedLazy<'tree, ScopeValueImportIdx<'tree>, FatType<'tree>>;
/// A resolved type declaration from an imported type. See [ResolvedLazy]
pub type RlImportedTypeDecl<'tree> = ResolvedLazy<'tree, ScopeTypeImportIdx<'tree>, FatTypeDecl<'tree>>;
/// [RlType] with arbitrary thin version (dynamically-sized)
pub type DynRlType<'tree> = DynResolvedLazy<'tree, FatType<'tree>>;
/// [RlTypeDecl] with arbitrary thin version (dynamically-sized)
pub type DynRlTypeDecl<'tree> = DynResolvedLazy<'tree, FatTypeDecl<'tree>>;

/// A fat (with resolved context) version of data which can be stripped and converted back into the
/// thin (without resolved context) version.
pub trait ToThin<Thin> {
    /// Strips excess data and converts back into the thin version, cloning anything necessary.
    fn thin(&self) -> Thin;
}

/// The dual of [ToThin], auto-implemented like [Into] is auto-implemented for [From]
pub trait FromFat<Fat: ?Sized> {
    /// Same as [ToThin::thin]. When possible, please use that instead
    fn from_fat(fat: &Fat) -> Self;
}

/// A thin (without resolved context) version of data which can be resolved into the fat version
/// with [ResolveCtx].
pub trait ResolveInto<'tree, Fat> {
    /// Computes the fat version from the thin version, scope, and resolution context. Note that if
    /// the thin version was already computed before, it is cached and `resolve` may be skipped.
    /// Therefore `resolve` should have no side-effects except for ones that would be redundant
    /// (e.g. logging diagnostics and going through imports is ok).
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> Fat;
}
pub type DynResolveInto<'tree, Fat> = dyn ResolveInto<'tree, Fat> + 'tree;

impl<Fat: ToThin<Thin> + ?Sized, Thin> FromFat<Fat> for Thin {
    fn from_fat(fat: &Fat) -> Self {
        fat.thin()
    }
}

impl<'a, 'tree> ResolveCtx<'a, 'tree> {
    pub fn new<'b>(project_ctx: ProjectCtx<'a>, importer_path: &'b ModulePath) -> ResolveCtx<'b, 'tree> where 'a: 'b {
        ResolveCtx {
            idents_being_resolved: RefCell::new(IndexSet::new()),
            imports: project_ctx.import_ctx.file(importer_path),
            diagnostics: project_ctx.diagnostics.file(importer_path),
            project_diagnostics: project_ctx.diagnostics,
        }
    }

    fn other_file<'b, 'tree>(&'b self, path: &'b ModulePath) -> ResolveCtx<'b, 'tree> {
        ResolveCtx {
            idents_being_resolved: self.idents_being_resolved.clone(),
            imports: self.imports.other_file(path),
            diagnostics: self.project_diagnostics.file(path),
            project_diagnostics: self.project_diagnostics,
        }
    }

    pub(crate) fn logger(&self) -> FileLogger<'a> {
        FileLogger::new(self.diagnostics)
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

impl<Thin: FromFat<Fat>, Fat> ResolvedLazy<Thin, Fat> {
    pub fn resolved(fat: Fat) -> Self {
        Self {
            scope: WeakScopePtr::new(),
            thin: Thin::from_fat(&fat),
            fat: OnceCell::with_value(fat)
        }
    }
}

impl<'tree, Thin: ResolveInto<'tree, Fat>, Fat> ResolvedLazy<'tree, Thin, Fat> {
    pub fn new(scope: &InactiveScopePtr<'tree>, thin: Thin) -> Self {
        Self {
            scope: scope.downgrade(),
            thin,
            fat: OnceCell::new()
        }
    }

    //noinspection DuplicatedCode
    /// Return the cached fat type or resolve if not cached
    pub fn resolve(&self, ctx: &ResolveCtx<'_, 'tree>) -> &Fat {
        self.fat.get_or_init(|| {
            let scope = self.scope.upgrade().map(InactiveScopePtr::activate);
            self.thin.resolve(scope.as_ref(), ctx)
        })
    }

    //noinspection DuplicatedCode (can't just abstract the lines in get_or_init because self is consumed here)
    /// Resolve if not cached, then return the thin and fat types
    pub fn into_resolve(self, ctx: &ResolveCtx<'_, 'tree>) -> (Thin, Fat) {
        let fat = self.fat.into_inner().unwrap_or_else(|| {
            let scope = self.scope.upgrade().map(InactiveScopePtr::activate);
            self.thin.resolve(scope.as_ref(), ctx)
        });
        (self.thin, fat)
    }
}

impl<'tree, Thin: ResolveInto<'tree, Fat> + Debug + Clone + 'tree, Fat: FatTrait<'tree> + 'tree> ResolvedLazyTrait<Fat> for ResolvedLazy<Thin, Fat> where Fat::NormalThin: Clone {
    fn resolve(&self, ctx: &ResolveCtx<'_, 'tree>) -> &Fat {
        self.resolve(ctx)
    }

    fn box_clone(&self) -> Box<dyn ResolvedLazyTrait<Fat>> {
        Box::new(self.clone())
    }

    fn normalize_clone(&self, ctx: &ResolveCtx<'_, 'tree>) -> ResolvedLazy<Fat::NormalThin, Fat> {
        match Fat::downcast_thin(&self.thin as &DynResolveInfo<'tree, Fat>) {
            None => ResolvedLazy::resolved(self.resolve(ctx).clone()),
            Some(thin) => ResolvedLazy {
                scope: self.scope.clone(),
                thin: thin.clone(),
                fat: self.fat.clone()
            }
        }
    }
}

impl<'tree, Thin, Fat> ResolvedLazy<'tree, Thin, Fat> {
    /// Transforms the thin value and, if computed, the fat value as well.
    ///
    /// **Warning:** the transformations must ensure that the following rules hold:
    ///
    /// ```coq
    /// (∀ thin fat,     thin == fat.thin()       => f_thin(thin)              == f_fat(fat).thin()) /\
    /// (∀ thin fat ctx, thin.resolve(ctx) == fat => f_thin(thin.resolve(ctx)) == f_fat(fat))
    /// ```
    pub fn bimap<NewThin, NewFat>(
        self,
        f_thin: impl FnOnce(Thin) -> NewThin,
        f_fat: impl FnOnce(Fat) -> NewFat,
    ) -> ResolvedLazy<'tree, NewThin, NewFat> {
        ResolvedLazy {
            scope: self.scope,
            thin: f_thin(self.thin),
            fat: OnceCell::with_option(self.fat.into_inner().map(f_fat))
        }
    }

    /// [Self::bimap]s the thin and fat data from `other`'s types into this's types.
    ///
    /// **Warning:** as with [Self::bimap], the transformations must ensure that the following rules hold:
    ///
    /// ```coq
    /// (∀ thin fat,     thin == fat.thin()       => f_thin(thin)              == f_fat(fat).thin()) /\
    /// (∀ thin fat ctx, thin.resolve(ctx) == fat => f_thin(thin.resolve(ctx)) == f_fat(fat))
    /// ```
    pub fn from<OldThin: Into<Thin>, OldFat: Into<Fat>>(other: ResolvedLazy<OldThin, OldFat>) -> Self {
        other.bimap(|t| t.into(), |t| t.into())
    }
}

impl<'tree> FatTrait<'tree> for FatType<'tree> {
    type NormalThin = ThinType<'tree>;

    fn downcast_thin(thin: &DynResolveInto<'tree, Self>) -> Option<&Self::NormalThin> {
        downcast_ref_with_lifetime!(('tree, DynResolveInto<Self> => ThinType) thin)
    }
}

impl<'tree> ToThin<ThinType<'tree>> for FatType<'tree> {
    fn thin(&self) -> ThinType {
        match self {
            FatType::Any { ann } => {
                ThinType::Any { ann: *ann }
            },
            FatType::Never { ann, nullability } => {
                ThinType::Never { ann: *ann, nullability: *nullability }
            },
            FatType::Structural { ann, nullability, structure } => {
                ThinType::Structural { ann: *ann, nullability: *nullability, structure: structure.thin() }
            },
            FatType::Nominal { ann, nullability, id, inherited: _ } => {
                ThinType::Nominal { ann: *ann, nullability: *nullability, id: id.thin() }
            }
            FatType::Hole { ann, nullability, hole: _ } => {
                ThinType::dummy_for_hole(*ann, *nullability)
            },
        }
    }
}

impl<'tree> FatTrait<'tree> for FatTypeDecl<'tree> {
    type NormalThin = ThinTypeDecl<'tree>;

    fn downcast_thin(thin: &DynResolveInto<'tree, Self>) -> Option<&Self::NormalThin> {
        downcast_ref_with_lifetime!(('tree, DynResolveInto<Self> => ThinTypeDecl) thin)
    }
}

impl<'tree> ToThin<ThinTypeDecl<'tree>> for FatTypeDecl<'tree> {
    fn thin(&self) -> ThinTypeDecl<'tree> {
        ThinTypeDecl {
            ann: *self.ann,
            name: self.name.clone(),
            type_params: self.type_params.iter().map(|p| p.thin()).collect(),
            supertypes: self.inherited.super_ids.iter()
                .map(|id| ThinType::Nominal {
                    ann: id.ann,
                    nullability: Nullability::NonNullable,
                    id: id.thin(),
                })
                .chain(self.inherited.structure.as_ref().map(|structure| {
                    ThinType::Structural {
                        ann: *structure.ann(),
                        nullability: Nullability::NonNullable,
                        structure: structure.thin()
                    }
                }))
                .collect(),
            typescript_supertype: self.inherited.typescript_types.clone(),
            guard: self.inherited.guards.clone(),
        }
    }
}

impl_by_map!(
    <'tree, Lhs: crate::analyses::types::TypeTraitMapsFrom<Rhs>, Rhs: ToThin<Lhs> | crate::analyses::types::TypeTrait<'tree>> FatTrait<'tree> (
        type NormalThin
    ) for IdentType, StructureType, TypeParam, OptionalType, ReturnType
);

impl_by_map!(
    <'tree, Lhs: crate::analyses::types::TypeTraitMapsFrom<'tree, Rhs>, Rhs: crate::analyses::types::TypeTrait<'tree>> ToThin (
        fn thin(&self) by map_ref
    ) for IdentType, StructureType, TypeParam, OptionalType, ReturnType
);

impl<'tree> ResolveInto<FatType<'tree>> for ThinType<'tree> {
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> FatType {
        let e = FileLogger::new(ctx.diagnostics);
        match self {
            ThinType::Any { ann } => {
                FatType::Any { ann: *ann }
            },
            ThinType::Never { ann, nullability } => {
                FatType::Never { ann: *ann, nullability: *nullability }
            },
            ThinType::Structural { ann, nullability, structure } => {
                FatType::Structural { ann: *ann, nullability: *nullability, structure: structure.resolve(scope, ctx) }
            },
            ThinType::Nominal { ann, nullability, id } => {
                let id = id.resolve(scope, ctx);
                let inherited = if ctx.idents_being_resolved.borrow_mut().insert(id.name.name) {
                    let inherited = scope.and_then(|scope| {
                        scope.types.inherited(id.clone(), ctx).or_else(|| {
                            error!(e, "Unresolved type" => id);
                            None
                        })
                    }).unwrap_or_else(FatTypeInherited::empty);
                    ctx.idents_being_resolved.borrow_mut().shift_remove(&id.name);
                    inherited
                } else {
                    debug!(e, "Recursive type" => id);
                    FatTypeInherited::empty()
                };
                FatType::Nominal { ann: *ann, nullability: *nullability, id, inherited: Box::new(inherited) }
            }
            ThinType::IllegalVoid { ann } => {
                error!(e, "Void is not allowed here" => ann);
                FatType::null(*ann)
            }
        }
    }
}

impl<'tree> ResolveInto<'tree, IdentType<FatType<'tree>>> for IdentType<'tree, ThinType<'tree>> {
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> IdentType<FatType<'tree>> {
        // We will report missing type identifiers and bad # of generic args in another place
        let variances = scope.and_then(|scope| {
            scope.types.get(&self.name)
        }).map(|type_binding| {
            // TODO: Is the scope correct? (I assume so because the decl is from scope.types
            //    but also it may not actually be changed from the original)
            let type_decl = type_binding.type_decl().resolve(ctx);
            &type_decl.type_params
        }).into_iter().flatten().map(|type_param| type_param.variance_bound)
        // pad with default variance (bivariant) in case we fail to reoslve or there are extra type args
            .chain(repeat(Variance::Bivariant));
        IdentType {
            ann: *self.ann,
            name: self.name.clone(),
            generic_args: zip(variances, &self.generic_args).map(|(variance_bound, generic_arg)| {
                FatTypeArg {
                    variance_bound,
                    type_: generic_arg.resolve(scope, ctx)
                }
            }).collect()
        }
    }
}

impl<'tree> ResolveInto<FatTypeDecl<'tree>> for TypeParam<'tree, ThinType<'tree>> {
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> FatTypeDecl<'tree> {
        let fat_param = <TypeParam<'tree, ThinType<'tree>> as ResolveInto<TypeParam<'tree, FatType<'tree>>>>::resolve(self, scope, ctx);
        fat_param.into_decl()
    }
}

impl<'tree> ResolveInto<FatTypeDecl<'tree>> for ThinTypeDecl<'tree> {
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> FatTypeDecl<'tree> {
        let supers = self.supertypes.iter().map(|t| t.resolve(scope, ctx));
        let mut inherited = FatTypeInherited {
            super_ids: VecDeque::new(),
            structure: None,
            typescript_types: self.typescript_supertype.iter().cloned().collect(),
            guards: self.guard.iter().cloned().collect(),
            is_never: false,
        };
        for super_ in supers {
            inherited.unify_with_super(super_, TypeLogger::ignore());
        }
        FatTypeDecl {
            ann: *self.ann,
            name: self.name.clone(),
            type_params: self.type_params.iter().map(|p| p.resolve(scope, ctx)).collect(),
            inherited: Box::new(inherited),
        }
    }
}

impl<'tree, Alias: ScopeImportAlias<'tree>> ResolveInto<'tree, Alias::Fat> for ScopeImportIdx<Alias> {
    fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) -> Alias::Fat {
        let e = FileLogger::new(ctx.diagnostics);
        let scope = scope.expect("ScopeValueImportIdx::resolve: scope must be Some");
        let (import_path, import_node) = scope.import(self);

        let (module_path, module) = match ctx.imports.resolve_and_cache_transpile(&import_path.module_path, |script_path, module_path| {
            begin_transpile_file_no_cache(script_path, module_path, ctx.project_diagnostics.file(module_path)).map_err(ImportError::from)
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

impl_by_map!(
    <'tree, Lhs: crate::analyses::types::TypeTraitMapsFrom<Rhs>, Rhs: crate::analyses::types::TypeTrait> ResolveInto<'tree> (
        fn resolve(&self, scope: Option<&ActiveScopePtr<'tree>>, ctx: &ResolveCtx<'_, 'tree>) by map_ref
    ) for StructureType, TypeParam, OptionalType, ReturnType
);

impl<'tree> RlType<'tree> {
    pub const ANY: Self = Self::any(Ann::Intrinsic);

    pub const NEVER: Self = Self::never(Ann::Intrinsic);

    pub const NULL: Self = Self::null(Ann::Intrinsic);

    pub const fn any(ann: Ann<'tree>) -> Self {
        Self {
            scope: WeakScopePtr::new(),
            thin: ThinType::Any { ann },
            fat: OnceCell::with_value(FatType::Any { ann })
        }
    }

    pub const fn never(ann: Ann<'tree>) -> Self {
        Self {
            scope: WeakScopePtr::new(),
            thin: ThinType::never(ann),
            fat: OnceCell::with_value(FatType::never(ann))
        }
    }

    pub const fn null(ann: Ann<'tree>) -> Self {
        Self {
            scope: WeakScopePtr::new(),
            thin: ThinType::null(ann),
            fat: OnceCell::with_value(FatType::null(ann))
        }
    }

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

    /// **Panics** if this is not a structural function type.
    /// Replaces the return type.
    pub fn with_return_type(self, return_type: RlReturnType<'tree>, ctx: &ResolveCtx<'_, 'tree>) -> RlType<'tree> {
        let (thin_return_type, fat_return_type) = return_type.into_resolve(ctx);
        // SAFETY: bimap preserves thin/fat relation
        unsafe {
            self.bimap(
                |f_thin| {
                    let ThinType::Structural {
                        ann,
                        nullability: Nullability::NonNullable,
                        structure: StructureType::Fn(fn_type)
                    } = f_thin else {
                        panic!("ast function's (thin) type is not a structural function type");
                    };
                    ThinType::Structural {
                        ann,
                        nullability: Nullability::NonNullable,
                        structure: StructureType::Fn(Box::new(fn_type.with_return_type(thin_return_type)))
                    }
                },
                |f_fat| {
                    let FatType::Structural {
                        ann,
                        nullability: Nullability::NonNullable,
                        structure: StructureType::Fn(fn_type)
                    } = f_fat else {
                        panic!("ast function's (fat) type is not a structural function type");
                    };
                    FatType::Structural {
                        ann,
                        nullability: Nullability::NonNullable,
                        structure: StructureType::Fn(Box::new(fn_type.with_return_type(fat_return_type)))
                    }
                }
            )
        }
    }
}

impl<'tree> RlReturnType<'tree> {
    /// Functions with no `return` statement at the end implicitly return `Void` AKA this
    pub fn implicit_void(ann: Ann<'tree>) -> Self {
        Self::resolved(ReturnType::Void { ann })
    }

    pub fn resolved_type(type_: FatType<'tree>) -> Self {
        Self::resolved(ReturnType::Type(type_))
    }
}

/// [RlType] which is sync because the interior mutability is blocked ([OnceCell] is already filled).
/// It can be shared across threads, but the inner value can only be accessed if there is no
/// interior mutability, otherwise it will panic to prevent UB.
#[repr(transparent)]
struct SyncRlType<'tree>(RlType<'tree>);

impl<'tree> AsRef<RlType<'tree>> for SyncRlType<'tree> {
    fn as_ref(&self) -> &RlType<'tree> {
        assert!(self.0.fat.get().is_some(), "can't get inner value because there is possible unsafe interior mutability");
        // SAFETY: THIS instance is ok to be accessed across threads, because it has no interior mutability
        &self.0
    }
}

impl<'tree> Deref for SyncRlType<'tree> {
    type Target = RlType<'tree>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

unsafe impl<'tree> Sync for SyncRlType<'tree> {}

// SAFETY: THESE instances are sync because their `fat` cell has a value
static ANY_STATIC: SyncRlType<'static> = SyncRlType(RlType::ANY);
static NEVER_STATIC: SyncRlType<'static> = SyncRlType(RlType::NEVER);
static NULL_STATIC: SyncRlType<'static> = SyncRlType(RlType::NULL);

impl<'tree> RlTypeDecl<'tree> {
    pub const MISSING: Self = Self::missing(Ann::Intrinsic);

    pub const fn missing(ann: Ann<'tree>) -> Self {
        Self {
            scope: WeakScopePtr::new(),
            thin: ThinTypeDecl::missing(ann),
            // Cannot create and keep this const because FatTypeDecl has a Box, so we leave empty
            fat: OnceCell::new()
        }
    }

    /// Converts into the type of an instance of this declaration
    pub fn into_type(self) -> RlType<'tree> {
        unsafe { self.bimap(ThinTypeDecl::into_type, FatTypeDecl::into_type) }
    }
}
