use std::collections::HashMap;
use std::sync::Arc;
use once_cell::sync::OnceCell;
use crate::analyses::bindings::TypeName;
use crate::analyses::types::{FatType, FatTypeDecl, ReturnType, ThinType, ThinTypeDecl, TypeParam};
use crate::ast::InProjectLoc;
use crate::ast::tree_sitter::TSNode;
use crate::ast::typed_nodes::AstType;
use crate::diagnostics::ProjectDiagnostics;
use crate::import_export::export::ModulePath;
use crate::import_export::import_ctx::ImportCtx;

/// Types and values which must go through resolution which may have arbitrary order and even
/// contain cycles (the cycles will be checked and avoided).
/// This datastructure consists of a `thin` (pre-resolved) value and a `fat` (post-resolved) value.
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
/// even make `C` reference them! But all of this is valid code.
///
/// To handle this, the AST nodes encode `A`, `B`, and all other potentially forward/circular data
/// in [Resolved] datastructures, which are essentially fancy `Lazy` values. The datastructure
/// starts out with only a "thin" definition, which is computed solely from what is parsed i.e.
/// local. Then, when information about `A` or `B`'s inner structure is needed (e.g. to confirm that
/// `[{ field; 1, another; 2 }]` can be wrapped by `A`), the "fat" definition is computed or
/// *forced*.
///
/// Importantly, the computation for the fat definition has context, which (besides diagnostic and
/// import resolution capabilities), includes the "thin" versions of outer types which are currently
/// being forced while forcing the inner type. So when forcing a type would ordinarily cause the
/// type to be forced again, leading to an infinite loop or deadlock or some other issue, the
/// instead the forcing computation does something like:
///
/// - If a nested definition (like the above, simply leaves the type unexpanded)
/// - If a type is its own supertype, logs a diagnostic error because this is illegal
/// - (Other cases which may not be documented, the point is it does not try to force again)
///
/// After the type is forced, it is stored within the [Resolved] so that it does not need to be
/// forced again. The resolution context also maps the "thin" value to the "fat" value so that
/// we do not have to force redundantly.
///
/// TODO: Further explain and also document the two/three-pass compilation process
#[derive(Debug, PartialEq, Eq)]
pub struct Resolved<Thin, Fat> {
    /// Where the value or type was created, and whether it was explicitly provided or inferred
    pub creation: Creation,
    /// Thin version of the resolved value
    pub thin: Thin,
    /// Fat version of the resolved value
    fat: OnceCell<Fat>,
    /// Computes the fat version from the thin version and resolution context. Note that if the
    /// thin version was already computed before, it is cached, so `compute` should have no side-
    /// effects except for ones that would be redundant (e.g. logging diagnostics is ok).
    compute: fn(&mut ResolutionCtx<'_>, &Thin) -> Fat
}

/// How a type or value was created. Used for diagnostics
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Creation {
    /// `loc` points to a syntax node of the provided type
    ExplicitlyProvided { loc: InProjectLoc },
    /// Type was inferred from the expression at `loc`.
    Inferred { loc: InProjectLoc },
}

/// Context containing imports, diagnostics, and cached resolved values
pub struct ResolutionCtx<'a> {
    imports: &'a mut ImportCtx,
    diagnostics: &'a mut ProjectDiagnostics,
    /// Cache of resolved values *and* values currently being resolved, to avoid infinite recursion
    /// when resolving types
    resolved: &'a mut ResolvedCache,
}

struct ResolvedCache {
    types: HashMap<ThinType, PartiallyComputed<FatType>>,
    params: HashMap<TypeParam<ThinType>, FatTypeDecl>,
    imported_types: HashMap<(ModulePath, TypeName), PartiallyComputed<FatType>>,
}

enum PartiallyComputed<T> {
    InProgress,
    Computed(T)
}

/// A resolved type. See [Resolved]
pub type ResolvedType = Resolved<ThinType, FatType>;
/// A resolved type parameter. "fat" value is `TypeParam::into_decl` because that's all we care
/// about. See [Resolved]
pub type ResolvedTypeParam = Resolved<TypeParam<ThinType>, FatTypeDecl>;
/// A resolved return type. See [Resolved]
pub type ResolvedReturnType = Resolved<ReturnType<ThinType>, ReturnType<FatType>>;
/// A resolved type declaration. See [Resolved]
pub type ResolvedTypeDecl = Resolved<ThinTypeDecl, FatTypeDecl>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredReturnType<'tree> {
    pub type_: ReturnType<FatType>,
    pub return_node: Option<TSNode<'tree>>,
    pub explicit_type: Option<AstType<'tree>>,
}

impl<'tree> InferredType<'tree> {
    pub fn new(type_: FatType, inferred_from: TSNode<'tree>) -> Self {
        Self { type_, inferred_from, explicit_type: None }
    }
}

impl<'a, 'tree> From<&'a AstType<'tree>> for InferredType<'tree> {
    fn from(type_: &'a AstType<'tree>) -> Self {
        Self {
            type_: type_.type_.clone(),
            inferred_from: type_.node,
            explicit_type: Some(type_.clone())
        }
    }
}