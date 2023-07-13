use once_cell::sync::OnceCell;

use crate::semantic::expr::Type;
use crate::semantic::r#use::ValueUse;
use crate::semantic::storage::Id;

/// Value expression = either a value (identifier, builtin or structure) or operation which reduces to a value
pub type Expr<'tree> = Id<'tree, ExprData<'tree>>;
/// [Expr] data
#[derive(Debug)]
pub enum ExprData<'tree> {
    Identifier {
        /// Assigned and required type for type checking
        r#type: ExprType<'tree>,
        /// Identifier name and declaration referenced by this identifier, or [None] if this is a
        /// nonexistent reference
        r#use: Option<ValueUse<'tree>>
    },
    /// Misc operation, we only care about the sub-expressions
    Misc {
        /// Assigned and required type for type checking
        r#type: ExprType<'tree>,
        /// Immediate sub-expressions (e.g. operator arguments, or function call name and arguments)
        children: Expr<'tree>
    }
}

/// Assigned and required type for type checking.
///
/// This information is filled during analysis, then the types are checked after.
#[derive(Debug)]
pub struct ExprType<'tree> {
    /// The type this expression **is**
    assigned: OnceCell<Type<'tree>>,
    /// The type this expression **must be assigned** or there is a type error.
    ///
    /// If [CheckTypeAt::CompileTime], the assigned type must be a subtype of the required type.
    /// If [CheckTypeAt::Runtime], the assigned type must only be bivariant (either can be a
    /// subtype), and if its not a subtype, we will insert a guard at runtime.
    required: OnceCell<(Type<'tree>, CheckTypeAt)>
}

/// When the subtype relation is checked: whether the `assigned` type must be a subtype of the
/// `required` type, or instances must merely be instances of `required` at runtime.
#[derive(Debug, Clone, Copy)]
pub enum CheckTypeAt {
    /// Enforce that `assigned` is a subtype of `required` at compile-time
    CompileTime,
    /// Enforce that `assigned` is bivariant to `required`, and if not a subtype of `required`,
    /// insert a runtime guard to check instances are instances of `required` at runtime.
    Runtime
}

impl<'tree> ExprData<'tree> {
    #[inline]
    pub fn expr_type(&self) -> &ExprType<'tree> {
        match self {
            ExprData::Identifier { r#type, .. } => r#type,
            ExprData::Misc { r#type, .. } => r#type
        }
    }
}

impl<'tree> ExprType<'tree> {
    /// The type this expression **is**
    pub fn assigned(&self) -> Option<&Type<'tree>> {
        self.assigned.get()
    }

    /// The type this expression **must be assigned at compile-time** or there is a type error
    pub fn required(&self) -> Option<&Type<'tree>> {
        self.required.get().filter(|(_, c)| match c {
            CheckTypeAt::CompileTime => true,
            CheckTypeAt::Runtime => false
        }).map(|(t, _)| t)
    }

    /// The type this expression **must be an instance of at runtime** or there is a type error.
    /// This will raise a compile-time error if `assigned` is not bivariant to `required` (which
    /// means there are no valid assignments), and if `assigned` is not a subtype of `required`,
    /// the compiler will insert a runtime guard to check instances are instances of `required` at
    /// runtime.
    pub fn runtime_required(&self) -> Option<&Type<'tree>> {
        self.required.get().filter(|(_, c)| match c {
            CheckTypeAt::CompileTime => false,
            CheckTypeAt::Runtime => true
        }).map(|(t, _)| t)
    }

    /// Set the type this expression **is**. *Warns* if already assigned.
    pub fn assign(&self, r#type: Type<'tree>) {
        if let Err((real_type, r#type)) = self.assigned.try_insert(r#type) {
            log::warn!("Type already assigned to expression: {:?}. Tried to assign {:?} but won't because it already has {:?}", self, r#type, real_type);
        }
    }

    /// Set the type this expression **must be assigned at compile-time**. *Warns* if already
    /// required, and *warns* again if already runtime required.
    pub fn require(&self, r#type: Type<'tree>) {
        if let Err(((real_type, real_check_type), (r#type, _))) = self.required.try_insert((r#type, CheckTypeAt::CompileTime)) {
            log::warn!("Type already required for expression: {:?}. Tried to compile-time require {:?} but won't because it already requires {:?}", self, r#type, real_type);
            if !matches!(real_check_type, CheckTypeAt::CompileTime) {
                log::warn!("Cannot require type at compile-time and runtime: in {:?}, {:?} required at runtime, then {:?} required at compile time", self, real_type, r#type);
            }
        }
    }

    /// Set the type this expression **must be an instance of at runtime**. *Warns* if already
    /// required, and *warns* again if already compile-time required.
    pub fn runtime_require(&self, r#type: Type<'tree>) {
        if let Err(((real_type, real_check_type), (r#type, _))) = self.required.try_insert((r#type, CheckTypeAt::Runtime)) {
            log::warn!("Type already required for expression: {:?}. Tried to runtime require {:?} but won't because it already requires {:?}", self, r#type, real_type);
            if !matches!(real_check_type, CheckTypeAt::Runtime) {
                log::warn!("Cannot require type at compile-time and runtime: in {:?}, {:?} required at compile-time, then {:?} required at runtime", self, real_type, r#type);
            }
        }
    }
}
