use std::cell::Cell;
use crate::impl_has_ann_enum;
use crate::semantic::ann::Ann;
use crate::semantic::arena::Interned;
use crate::semantic::expr::Type;
use crate::semantic::r#use::ValueUse;

/// Value expression = either a value (identifier, builtin or structure) or operation which reduces to a value
pub type Expr<'tree> = Interned<'tree, OwnedExpr<'tree>>;
/// Owned [Expr]
#[derive(Debug)]
pub enum OwnedExpr<'tree> {
    Identifier {
        /// Source location
        ann: Ann<'tree>,
        /// Assigned and required type for type checking
        type_: ExprType<'tree>,
        /// Identifier name and declaration referenced by this identifier, or [None] if this is a
        /// nonexistent reference
        r#use: Option<ValueUse<'tree>>
    },
    /// Misc operation, we only care about the sub-expressions
    Misc {
        /// Source location
        ann: Ann<'tree>,
        /// Assigned and required type for type checking
        type_: ExprType<'tree>,
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
    assigned: Cell<Option<Type<'tree>>>,
    /// The type this expression **must be assigned** or there is a type error.
    ///
    /// If [CheckTypeAt::CompileTime], the assigned type must be a subtype of the required type.
    /// If [CheckTypeAt::Runtime], the assigned type must only be bivariant (either can be a
    /// subtype), and if its not a subtype, we will insert a guard at runtime.
    required: Cell<Option<(Type<'tree>, CheckTypeAt)>>
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
pub trait HasExprType<'tree> {
    fn expr_type(&self) -> &ExprType<'tree>;
}

impl_has_ann_enum!(OwnedExpr { Identifier, Misc });

impl<'tree> HasExprType<'tree> for OwnedExpr<'tree> {
    #[inline]
    fn expr_type(&self) -> &ExprType<'tree> {
        match self {
            OwnedExpr::Identifier { type_, .. } => type_,
            OwnedExpr::Misc { type_, .. } => type_
        }
    }
}

impl<'tree, 'a, T: HasExprType<'tree>> HasExprType<'tree> for Interned<'a, T> {
    #[inline]
    fn expr_type(&self) -> &ExprType<'tree> {
        self.as_ref().expr_type()
    }
}

impl<'tree> ExprType<'tree> {
    /// The type this expression **is**
    pub fn assigned(&self) -> Option<Type<'tree>> {
        self.assigned.get()
    }

    /// The type this expression **must be assigned at compile-time** or there is a type error
    pub fn required(&self) -> Option<Type<'tree>> {
        self.required.get().filter(|_, c| match c {
            CheckTypeAt::CompileTime => true,
            CheckTypeAt::Runtime => false
        }).map(|(t, _)| t)
    }

    /// The type this expression **must be an instance of at runtime** or there is a type error.
    /// This will raise a compile-time error if `assigned` is not bivariant to `required` (which
    /// means there are no valid assignments), and if `assigned` is not a subtype of `required`,
    /// the compiler will insert a runtime guard to check instances are instances of `required` at
    /// runtime.
    pub fn runtime_required(&self) -> Option<Type<'tree>> {
        self.required.get().filter(|_, c| match c {
            CheckTypeAt::CompileTime => false,
            CheckTypeAt::Runtime => true
        }).map(|(t, _)| t)
    }

    /// Set the type this expression **is**. If already assigned, logs an error and assigns to the
    /// type intersection.
    pub fn assign(&self, type_: Type<'tree>) {
        self.assigned.set(Some(match self.assigned.get() {
            None => type_,
            Some(old_type) => {
                log::error!("Type already assigned to expression: {:?}, {:?}, {:?}", self, old_type, type_);
                old_type & type_
            }
        }))
    }

    /// Set the type this expression **must be assigned at compile-time**. If already required, logs
    /// an error and requires the type intersection. If runtime-required, **panics**.
    pub fn require(&self, type_: Type<'tree>) {
        self.required.set(Some((match self.required.get() {
            None => type_,
            Some((old_type, CheckTypeAt::CompileTime)) => {
                log::error!("Type already required for expression: {:?}, {:?}, {:?}", self, old_type, type_);
                old_type & type_
            }
            Some((old_type, CheckTypeAt::Runtime)) => {
                panic!("Cannot require type at runtime and compile-time: {:?}, {:?}, {:?}", self, old_type, type_);
            }
        }, CheckTypeAt::CompileTime)))
    }

    /// Set the type this expression **must be an instance of at runtime**. If already runtime-
    /// required, logs an error and runtime-requires the type intersection. If compile-time
    /// required, **panics**.
    pub fn runtime_require(&self, type_: Type<'tree>) {
        self.required.set(Some((match self.required.get() {
            None => type_,
            Some((old_type, CheckTypeAt::CompileTime)) => {
                panic!("Cannot require type at compile-time and runtime: {:?}, {:?}, {:?}", self, old_type, type_);
            },
            Some((old_type, CheckTypeAt::Runtime)) => {
                log::error!("Type already runtime-required for expression: {:?}, {:?}, {:?}", self, old_type, type_);
                old_type & type_
            }
        }, CheckTypeAt::CompileTime)))
    }
}
