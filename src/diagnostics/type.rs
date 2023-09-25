use std::cell::RefCell;
use std::fmt::Display;

use smallvec::SmallVec;

use crate::{hint, issue, note};
use crate::diagnostics::{Diagnostic, Diagnostics};
use crate::semantic::expr::Type;
use crate::semantic::name::{FieldName, TypeName};
use crate::storage::Ann;
use crate::syntax::nodes;

/// Logs diagnostics in type-checking. Has extra context to do so easily.
///
/// Type methods which can log diagnostics (e.g. unification) take this as a parameter named
/// `e: TypeLogger` or `mut e: TypeLogger`. Instead of passing `e` directly to multiple children,
/// you must use `e.with_context(...)`
#[derive(Debug)]
pub struct TypeLogger<'a, 'b, 'tree>(_TypeLogger<'a, 'b, 'tree>);

#[derive(Debug)]
enum _TypeLogger<'a, 'b, 'tree> {
    Base(TypeLoggerBase<'b, 'tree>),
    Derived { base: &'a TypeLoggerBase<'b, 'tree> },
    Ignore
}

#[derive(Debug)]
struct TypeLoggerBase<'b, 'tree> {
    logger: &'b Diagnostics<'tree>,
    info: TypeCheckInfo<'tree>,
    // RULE A: This contains the context of the current TypeLogger:
    // - pushes when a new TypeLogger is created via with_context
    // - pops when that TypeLogger is destroyed.
    // If these are pointers, they are always active references though may have different lifetimes
    // (but they're not so currently we don't have to do anything unsafe)
    context: RefCell<SmallVec<[TypeLoc<'tree>; 2]>>
}

/// The additional info displayed to the user in type-check errors
#[derive(Debug)]
pub struct TypeCheckInfo<'tree> {
    pub type_check_loc: Ann<'tree>,
    pub assigned_info: TypeInfo<'tree>,
    pub required_info: TypeInfo<'tree>,
}

/// Part of the additional info displayed to the user in type-check errors;
/// the part for the assigned or required type
#[derive(Debug)]
pub struct TypeInfo<'tree> {
    pub r#type: Type<'tree>,
    pub defined_value: Option<nodes::Expression<'tree>>,
    pub explicit_type: Option<nodes::NominalTypeAnnotation<'tree>>,
}

/// Step to get from one type to one of its inner types (generalized type arguments), or from a type
/// decl to one of its supertypes. There are also "in-between" steps like `SuperStructure`.
#[derive(Debug, Clone)]
pub enum TypeLoc<'tree> {
    Supertype { index: usize },
    SuperIdGeneric { name: &'tree TypeName },
    SuperStructure,
    TypeArgument { index: usize },
    FunctionTypeParam { name: &'tree TypeName },
    FunctionThisParam,
    FunctionParam { index: usize },
    FunctionRestParam,
    FunctionReturn,
    ArrayElement,
    TupleElement { index: usize },
    ObjectField { name: &'tree FieldName },
    /// For misc position
    Position { index: usize },
}

impl<'b, 'tree> TypeLogger<'b, 'b, 'tree> {
    pub fn new(e: &'b Diagnostics<'tree>, info: TypeCheckInfo<'tree>) -> Self {
        TypeLogger(_TypeLogger::Base(TypeLoggerBase {
            logger: e,
            info,
            context: RefCell::new(SmallVec::new())
        }))
    }
}

impl<'a, 'b, 'tree> TypeLogger<'a, 'b, 'tree> {
    pub fn ignore() -> TypeLogger<'a, 'b, 'tree> {
        TypeLogger(_TypeLogger::Ignore)
    }

    pub fn with_context<'a2>(
        &'a2 self,
        context: TypeLoc<'tree>
    ) -> TypeLogger<'a2, 'b, 'tree> {
        // ^ RULE A -> will pop context on derived drop
        match self.base() {
            None => TypeLogger::ignore(),
            Some(base) => {
                base.context.borrow_mut().push(context);
                TypeLogger(_TypeLogger::Derived { base })
            }
        }
    }

    fn base(&self) -> Option<&TypeLoggerBase<'b, 'tree>> {
        match &self.0 {
            _TypeLogger::Base(base) => Some(base),
            _TypeLogger::Derived { base } => Some(base),
            _TypeLogger::Ignore => None
        }
    }

    pub fn insert(&self, mut diagnostic: Diagnostic<'tree>) {
        let Some(base) = self.base() else {
            return
        };
        let TypeCheckInfo {
            type_check_loc,
            assigned_info: TypeInfo {
                r#type: assigned_type,
                defined_value: assigned_value,
                explicit_type: assigned_explicit_type
            },
            required_info: TypeInfo {
                r#type: required_type,
                defined_value: required_value,
                explicit_type: required_explicit_type
            }
        } = &base.info;
        // diagnostic.add_loc(*type_check_loc, todo!("store"));
        let context_borrow = base.context.borrow();
        for context in &*context_borrow {
            diagnostic.add_info(note!("in {}", context));
        }
        diagnostic.add_info(issue!("assigned type" @ *assigned_type.ann()));
        diagnostic.add_info(issue!("required type" @ *required_type.ann()));
        if let Some(assigned_value) = assigned_value {
            diagnostic.add_info(hint!("assigned type inferred here" @ Ann::direct(*assigned_value)));
        }
        if let Some(required_value) = required_value {
            diagnostic.add_info(hint!("required type inferred here" @ Ann::direct(*required_value)));
        }
        if let Some(assigned_explicit_type) = assigned_explicit_type {
            diagnostic.add_info(hint!("assigned type defined here" @ Ann::direct(*assigned_explicit_type)));
        }
        if let Some(required_explicit_type) = required_explicit_type {
            diagnostic.add_info(hint!("required type defined here" @ Ann::direct(*required_explicit_type)));
        }
        base.logger.insert(diagnostic)
    }
}

impl<'a, 'b, 'tree> Drop for _TypeLogger<'a, 'b, 'tree> {
    fn drop(&mut self) {
        match self {
            _TypeLogger::Base(base) => {
                // Sanity check
                debug_assert!(base.context.borrow().is_empty());
            },
            _TypeLogger::Derived { base } => {
                // ^ RULE A: Must pop top context as it's no longer alive
                base.context.borrow_mut().pop();
            },
            _TypeLogger::Ignore => {}
        }
    }
}

impl<'tree> Display for TypeLoc<'tree> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeLoc::Supertype { index } => write!(f, "supertype {}", index),
            TypeLoc::SuperIdGeneric { name } => write!(f, "super id type argument {}", name),
            TypeLoc::SuperStructure => write!(f, "super structure"),
            TypeLoc::TypeArgument { index } => write!(f, "type argument {}", index),
            TypeLoc::ArrayElement => write!(f, "array element"),
            TypeLoc::TupleElement { index } => write!(f, "element {}", index),
            TypeLoc::ObjectField { name } => write!(f, "field {}", name),
            TypeLoc::FunctionTypeParam { name } => write!(f, "type parameter {}", name),
            TypeLoc::FunctionThisParam => write!(f, "this parameter"),
            TypeLoc::FunctionParam { index } => write!(f, "parameter {}", index),
            TypeLoc::FunctionRestParam => write!(f, "rest parameter"),
            TypeLoc::FunctionReturn => write!(f, "return type"),
            TypeLoc::Position { index } => write!(f, "position {}", index),
        }
    }
}