use std::cell::Cell;
use std::cmp::Ordering;
use std::ops::{Deref, DerefMut};
use indexmap::IndexMap;
use type_sitter_lib::UntypedNode;
use typed_arena_nomut::Arena;
use crate::{debug, note, impl_has_ann_enum, impl_has_ann_record_struct, warning};
use crate::semantic::ann::{Ann, HasAnn};
use crate::semantic::def::{OwnedTypeDef, OwnedValueDef, TypeDef, ValueDef};
use crate::semantic::expr::{Expr, OwnedExpr, OwnedType};
pub use toplevel::*;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::semantic::arena::{AnnArena, IdentityRef, NameArena};
use crate::semantic::name::{TypeName, ValueName};
use crate::semantic::SemanticPhase;

mod toplevel;

/// e.g. toplevel scope, class scope, function scope
pub type Scope<'tree> = IdentityRef<'tree, OwnedScope<'tree>>;
#[derive(Debug)]
// #[derive(MultiPhase)] #[phase(SemanticPhase)]
pub struct OwnedScope<'tree> {
    /// The scope node
    pub ann: Ann<'tree>,
    /// The parent scope
    parent: Option<Scope<'tree>>,
    /// The scope's phase
    phase: SemanticPhase,
    /// The child scopes
    children: AnnArena<'tree, OwnedScope<'tree>>,
    /// Value definitions
    value_defs: NameArena<'tree, OwnedValueDef<'tree>>,
    /// Type definitions
    type_defs: NameArena<'tree, OwnedTypeDef<'tree>>,
    /// Value expressions
    // #[phase(SemanticPhase::Expressions)]
    exprs: AnnArena<'tree, OwnedExpr<'tree>>,
    /// Types
    // #[phase(SemanticPhase::Expressions)]
    types: AnnArena<'tree, OwnedType<'tree>>,
    /// Scope `return` or `throw` if present
    // #[phase(SemanticPhase::Expressions)]
    exit: Cell<Option<ScopeExit<'tree>>>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeExit<'tree> {
    Return {
        /// Source location
        ann: Ann<'tree>,
        /// Returned expression
        expr: Expr<'tree>
    },
    Throw {
        /// Source location
        ann: Ann<'tree>,
        /// Returned expression
        expr: Expr<'tree>
    }
}

impl_has_ann_record_struct!(OwnedScope);
impl_has_ann_enum!(ScopeExit { Return, Throw });

impl<'tree> OwnedScope<'tree> {
    /// Create a new, empty scope
    fn new(ann: Ann<'tree>, parent: Option<Scope<'tree>>) -> Self {
        Self {
            ann,
            parent,
            phase: SemanticPhase::Definitions,
            children: AnnArena::new(),
            value_defs: NameArena::new(),
            type_defs: NameArena::new(),
            exprs: AnnArena::new_unset(),
            types: AnnArena::new_unset(),
            exit: Cell::new(None),
        }
    }

    /// Transition to [SemanticPhase::Expressions] phase
    pub fn conv_expressions(&mut self) {
        self.phase = SemanticPhase::Expressions;
        self.children.conv_retrieval();
        self.value_defs.conv_retrieval();
        self.type_defs.conv_retrieval();
        self.exprs.conv_insertion();
        self.types.conv_insertion()
    }

    /// Transition to [SemanticPhase::TypeChecking] phase
    pub fn conv_type_checking(&mut self) {
        self.phase = SemanticPhase::TypeChecking;
        self.exprs.conv_retrieval();
        self.types.conv_retrieval();
    }

    /// Transition to [SemanticPhase::Removal] phase
    pub fn conv_removal(&mut self) {
        self.phase = SemanticPhase::Removal;
        self.children.conv_removal();
        self.value_defs.conv_removal();
        self.type_defs.conv_removal();
        self.exprs.conv_removal();
        self.types.conv_removal();
    }

    /// Translation to [SemanticPhase::Definitions] phase
    pub fn conv_definitions(&mut self) {
        self.phase = SemanticPhase::Definitions;
        self.children.conv_insertion();
        self.value_defs.conv_insertion();
        self.type_defs.conv_insertion();
        self.exprs.conv_insertion();
        self.types.conv_insertion();
    }
}

impl<'tree> Scope<'tree> {
    /// Get the scope's current phase
    pub fn phase(&self) -> SemanticPhase {
        self.phase
    }

    /// Add a child scope
    pub fn add_child(&self, ann: Ann<'tree>) -> Scope<'tree> {
        assert_eq!(self.phase(), SemanticPhase::Definitions, "Scope::add_child can only be called in the Definitions phase");
        let child = OwnedScope::new(ann, Some(*self));
        self.children.alloc(child).into()
    }

    /// Iterate child scopes
    pub fn children(&self) -> impl Iterator<Item=Scope<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::children can only be called after the Definitions phase"
        );
        self.children.iter().map(|child| child.into())
    }

    /// Add a value definition
    pub fn add_value_def(&self, def: OwnedValueDef<'tree>) -> ValueDef<'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Definitions,
            "Scope::add_value_def can only be called in the Definitions phase"
        );
        self.value_defs.alloc(def).into()
    }

    /// Add a type definition
    pub fn add_type_def(&self, def: OwnedTypeDef<'tree>) -> TypeDef<'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Definitions,
            "Scope::add_value_def can only be called in the Definitions phase"
        );
        self.type_defs.alloc(def).into()
    }

    /// Add an exit
    pub fn add_exit(&self, exit: ScopeExit<'tree>) {
        assert_eq!(
            self.phase(),
            SemanticPhase::Expressions,
            "Scope::add_exit can only be called in the Expressions phase"
        );
        if let Some(other_exit) = self.exit.get() {
            let (old_exit, new_exit, exit_is_new) = match other_exit.cmp_ann(&exit) {
                Ordering::Less => (other_exit, exit, true),
                Ordering::Greater => (exit, other_exit, false),
                Ordering::Equal => {
                    debug!("redundant add_exit call" @ new_exit;
                        note!("old exit" @ old_exit));
                    return
                }
            };
            warning!("dead code, scope has multiple exits" @ new_exit;
                issue!("old exit" @ old_exit));

            if !exit_is_new {
                return;
            }
        };
        self.exit.set(Some(exit))
    }

    /// Get the value definition with the given name
    pub fn get_local_value_def(&self, name: &'tree ValueName) -> Option<ValueDef<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::get_value_def can only be called after the Definitions phase"
        );
        self.value_defs.get(name).map(|def| def.into())
    }

    /// Get the type definition with the given name
    pub fn get_local_type_def(&self, name: &'tree TypeName) -> Option<TypeDef<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::get_type_def can only be called after the Definitions phase"
        );
        self.type_defs.get(name).map(|def| def.into())
    }

    /// Iterate local value definitions
    pub fn local_value_defs(&self) -> impl Iterator<Item=ValueDef<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::value_defs can only be called after the Definitions phase"
        );
        self.value_defs.iter().map(|def| def.into())
    }

    /// Iterate local type definitions
    pub fn local_type_defs(&self) -> impl Iterator<Item=TypeDef<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::type_defs can only be called after the Definitions phase"
        );
        self.type_defs.iter().map(|def| def.into())
    }

    /// Get the current exit
    pub fn exit(&self) -> Option<ScopeExit<'tree>> {
        assert!(
            self.phase() > SemanticPhase::Expressions,
            "Scope::exit can only be called after the Expressions phase"
        );
        self.exit.get()
    }

    /// Get the visible (local or ancestor) value definition with the given name
    pub fn get_visible_value_def(&self, name: &'tree ValueName) -> Option<ValueDef<'tree>> {
        self.get_local_value_def(name).or_else(|| {
            self.parent().and_then(|parent| parent.get_visible_value_def(name))
        })
    }

    /// Get the visible (local or ancestor) type definition with the given name
    pub fn get_visible_type_def(&self, name: &'tree TypeName) -> Option<TypeDef<'tree>> {
        self.get_local_type_def(name).or_else(|| {
            self.parent().and_then(|parent| parent.get_visible_type_def(name))
        })
    }
}
