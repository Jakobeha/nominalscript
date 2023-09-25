use crate::storage::{Id, InnerSet};

/// Package Scopes
pub struct Scopes<'tree> {
    root: Scope<'tree>,
}

/// e.g. toplevel scope, class scope, function scope
pub type Scope<'tree> = Id<'tree, ScopeData<'tree>>;
#[derive(Debug)]
pub struct ScopeData<'tree> {
    parent: Option<Scope<'tree>>,
    children: InnerSet<'tree, Scope<'tree>>,
}

/*
    /// Immediate value definitions = values defined in this scope, not parent or children
    value_defs: AnnSet<'tree, OwnedValueDef<'tree>>,
    /// Immediate type definitions = types defined in this scope, not parent or children
    type_defs: AnnSet<'tree, OwnedTypeDef<'tree>>,
    #[phase(Expressions)]
    /// Ordered value expressions
    exprs: Vec<Expr<'tree>>,
    /// Types
    // #[phase(SemanticPhase::Expressions)]
    types: Vec<Type<'tree>>,
    /// Scope `return` or `throw` if present
    exit: Option<ScopeExit<'tree>>
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

impl<'cycle, 'tree> OwnedScope<'cycle, 'tree> {
    /// Create a new, empty scope
    fn new(ann: Ann<'tree>, parent: Option<Scope<'cycle, 'tree>>) -> Self {
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
        for child in self.children.iter_mut() {
            child.conv_expressions();
        }
        self.value_defs.conv_retrieval();
        self.type_defs.conv_retrieval();
        self.exprs.conv_insertion();
        self.types.conv_insertion()
    }

    /// Transition to [SemanticPhase::TypeChecking] phase
    pub fn conv_type_checking(&mut self) {
        self.phase = SemanticPhase::TypeChecking;
        for child in self.children.iter_mut() {
            child.conv_type_checking();
        }
        self.exprs.conv_retrieval();
        self.types.conv_retrieval();
    }

    /// Transition to [SemanticPhase::Removal] phase
    pub fn conv_removal(&mut self) {
        self.phase = SemanticPhase::Removal;
        for child in self.children.iter_mut() {
            child.conv_removal();
        }
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
        for child in self.children.iter_mut() {
            child.conv_definitions();
        }
        self.value_defs.conv_insertion();
        self.type_defs.conv_insertion();
        self.exprs.conv_insertion();
        self.types.conv_insertion();
    }
}

impl<'cycle, 'tree> Scope<'cycle, 'tree> {
    /// Get the scope's current phase
    pub fn phase(&self) -> SemanticPhase {
        self.phase
    }

    /// Add a child scope
    pub fn add_child(&self, ann: Ann<'tree>) -> Scope<'cycle, 'tree> {
        assert_eq!(self.phase(), SemanticPhase::Definitions, "Scope::add_child can only be called in the Definitions phase");
        let child = OwnedScope::new(ann, Some(*self));
        self.children.alloc(child).into()
    }

    /// Add a value definition
    pub fn add_value_def(&self, def: OwnedValueDef<'cycle, 'tree>) -> ValueDef<'cycle, 'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Definitions,
            "Scope::add_value_def can only be called in the Definitions phase"
        );
        self.value_defs.alloc(def).into()
    }

    /// Add a type definition
    pub fn add_type_def(&self, def: OwnedTypeDef<'cycle, 'tree>) -> TypeDef<'cycle, 'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Definitions,
            "Scope::add_value_def can only be called in the Definitions phase"
        );
        self.type_defs.alloc(def).into()
    }

    /// Add an expression
    pub fn add_expr(&self, expr: OwnedExpr<'cycle, 'tree>) -> Expr<'cycle, 'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Expressions,
            "Scope::add_expr can only be called in the Expressions phase"
        );
        self.exprs.alloc(expr).into()
    }

    /// Add a type
    pub fn add_type(&self, ty: OwnedType<'cycle, 'tree>) -> Type<'cycle, 'tree> {
        assert_eq!(
            self.phase(),
            SemanticPhase::Expressions,
            "Scope::add_type can only be called in the Expressions phase"
        );
        self.types.alloc(ty).into()
    }

    /// Add an exit
    pub fn add_exit(&self, exit: ScopeExit<'cycle, 'tree>) {
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

    /// Iterate child scopes
    pub fn children(&self) -> impl Iterator<Item=Scope<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::children can only be called after the Definitions phase"
        );
        self.children.iter().map(|child| child.into())
    }

    /// Get the value definition with the given name
    pub fn local_value_def(&self, name: &'tree ValueName) -> Option<ValueDef<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::local_value_def can only be called after the Definitions phase"
        );
        self.value_defs.get(name).map(|def| def.into())
    }

    /// Get the type definition with the given name
    pub fn local_type_def(&self, name: &'tree TypeName) -> Option<TypeDef<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::local_type_def can only be called after the Definitions phase"
        );
        self.type_defs.get(name).map(|def| def.into())
    }

    /// Iterate local value definitions
    pub fn local_value_defs(&self) -> impl Iterator<Item=ValueDef<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::local_value_defs can only be called after the Definitions phase"
        );
        self.value_defs.iter().map(|def| def.into())
    }

    /// Iterate local type definitions
    pub fn local_type_defs(&self) -> impl Iterator<Item=TypeDef<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Definitions,
            "Scope::local_type_defs can only be called after the Definitions phase"
        );
        self.type_defs.iter().map(|def| def.into())
    }

    /// Get the current exit
    pub fn exit(&self) -> Option<ScopeExit<'cycle, 'tree>> {
        assert!(
            self.phase() > SemanticPhase::Expressions,
            "Scope::exit can only be called after the Expressions phase"
        );
        self.exit.get()
    }

    /// Get the visible (local or ancestor) value definition with the given name
    pub fn visible_value_def(&self, name: &'tree ValueName) -> Option<ValueDef<'cycle, 'tree>> {
        self.local_value_def(name).or_else(|| {
            self.parent().and_then(|parent| parent.visible_value_def(name))
        })
    }

    /// Get the visible (local or ancestor) type definition with the given name
    pub fn visible_type_def(&self, name: &'tree TypeName) -> Option<TypeDef<'cycle, 'tree>> {
        self.local_type_def(name).or_else(|| {
            self.parent().and_then(|parent| parent.visible_type_def(name))
        })
    }
}
*/