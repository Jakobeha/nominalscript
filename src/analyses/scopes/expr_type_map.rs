use std::cell::RefCell;
use std::collections::HashMap;
use crate::analyses::bindings::LocalValueBinding;
use crate::analyses::types::InferredType;
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::{error, FileLogger};

/// Keeps track of an expression's required type (from context) and assigned type (actually inferred).
pub struct ExprTypeMap<'tree>(RefCell<_ExprTypeMap<'tree>>);

struct _ExprTypeMap<'tree> {
    required_types: HashMap<TSNode<'tree>, InferredType<'tree>>,
    runtime_required_types: HashMap<TSNode<'tree>, InferredType<'tree>>,
    assigned_types: HashMap<TSNode<'tree>, InferredType<'tree>>,
}

impl<'tree> ExprTypeMap<'tree> {
    pub(super) fn new() -> Self {
        Self(RefCell::new(_ExprTypeMap {
            required_types: HashMap::new(),
            runtime_required_types: HashMap::new(),
            assigned_types: HashMap::new()
        }))
    }

    pub fn require(&self, node: TSNode<'tree>, type_: InferredType<'tree>) {
        let mut this = self.0.borrow_mut();
        assert!(!this.required_types.contains_key(&node), "node already assigned a required type");
        assert!(!this.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type, can't also be assigned a required type");
        this.required_types.insert(node, type_);
    }

    pub fn runtime_require(&self, node: TSNode<'tree>, type_: InferredType<'tree>) {
        let mut this = self.0.borrow_mut();
        assert!(!this.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type");
        assert!(!this.required_types.contains_key(&node), "node already assigned a required type, can't also be assigned a runtime-required type");
        this.runtime_required_types.insert(node, type_);
    }

    pub fn assign(&self, node: TSNode<'tree>, type_: InferredType<'tree>) {
        let mut this = self.0.borrow_mut();
        assert!(!this.assigned_types.contains_key(&node), "node already assigned a type");
        this.assigned_types.insert(node, type_);
    }

    /// Lookup assigned type
    pub fn get(&self, node: TSNode<'tree>) -> Option<InferredType<'tree>> {
        let mut this = self.0.borrow();
        this.assigned_types.get(&node).cloned()
    }

    /// Infer the binding's type from uses' *required* types
    pub fn backwards_infer(&mut self, binding: impl LocalValueBinding<'tree>) -> Option<&InferredType<'tree>> {
        let mut this = self.0.borrow();
        binding.local_uses().iter().find_map(|use_| {
            assert!(
                !this.assigned_types.contains_key(&use_),
                "use already assigned a type (didn't expect, can probably just remove the assertion and just call this.assignedTypes.get(use.id)"
            );
            this.required_types.get(&use_)
        })
    }

    pub fn check_all(&self, e: &mut FileLogger<'_>) {
        let mut this = self.0.borrow();
        for (node, required_type) in &this.required_types {
            let assigned_type = this.assigned_types.get(node);
            InferredType::check_assignable_to(assigned_type, Some(required_type), node, false, e);
        }
        for (node, required_type) in &this.runtime_required_types {
            let assigned_type = this.assigned_types.get(node);
            InferredType::check_assignable_to(assigned_type, Some(required_type), node, true, e);
        }
    }
}