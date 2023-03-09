use std::cell::RefCell;
use std::collections::HashMap;
use crate::analyses::bindings::LocalValueBinding;
use crate::analyses::types::DeterminedType;
use crate::ast::tree_sitter::TSNode;
use crate::diagnostics::{error, FileLogger};

/// Keeps track of an expression's required type (from context) and assigned type (actually inferred).
pub struct ExprTypeMap<'tree> {
    required_types: HashMap<TSNode<'tree>, DeterminedType<'tree>>,
    runtime_required_types: HashMap<TSNode<'tree>, DeterminedType<'tree>>,
    assigned_types: HashMap<TSNode<'tree>, DeterminedType<'tree>>,
}

impl<'tree> ExprTypeMap<'tree> {
    pub(super) fn new() -> Self {
        Self {
            required_types: HashMap::new(),
            runtime_required_types: HashMap::new(),
            assigned_types: HashMap::new()
        }
    }

    pub fn require(&mut self, node: TSNode<'tree>, type_: DeterminedType<'tree>) {
        assert!(!self.required_types.contains_key(&node), "node already assigned a required type");
        assert!(!self.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type, can't also be assigned a required type");
        self.required_types.insert(node, type_);
    }

    pub fn runtime_require(&mut self, node: TSNode<'tree>, type_: DeterminedType<'tree>) {
        assert!(!self.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type");
        assert!(!self.required_types.contains_key(&node), "node already assigned a required type, can't also be assigned a runtime-required type");
        self.runtime_required_types.insert(node, type_);
    }

    pub fn assign(&mut self, node: TSNode<'tree>, type_: DeterminedType<'tree>) {
        assert!(!self.assigned_types.contains_key(&node), "node already assigned a type");
        self.assigned_types.insert(node, type_);
    }

    /// Lookup assigned type
    pub fn get(&self, node: TSNode<'tree>) -> Option<&DeterminedType<'tree>> {
        self.assigned_types.get(&node)
    }

    /// Infer the binding's type from uses' *required* types
    pub fn backwards_infer(&mut self, binding: &impl LocalValueBinding<'tree>) -> Option<&DeterminedType<'tree>> {
        binding.local_uses().iter().find_map(|use_| {
            assert!(
                !self.assigned_types.contains_key(&use_),
                "use already assigned a type (didn't expect, can probably just remove the assertion and just call this.assignedTypes.get(use.id)"
            );
            self.required_types.get(&use_)
        })
    }

    pub fn check_all(&self, e: &mut FileLogger<'_>) {
        for (node, required_type) in &self.required_types {
            let assigned_type = self.assigned_types.get(node);
            DeterminedType::check_subtype(assigned_type, Some(required_type), node, e);
        }
        for (node, required_type) in &self.runtime_required_types {
            let assigned_type = self.assigned_types.get(node);
            DeterminedType::check_not_disjoint(assigned_type, Some(required_type), node, e);
            // TODO insert guard if assigned_type is not a subtype of required_type
        }
    }
}