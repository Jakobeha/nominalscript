use std::collections::HashMap;

use crate::analyses::types::{DeterminedType, ResolveCtx};
use crate::concrete::tree_sitter::TSNode;
use crate::diagnostics::FileLogger;

/// Keeps track of an expression's required type (from context) and assigned type (actually inferred).
#[derive(Debug)]
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

    pub fn require(&mut self, node: TSNode<'tree>, type_: impl Into<DeterminedType<'tree>>) {
        let type_ = type_.into();
        assert!(!self.required_types.contains_key(&node), "node already assigned a required type");
        assert!(!self.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type, can't also be assigned a required type");
        self.required_types.insert(node, type_);
    }

    pub fn runtime_require(&mut self, node: TSNode<'tree>, type_: impl Into<DeterminedType<'tree>>) {
        let type_ = type_.into();
        assert!(!self.runtime_required_types.contains_key(&node), "node already assigned a runtime-required type");
        assert!(!self.required_types.contains_key(&node), "node already assigned a required type, can't also be assigned a runtime-required type");
        self.runtime_required_types.insert(node, type_);
    }

    pub fn assign(&mut self, node: TSNode<'tree>, type_: impl Into<DeterminedType<'tree>>) {
        let type_ = type_.into();
        assert!(!self.assigned_types.contains_key(&node), "node already assigned a type");
        self.assigned_types.insert(node, type_);
    }

    /// Lookup assigned type
    pub fn get(&self, node: TSNode<'tree>) -> Option<&DeterminedType<'tree>> {
        self.assigned_types.get(&node)
    }

    pub fn check_all(&mut self, e: &FileLogger<'_>, ctx: &ResolveCtx<'_>) {
        for (node, required_type) in self.required_types.drain() {
            // Already checking disjoint so no need to check subtype
            let _runtime_required_type = self.runtime_required_types.remove(&node);
            let assigned_type = self.assigned_types.remove(&node);
            DeterminedType::check_subtype(assigned_type, Some(required_type), node, e, ctx);
        }
        for (node, required_type) in self.runtime_required_types.drain() {
            let assigned_type = self.assigned_types.remove(&node);
            DeterminedType::check_not_disjoint(assigned_type, Some(required_type), node, e, ctx);
            // TODO insert guard if assigned_type is not a subtype of required_type
        }
    }
}