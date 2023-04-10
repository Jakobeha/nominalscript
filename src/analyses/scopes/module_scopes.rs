use std::collections::HashMap;
use std::iter::once;
use std::ptr::null;

use smallvec::SmallVec;
use crate::analyses::scopes::{ExprTypeMap, InactiveScopePtr};

use crate::analyses::types::DeterminedReturnType;
use crate::ast::tree_sitter::{TSCursor, TSNode};

/// The hierarchy of scopes within a file or module (including submodules)
#[derive(Debug)]
pub struct ModuleScopes<'tree> {
    root_node: TSNode<'tree>,
    scopes: HashMap<TSNode<'tree>, InactiveScopePtr<'tree>>,
    lexical_parents: HashMap<InactiveScopePtr<'tree>, InactiveScopePtr<'tree>>,
    lexical_children: HashMap<InactiveScopePtr<'tree>, Vec<InactiveScopePtr<'tree>>>
}

struct SeenLexicalDescendantsOf<'a, 'tree: 'a> {
    module_scopes: &'a ModuleScopes<'tree>,
    scope: &'a InactiveScopePtr<'tree>,
    index: usize,
    stack: Vec<&'a InactiveScopePtr<'tree>>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeParentType {
    Lexical,
    NonLexical
}

pub struct ScopeAncestorsAndTypesOf<'a, 'tree> {
    c: &'a mut TSCursor<'tree>
}

pub struct ScopeAncestorsOf<'a, 'tree> {
    c: &'a mut TSCursor<'tree>
}

impl<'tree> ModuleScopes<'tree> {
    pub(super) fn new(root_node: TSNode<'tree>) -> ModuleScopes<'tree> {
        let mut this = ModuleScopes {
            root_node,
            scopes: HashMap::new(),
            lexical_parents: HashMap::new(),
            lexical_children: HashMap::new()
        };
        this.scopes.insert(root_node, InactiveScopePtr::new(None));
        this
    }

    pub fn root(&self) -> &InactiveScopePtr<'tree> {
        &self.scopes[&self.root_node]
    }

    /// Gets the parent scope containing the node
    ///
    /// *Panics* if the node is the tree root
    pub fn containing(&mut self, node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> &InactiveScopePtr<'tree> {
        let parent = scope_parent_of(node, c).expect("node is root so does not have a parent");
        self._denoted_by(parent, c)
    }

    /// Gets the scope denoted by the node (which should be a statement block, etc.)
    pub fn denoted_by(&mut self, node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Option<&InactiveScopePtr<'tree>> {
        match is_scope_node(node, c) {
            false => None,
            true => Some(self._denoted_by(node, c))
        }
    }

    /// [Self::denoted_by] but simply assumes we are at the scope
    fn _denoted_by(&mut self, node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> &InactiveScopePtr<'tree> {
        debug_assert!(is_scope_node(node, c));
        if self.scopes.contains_key(&node) {
            return &self.scopes[&node]
        }
        // Truly awful pre-optimized code: what it does is add missing scopes for this node and
        // any parents which are also missing scopes, as well as lexical relations.
        let mut ancestors = scope_ancestors_and_types_of(node, c);
        let mut descendants = SmallVec::<[(TSNode<'tree>, ScopeParentType); 1]>::new();
        let mut node_or_parent = node;
        loop {
            let Some((parent, parent_type)) = ancestors.next() else {
                panic!("node is not in the tree (doesn't have root node as an ancestor)")
            };
            descendants.push((node_or_parent, parent_type));
            node_or_parent = parent;
            if self.scopes.contains_key(&node_or_parent) { break }
        }
        // We need this to be a raw pointer so we can call self.scopes.entry
        let mut scope = &self.scopes[&node_or_parent] as *const _;
        for (child, child_type) in descendants {
            // SAFETY: See other comments. This reference is alive and not borrowed anywhere else
            let child_scope = {
                let scope = unsafe { &*scope };
                let child_scope = InactiveScopePtr::new(Some(scope));
                if matches!(child_type, ScopeParentType::Lexical) {
                    self.lexical_parents.insert(child_scope.clone(), scope.clone());
                    self.lexical_children.entry(scope.clone()).or_default().push(child_scope.clone());
                }
                child_scope
            };
            // We have to set this before calling self.scopes.entry so that we don't have a pointer
            // inside of self.scopes when it's mutably borrowed
            #[allow(unused_assignments)]
            { scope = null(); }
            let std::collections::hash_map::Entry::Vacant(child_entry) = self.scopes.entry(child) else {
                unreachable!("we just checked that it's not in the map")
            };
            scope = child_entry.insert(child_scope) as *mut _ as *const _;
        }
        // SAFETY: See other comments. This reference is alive and not borrowed anywhere else
        unsafe { &*scope }
    }

    /// Iterate through this scope and all its lexical descendants *which have been created* i.e. "seen"
    pub fn seen_lexical_descendants_of<'a>(&'a self, scope: &'a InactiveScopePtr<'tree>) -> impl Iterator<Item=&'a InactiveScopePtr<'tree>> + 'a {
        SeenLexicalDescendantsOf::new(self, scope)
    }

    /// Iterate through this scope and all its lexical descendants *which have been created* i.e. "seen"
    pub fn seen_return_types_of<'a>(&'a self, scope: &'a InactiveScopePtr<'tree>, typed_exprs: &'a ExprTypeMap<'tree>) -> impl Iterator<Item=DeterminedReturnType<'tree>> + 'a {
        once(scope).chain(self.seen_lexical_descendants_of(scope))
            .map(|scope| scope.activate_ref().values.return_type(typed_exprs))
    }
}

impl<'a, 'tree: 'a> SeenLexicalDescendantsOf<'a, 'tree> {
    fn new(module_scopes: &'a ModuleScopes<'tree>, scope: &'a InactiveScopePtr<'tree>) -> SeenLexicalDescendantsOf<'a, 'tree> {
        SeenLexicalDescendantsOf {
            module_scopes,
            scope,
            index: 0,
            stack: vec![]
        }
    }
}

impl<'a, 'tree: 'a> Iterator for SeenLexicalDescendantsOf<'a, 'tree> {
    type Item = &'a InactiveScopePtr<'tree>;

    fn next(&mut self) -> Option<&'a InactiveScopePtr<'tree>> {
        let module_scopes = self.module_scopes;
        let children = module_scopes.lexical_children.get(self.scope);
        if let Some(child) = children.and_then(|children| children.get(self.index)) {
            self.index += 1;
            self.stack.push(child);
            Some(child)
        } else if let Some(scope) = self.stack.pop() {
            self.scope = scope;
            self.index = 0;
            self.next()
        } else {
            None
        }
    }
}

impl<'a, 'tree> Iterator for ScopeAncestorsAndTypesOf<'a, 'tree> {
    type Item = (TSNode<'tree>, ScopeParentType);

    fn next(&mut self) -> Option<Self::Item> {
        let mut type_ = ScopeParentType::Lexical;
        while self.c.goto_parent() {
            if matches!(type_, ScopeParentType::Lexical) && is_at_lexical_border(self.c) {
                type_ = ScopeParentType::NonLexical;
            }
            if is_at_scope(&mut self.c) {
                return Some((self.c.node(), type_))
            }
        }
        None
    }
}

impl<'a, 'tree> Iterator for ScopeAncestorsOf<'a, 'tree> {
    type Item = TSNode<'tree>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.c.goto_parent() {
            if is_at_scope(&mut self.c) {
                return Some(self.c.node())
            }
        }
        None
    }
}

pub fn scope_parent_and_type<'tree>(node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Option<(TSNode<'tree>, ScopeParentType)> {
    let mut type_ = ScopeParentType::Lexical;
    c.goto(node);
    while c.goto_parent() {
        if matches!(type_, ScopeParentType::Lexical) && is_at_lexical_border(c) {
            type_ = ScopeParentType::NonLexical;
        }
        if is_at_scope(c) {
            return Some((c.node(), type_))
        }
    }
    None
}

pub fn scope_ancestors_and_types_of<'a, 'tree>(node: TSNode<'tree>, c: &'a mut TSCursor<'tree>) -> ScopeAncestorsAndTypesOf<'a, 'tree> {
    c.goto(node);
    ScopeAncestorsAndTypesOf { c }
}

pub fn scope_ancestors_of<'a, 'tree>(node: TSNode<'tree>, c: &'a mut TSCursor<'tree>) -> ScopeAncestorsOf<'a, 'tree> {
    c.goto(node);
    ScopeAncestorsOf { c }
}

pub fn scope_parent_of<'tree>(node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Option<TSNode<'tree>> {
    c.goto(node);
    while c.goto_parent() {
        if is_at_scope(c) {
            return Some(c.node())
        }
    }
    None
}

pub fn lexical_scope_parent_of<'tree>(node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Option<TSNode<'tree>> {
    c.goto(node);
    while c.goto_parent() {
        if is_at_lexical_border(c) {
            return None
        }
        if is_at_scope(c) {
            return Some(c.node())
        }
    }
    None
}

fn is_at_lexical_border(c: &TSCursor<'_>) -> bool {
    match c.node().kind() {
        "function_declaration" |
        "generator_function_declaration" |
        "function" |
        "generator_function" |
        "arrow_function" |
        "class_declaration" => true,
        _ => false
    }
}

fn is_scope_node<'tree>(node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> bool {
    c.goto(node);
    is_at_scope(c)
}

fn is_at_scope(c: &TSCursor<'_>) -> bool {
    match c.node().kind() {
        "program" |
        "statement_block" |
        "class_body" => true,
        _ => match c.field_name() {
            Some("body") |
            Some("consequence") => true,
            _ => match c.node().parent() {
                Some(parent) if parent.kind() == "else_clause" => true,
                _ => false
            }
        }
    }
}