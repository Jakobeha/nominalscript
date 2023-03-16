use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::{once, zip};
use std::rc::Rc;
use smallvec::SmallVec;
use crate::analyses::scopes::{Scope, ScopePtr};
use crate::analyses::types::DeterminedReturnType;
use crate::ast::tree_sitter::{TSCursor, TSNode};

/// The hierarchy of scopes within a file or module (including submodules)
pub struct ModuleScopes<'tree>(RefCell<_ModuleScopes<'tree>>);

struct _ModuleScopes<'tree> {
    scopes: HashMap<TSNode<'tree>, ScopePtr<'tree>>,
    lexical_parents: HashMap<TSNode<'tree>, TSNode<'tree>>,
    lexical_children: HashMap<TSNode<'tree>, Vec<TSNode<'tree>>>
}

struct SeenLexicalDescendantsOf<'a, 'tree: 'a> {
    module_scopes: &'a ModuleScopes<'tree>,
    node: TSNode<'tree>,
    index: usize,
    stack: Vec<TSNode<'tree>>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeParentType {
    Lexical,
    NonLexical
}

pub struct ScopeAncestorsAndTypesOf<'a> {
    c: &'a mut TSCursor<'_>
}

pub struct ScopeAncestorsOf<'a> {
    c: &'a mut TSCursor<'_>
}

impl<'tree> ModuleScopes<'tree> {
    pub(super) fn new() -> ModuleScopes<'tree> {
        ModuleScopes(RefCell::new(_ModuleScopes {
            scopes: HashMap::new(),
            lexical_parents: HashMap::new(),
            lexical_children: HashMap::new()
        }))
    }

    /// Gets the parent scope containing the node
    ///
    /// *Panics* if the node is the tree root
    pub fn of_node(&self, node: TSNode<'tree>, c: &mut TSCursor<'_>) -> ScopePtr<'tree> {
        let parent = scope_parent_of(node, c).expect("node is root so does not have a parent");
        self.get(parent, c)
    }

    /// Gets the scope denoted by the node (which should be a statement block, etc.)
    pub fn get(&self, node: TSNode<'tree>, c: &mut TSCursor<'_>) -> ScopePtr<'tree> {
        let mut this = self.0.borrow_mut();
        if let Some(scope) = this.scopes.get(&node) {
            return scope.clone()
        }
        // Truly awful pre-optimized code: what it does is add missing scopes for this node and
        // any parents which are also missing scopes, as well as lexical relations.
        let mut ancestors = scope_ancestors_and_types_of(node, c);
        let mut descendants = SmallVec::<[TSNode<'tree>; 4]>::new();
        let mut node_or_parent = node;
        while !this.scopes.contains_key(&node_or_parent) {
            if let Some((parent, is_parent_lexical)) = ancestors.next() {
                if is_parent_lexical {
                    this.lexical_parents.insert(node_or_parent, parent);
                    this.lexical_children.entry(parent).or_default().push(node_or_parent);
                }
                descendants.push(node_or_parent);
                node_or_parent = parent;
            } else {
                break
            }
        }
        let mut scope = this.scopes.get(&node_or_parent);
        for child in descendants {
            let child_scope = ScopePtr::new(scope);
            scope = None;
            let std::collections::hash_map::Entry::Vacant(child_entry) = this.scopes.entry(child) else {
                unreachable!("we just checked that it's not in the map")
            };
            scope = Some(child_entry.insert(child_scope));
        }
        scope.unwrap().clone()
    }

    pub fn seen_lexical_descendants_of(&self, node: TSNode<'tree>) -> impl Iterator<Item=TSNode<'tree>> {
        SeenLexicalDescendantsOf::new(self, node)
    }

    pub fn seen_return_types(&self, node: TSNode<'tree>) -> impl Iterator<Item=DeterminedReturnType<'tree>> {
        once(node).chain(seen_lexical_descendants_of(node))
            .map(|node| self.0.borrow().scopes[&node].return_type())
    }
}

impl<'a, 'tree: 'a> SeenLexicalDescendantsOf<'a, 'tree> {
    fn new(module_scopes: &'a ModuleScopes<'tree>, node: TSNode<'tree>) -> SeenLexicalDescendantsOf<'a, 'tree> {
        SeenLexicalDescendantsOf {
            module_scopes,
            node,
            index: 0,
            stack: vec![]
        }
    }
}

impl<'a, 'tree: 'a> Iterator for SeenLexicalDescendantsOf<'a, 'tree> {
    type Item = TSNode<'tree>;

    fn next(&mut self) -> Option<TSNode<'tree>> {
        let module_scopes = self.module_scopes.0.borrow();
        let children = module_scopes.lexical_children.get(&self.node);
        if let Some(&child) = children.and_then(|children| children.get(self.index)) {
            self.index += 1;
            self.stack.push(child);
            Some(child)
        } else if let Some(node) = self.stack.pop() {
            self.node = node;
            self.index = 0;
            self.next()
        } else {
            None
        }
    }
}

/*  get (node: TSNode): Scope {
    if (!this.scopes.has(node.id)) {
      const scope = new Scope(node)
      this.scopes.set(node.id, scope)
      const lexicalParent = lexicalScopeParentOf(node)
      if (lexicalParent !== null) {
        this.lexicalParents.set(scope, this.get(lexicalParent))
        this.seenLexicalChildrenOf(scope).push(scope)
      }
    }
    return this.scopes.get(node.id)!
  }

  /** Note: we won't return children we haven't otherwise seen yet */
  private seenLexicalChildrenOf (scope: Scope): Scope[] {
    if (!this.lexicalChildren.has(scope)) {
      this.lexicalChildren.set(scope, [])
    }
    return this.lexicalChildren.get(scope)!
  }

  * seenLexicalDescendantsOf (scope: Scope): Generator<Scope> {
    for (const child of this.seenLexicalChildrenOf(scope)) {
      yield child
      yield * this.seenLexicalDescendantsOf(child)
    }
  }

  * seenReturnTypes (scope: Scope, typedExprs: TypedExprs): Generator<NominalReturnTypeInferred> {
    yield scope.returnType(typedExprs)
    for (const descendant of this.seenLexicalDescendantsOf(scope)) {
      yield descendant.returnType(typedExprs)
    }
  }
}
*/

impl<'a> Iterator for ScopeAncestorsAndTypesOf<'a> {
    type Item = (TSNode<'a>, ScopeParentType);

    fn next(&mut self) -> Option<Self::Item> {
        let mut type_ = ScopeParentType::Lexical;
        while self.c.goto_parent() {
            if type_ == ScopeParentType::Lexical && is_at_lexical_border(c) {
                type_ = ScopeParentType::NonLexical;
            }
            if is_at_scope(&mut self.c) {
                return Some((self.c.node(), type_))
            }
        }
        None
    }
}

impl<'a> Iterator for ScopeAncestorsOf<'a> {
    type Item = TSNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.c.goto_parent() {
            if is_at_scope(&mut self.c) {
                return Some(self.c.node())
            }
        }
        None
    }
}

pub fn scope_parent_and_type(node: TSNode<'_>, c: &mut TSCursor<'_>) -> Option<(TSNode<'_>, ScopeParentType)> {
    let mut type_ = ScopeParentType::Lexical;
    c.goto(node);
    while c.goto_parent() {
        if type_ == ScopeParentType::Lexical && is_at_lexical_border(c) {
            type_ = ScopeParentType::NonLexical;
        }
        if is_at_scope(c) {
            return Some((c.node(), type_))
        }
    }
    None
}

pub fn scope_ancestors_and_types_of<'a>(node: TSNode<'_>, c: &'a mut TSCursor<'_>) -> ScopeAncestorsAndTypesOf<'a> {
    c.goto(node);
    ScopeAncestorsAndTypesOf { c }
}

pub fn scope_ancestors_of<'a>(node: TSNode<'_>, c: &'a mut TSCursor<'_>) -> ScopeAncestorsOf<'a> {
    c.goto(node);
    ScopeAncestorsOf { c }
}

pub fn scope_parent_of(node: TSNode<'_>, c: &mut TSCursor<'_>) -> Option<TSNode<'_>> {
    c.goto(node);
    while c.goto_parent() {
        if is_at_scope(c) {
            return Some(c.node())
        }
    }
    None
}

pub fn lexical_scope_parent_of(node: TSNode<'_>, c: &mut TSCursor<'_>) -> Option<TSNode<'_>> {
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