use std::collections::HashMap;
use std::iter::once;

use smallvec::SmallVec;
use crate::analyses::scopes::ExprTypeMap;

use crate::analyses::scopes::scope_ptr::ScopePtr;
use crate::analyses::types::DeterminedReturnType;
use crate::ast::tree_sitter::{TSCursor, TSNode};

/// The hierarchy of scopes within a file or module (including submodules)
#[derive(Debug)]
pub struct ModuleScopes<'tree> {
    root_node: TSNode<'tree>,
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
        this.scopes.insert(root_node, ScopePtr::new(None));
        this
    }

    pub fn root(&self) -> &ScopePtr<'tree> {
        &self.scopes[&self.root_node]
    }

    /// Gets the parent scope containing the node
    ///
    /// *Panics* if the node is the tree root
    pub fn of_node(&mut self, node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> &mut ScopePtr<'tree> {
        let parent = scope_parent_of(node, c).expect("node is root so does not have a parent");
        self.get(parent, c)
    }

    /// Gets the scope denoted by the node (which should be a statement block, etc.)
    pub fn get(&mut self, node: TSNode<'tree>, c: &mut TSCursor<'tree>) -> &mut ScopePtr<'tree> {
        /*if let Some(scope) = self.scopes.get_mut(&node) {
            return scope
        }*/
        // Truly awful pre-optimized code: what it does is add missing scopes for this node and
        // any parents which are also missing scopes, as well as lexical relations.
        let mut ancestors = scope_ancestors_and_types_of(node, c);
        let mut descendants = SmallVec::<[TSNode<'tree>; 4]>::new();
        let mut node_or_parent = node;
        while !self.scopes.contains_key(&node_or_parent) {
            if let Some((parent, parent_type)) = ancestors.next() {
                if matches!(parent_type, ScopeParentType::Lexical) {
                    self.lexical_parents.insert(node_or_parent, parent);
                    self.lexical_children.entry(parent).or_default().push(node_or_parent);
                }
                descendants.push(node_or_parent);
                node_or_parent = parent;
            } else {
                break
            }
        }
        // This could be done with just references if Rust's borrow checker was smarter / more
        // granular, because we set scope to None while doing the other borrow (see below)
        let mut scope = self.scopes.get_mut(&node_or_parent).map(|x| x as *mut _);
        for child in descendants {
            // SAFETY: See other comments. This reference is alive and not borrowed anywhere else
            let child_scope = ScopePtr::new(scope.map(|x| unsafe { &*x }));
            scope = None;
            // Other borrow here, notice we had to set scope to None to not double-borrow
            let std::collections::hash_map::Entry::Vacant(child_entry) = self.scopes.entry(child) else {
                unreachable!("we just checked that it's not in the map")
            };
            scope = Some(child_entry.insert(child_scope) as *mut _);
        }
        // SAFETY: See other comments. This reference is alive and not borrowed anywhere else
        unsafe { &mut *scope.unwrap() }
    }

    pub fn seen_lexical_descendants_of(&self, node: TSNode<'tree>) -> impl Iterator<Item=TSNode<'tree>> + '_ {
        SeenLexicalDescendantsOf::new(self, node)
    }

    pub fn seen_return_types<'a>(&'a self, node: TSNode<'tree>, typed_exprs: &'a ExprTypeMap<'tree>) -> impl Iterator<Item=DeterminedReturnType<'tree>> + 'a {
        once(node).chain(self.seen_lexical_descendants_of(node))
            .map(|node| self.scopes[&node].values.return_type(typed_exprs))
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
        let module_scopes = self.module_scopes;
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