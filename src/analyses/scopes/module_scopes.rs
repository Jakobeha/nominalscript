use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::once;
use std::rc::Rc;
use crate::analyses::scopes::Scope;
use crate::analyses::types::DeterminedReturnType;
use crate::ast::tree_sitter::{TSCursor, TSNode};

/// The hierarchy of scopes within a file or module (including submodules)
pub struct ModuleScopes<'tree>(RefCell<_ModuleScopes<'tree>>);

struct _ModuleScopes<'tree> {
    scopes: HashMap<TSNode<'tree>, Rc<Scope<'tree>>>,
    lexical_parents: HashMap<TSNode<'tree>, TSNode<'tree>>,
    lexical_children: HashMap<TSNode<'tree>, Vec<TSNode<'tree>>>
}

pub fn scope_parent_of(node: TSNode<'_>, c: &mut TSCursor<'_>) -> Option<TSNode<'_>> {
    // TODO
}

pub fn lexical_scope_parent_of(node: TSNode<'_>, c: &mut TSCursor<'_>) -> Option<TSNode<'_>> {
    // TODO
}

impl<'tree> ModuleScopes<'tree> {
    pub(super) fn new() -> ModuleScopes<'tree> {
        ModuleScopes(RefCell::new(_ModuleScopes {
            scopes: HashMap::new(),
            lexical_parents: HashMap::new(),
            lexical_children: HashMap::new()
        }))
    }

    pub fn get(&self, node: TSNode<'tree>, c: &mut TSCursor<'_>) -> Rc<Scope<'tree>> {
        if !self.0.borrow().scopes.contains_key(&node) {
            let mut this = self.0.borrow_mut();
            this.scopes.insert(node, Rc::new(Scope::new()));
            if let Some(lexical_parent) = lexical_scope_parent_of(node, c) {
                this.lexical_parents.insert(node, lexical_parent);
                this.lexical_children.entry(lexical_parent).or_default().push(node);
            }
        }
        self.0.borrow().scopes.get(&node).unwrap().clone()
    }

    pub fn seen_lexical_descendants_of(&self, node: TSNode<'tree>) -> impl Iterator<Item=TSNode<'tree>> {
        SeenLexicalDescendantsOf::new(self, node)
    }

    pub fn seen_return_types(&self, node: TSNode<'tree>) -> impl Iterator<Item=DeterminedReturnType<'tree>> {
        once(node).chain(seen_lexical_descendants_of(node))
            .map(|node| self.0.borrow().scopes[&node].return_type())
    }
}

struct SeenLexicalDescendantsOf<'a, 'tree: 'a> {
    module_scopes: &'a ModuleScopes<'tree>,
    node: TSNode<'tree>,
    index: usize,
    stack: Vec<TSNode<'tree>>
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