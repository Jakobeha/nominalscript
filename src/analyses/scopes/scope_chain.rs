use std::rc::Rc;
use crate::analyses::scopes::{ModuleCtx, Scope, scope_parent_of};
use crate::ast::tree_sitter::{TSCursor, TSNode};
use crate::ast::typed_nodes::AstValueDecl;

pub struct ScopeChain<'a, 'tree> {
    ctx: &'a ModuleCtx<'tree>,
    scopes: Vec<(TSNode<'tree>, Rc<Scope<'tree>>)>,
}

impl<'a, 'tree> ScopeChain {
    //noinspection RsUnreachableCode (this inspection is bugged for IntelliJ with let-else statements)
    pub fn new(ctx: &'a ModuleCtx<'tree>, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) -> Self {
        let mut scopes = Vec::new();
        let mut scope_parent = scope_parent;
        loop {
            let scope = ctx.scopes.get(scope_parent, c);
            scopes.push((scope_parent, scope));
            let Some(next_scope_parent) = scope_parent_of(scope_parent, c) else {
                break
            };
            scope_parent = next_scope_parent;
        }
        scopes.reverse();
        Self { ctx, scopes }
    }

    pub fn push(&mut self, scope_parent: TSNode<'tree>, c: &mut TSCursor<'tree>) {
        let scope = ctx.scopes.get(scope_parent, c);
        self.scopes.push((scope_parent, scope));
    }

    pub fn pop(&mut self) -> Option<(TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.pop()
    }

    pub fn top(&self) -> Option<&(TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.last()
    }

    pub fn top_mut(&mut self) -> Option<&mut (TSNode<'tree>, Rc<Scope<'tree>>)> {
        self.scopes.last_mut()
    }

    pub fn add_sequential(&mut self, decl: AstValueDecl<'tree>, c: &mut TSCursor<'tree>) {
        self.top_mut().expect("ScopeChain is empty").1.add_sequential(decl, c);
    }
}

/*
export class ScopeChain {
  private readonly scopeParents: TSNode[] = []
  private readonly scopes: Scope[] = []
  private topScopeNode: TSNode | null = null
  private topScope: Scope | null = null

  constructor (private readonly ctx: ModuleCtx, scopeParent: TSNode | null = null) {
    if (scopeParent !== null) {
      do {
        this.scopeParents.push(scopeParent)
        this.scopes.push(ctx.scopes.get(scopeParent))
        scopeParent = scopeParentOf(scopeParent)
      } while (scopeParent !== null)
      this.topScopeNode = this.scopeParents[0] ?? null
      this.topScope = this.scopes[0] ?? null
      this.scopeParents.reverse()
      this.scopes.reverse()
    }
  }

  push (scopeParent: TSNode): void {
    const scope = this.ctx.scopes.get(scopeParent)
    this.scopeParents.push(scopeParent)
    this.scopes.push(scope)
    this.topScopeNode = scopeParent
    this.topScope = scope
  }

  pop (): TSNode | null {
    const popped = this.scopeParents.pop() ?? null
    this.scopes.pop()
    this.topScopeNode = this.scopeParents[this.scopeParents.length - 1] ?? null
    this.topScope = this.scopes[this.scopes.length - 1] ?? null
    return popped
  }

  addSequential (decl: ValueDecl): void {
    assert(this.topScope !== null, 'cannot add sequential to empty scope chain')
    this.topScope.addSequential(decl)
  }

  getHoistedInTopScope (name: string): HoistedAstBinding | null {
    assert(this.topScope !== null, 'cannot getHoistedInTopScope from empty scope chain')
    return this.topScope.getHoisted(name)
  }

  get (name: string, posNode: TSNode): Binding | null {
    for (const scope of this.topToBottom()) {
      const found = scope.get(name, posNode)
      if (found !== null) {
        return found
      }
    }
    return GlobalBinding.tryGet(name) ?? null
  }

  getExactSequential (name: string, posNode: TSNode): ValueDecl {
    assert(this.topScope !== null, 'cannot getExactSequential from empty scope chain')
    return this.topScope.getExactSequential(name, posNode)
  }

  addReturn (stmt: TSNode, value: TSNode | null): void {
    assert(this.topScope !== null && this.topScopeNode !== null, 'cannot addReturn in empty scope chain')
    if (this.topScope.returnStmt !== null) {
      logError(
        'cannot return/throw multiple times in the same scope', this.topScopeNode,
        ['note: first return', this.topScope.returnStmt],
        ['note: second return', stmt]
      )
    }
    if (this.topScope.throwStmt !== null) {
      logError(
        'cannot return/throw multiple times in the same scope', this.topScopeNode,
        ['note: first throw', this.topScope.throwStmt],
        ['note: second return', stmt]
      )
    }
    this.topScope.returnStmt = stmt
    this.topScope.returnValue = value
  }

  addThrow (stmt: TSNode, value: TSNode | null): void {
    assert(this.topScope !== null && this.topScopeNode !== null, 'cannot addThrow in empty scope chain')
    if (this.topScope.returnStmt !== null) {
      logError(
        'cannot return/throw multiple times in the same scope', this.topScopeNode,
        ['note: first return', this.topScope.returnStmt],
        ['note: second throw', stmt]
      )
    }
    if (this.topScope.throwStmt !== null) {
      logError(
        'cannot return/throw multiple times in the same scope', this.topScopeNode,
        ['note: first throw', this.topScope.throwStmt],
        ['note: second throw', stmt]
      )
    }
    this.topScope.returnStmt = stmt
    this.topScope.throwValue = value
  }

  inferType (node: TSNode): NominalTypeInferred | null {
    return this.ctx.typedExprs.get(node)
  }

  // This may be inefficient, but there's no builtin on array to do reverse iteration
  // and I don't want to have to write this every time
  private * topToBottom (): Generator<Scope> {
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      yield this.scopes[i]
    }
  }
}

 */