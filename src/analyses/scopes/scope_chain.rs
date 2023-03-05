use crate::analyses::scopes::{ModuleCtx, Scope};
use crate::ast::tree_sitter::TSNode;

pub struct ScopeChain<'a, 'tree> {
    ctx: &'a ModuleCtx<'tree>,
    scopes: Vec<(TSNode<'tree>, Scope<'tree>)>,
}

impl<'a, 'tree> ScopeChain {
    pub fn new(ctx: &'a ModuleCtx<'tree>, scope_parent: TSNode<'tree>) -> Self {
        let mut scopes = Vec::new();
        let mut scope_parent = scope_parent;
        loop {
            let scope = ctx.scopes.get(scope_parent);
            scopes.push((scope_parent, scope));
            scope_parent = scope_parent_of(scope_parent);
            if scope_parent.is_null() {
                break;
            }
        }
        scopes.reverse();
        Self { ctx, scopes }
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