use std::collections::HashMap;
use crate::analyses::bindings::{TypeName, ValueName};
use crate::ast::tree_sitter::TSNode;

/// A local scope: contains all of the bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.).
///
/// Not all scopes actually have their own types, params, etc.
/// but they are all provided for conformity and because it uses negligible effort and resources.
pub struct Scope<'tree> {
    did_set_params: bool,
    values: ValueScope<'tree>,
    types: TypeScope<'tree>,
}

/// A local scope: contains all of the value bindings in a scope node
/// (top level, module, statement block, class declaration, arrow function, etc.)
pub struct ValueScope<'tree> {
    hoisted: HashMap<ValueName, ValueBinding<'tree>>,
    sequential: HashMap<ValueName, Vec<ValueBinding<'tree>>>,
    return_: Option<Return<'tree>>,
    throw: Option<Throw<'tree>>,
}

pub struct TypeScope<'tree> {
    hoisted: HashMap<TypeName, TypeBinding<'tree>>,
}

impl<'tree> Scope<'tree> {
    pub(super) fn new() -> Scope<'tree> {
        Scope {
            did_set_params: false,
            values: ValueScope::new(),
            types: TypeScope::new(),
        }
    }
}

impl<'tree> ValueScope<'tree> {
    fn new() -> ValueScope<'tree> {
        ValueScope {
            hoisted: HashMap::new(),
            sequential: HashMap::new(),
            return_: None,
            throw: None,
        }
    }
}

impl<'tree> TypeScope<'tree> {
    fn new() -> TypeScope<'tree> {
        TypeScope {
            hoisted: HashMap::new(),
        }
    }
}

/*
export class NominalTypeDeclMap {
  private readonly map: Map<string, NominalTypeDecl | NominalImportSpecifier> = new Map()

  has (name: string): boolean {
    return this.map.has(name)
  }

  get (name: string): NominalTypeDecl | NominalImportSpecifier | null {
    return this.map.get(name) ?? null
  }

  async getShape (name: string, use: TSNode | null): Promise<
  { shape: null, loc: TSNode | null } |
  { shape: NominalTypeDeclShape, loc: TSNode }
  > {
    const decl = this.get(name)
    if (decl === null) {
      return { shape: null, loc: null }
    }
    const shape = unwrapImportResult(await decl.resolveNominal?.get(), decl.node, use)
    return { shape, loc: decl.node }
  }

  set (name: string, decl: NominalTypeDecl | NominalImportSpecifier): void {
    this.map.set(name, decl)
  }

  async * getSupertypes (type: NominalTypeShape, use: TSNode): AsyncGenerator<NominalTypeShape> {
    if (!NominalTypeShape.isIdent(type)) {
      return
    }
    const decl = await this.getShape(type.ident, use)
    if (decl.shape === null) {
      return
    }
    const typeArgs = NominalTypeShape.argsForParams(type.genericArgs, decl.shape.genericParams.length, decl.loc, use)
    for (const nominalSupertypeNotSubst of decl.shape.nominalSupertypes) {
      const nominalSupertypeSubst = NominalTypeShape.subst(
        nominalSupertypeNotSubst,
        decl.shape.genericParams,
        typeArgs
      )
      yield nominalSupertypeSubst
      yield * this.getSupertypes(nominalSupertypeSubst, use)
    }
  }

  /** Returns the supertype which is structural */
  async superStructure (type: NominalTypeShape, use: TSNode): Promise<NominalTypeShape | null> {
    if (NominalTypeShape.isStructural(type)) {
      return type
    }
    for await (const supertype of this.getSupertypes(type, use)) {
      if (NominalTypeShape.isStructural(supertype)) {
        return supertype
      }
    }
    return null
  }

  /** Returns the supertype which is structural */
  async superStructure2 (type: NominalTypeInferred): Promise<NominalTypeInferred | null> {
    return mapNullable(
      await this.superStructure(type.shape, type.ast.node),
      shape => ({ shape, ast: type.ast })
    )
  }

  /** Returns the supertype which is a Promise */
  async superPromise (type: NominalTypeShape, use: TSNode): Promise<NominalTypeShape | null> {
    if (NominalTypeShape.isPromise(type)) {
      return type
    }
    for await (const supertype of this.getSupertypes(type, use)) {
      if (NominalTypeShape.isPromise(supertype)) {
        return supertype
      }
    }
    return null
  }

  /** Returns the supertype which is a Promise */
  async superPromise2 (type: NominalTypeInferred): Promise<NominalTypeInferred | null> {
    return mapNullable(
      await this.superPromise(type.shape, type.ast.node),
      shape => ({ shape, ast: type.ast })
    )
  }
}

export class Scope {
  private _params: Param[] | null = null
  private readonly hoisted: Map<string, HoistedAstBinding> = new Map()
  private readonly sequential: Map<string, ValueDecl[]> = new Map()
  public returnStmt: TSNode | null = null
  public returnValue: TSNode | null = null
  public throwStmt: TSNode | null = null
  public throwValue: TSNode | null = null

  constructor (readonly scopeNode: TSNode) {}

  get params (): readonly Param[] | null {
    return this._params
  }

  setParams (params: Param[]): void {
    assert(this._params === null, 'already has params')
    this._params = params
    for (const param of params) {
      this._addHoisted(param, true)
    }
  }

  addHoisted (decl: HoistedAstBinding): void {
    this._addHoisted(decl, false)
  }

  private _addHoisted (decl: HoistedAstBinding, allowParams: boolean): void {
    assert(scopeParentOf(decl.node)?.id === decl instanceof Param ? scopeParentOf(this.scopeNode)!.id : this.scopeNode.id, 'node added to scope must be in that scope, not a parent or child scope')
    assert(allowParams || !(decl instanceof Param), 'added a param outside of setting params')
    this.hoisted.set(decl.name.node.text, decl)
  }

  getHoisted (name: string): HoistedAstBinding | null {
    return this.hoisted.get(name) ?? null
  }

  addSequential (decl: ValueDecl): void {
    assert(scopeParentOf(decl.node)?.id === this.scopeNode.id, 'node added to scope must be in that scope, not a parent or child scope')
    const name = decl.name.node.text
    if (!this.sequential.has(name)) {
      this.sequential.set(name, [])
    }
    this.sequential.get(name)!.push(decl)
  }

  hasAny (name: string): boolean {
    return this.sequential.has(name) || this.hoisted.has(name)
  }

  getLast (name: string): AstBinding | null {
    return this.getSequentialLast(name) ?? this.getHoisted(name)
  }

  private getSequentialLast (name: string): ValueDecl | null {
    if (!this.sequential.has(name)) {
      return null
    }
    const decls = this.sequential.get(name)!
    return decls[decls.length - 1]
  }

  has (name: string, posNode: TSNode): boolean {
    return this.get(name, posNode) !== null
  }

  get (name: string, posNode: TSNode): AstBinding | null {
    assert(GeneratorUtil.any(scopeParentsOf(posNode), parent => parent.id === this.scopeNode.id), 'Scope#get position node must be in that scope, not a parent or child scope')
    return this.getSequential(name, posNode) ?? this.getHoisted(name)
  }

  private getSequential (name: string, posNode: TSNode): ValueDecl | null {
    if (!this.sequential.has(name)) {
      return null
    }
    const decls = this.sequential.get(name)!
    return ArrayUtil.findLast(decls, decl => decl.node.startIndex <= posNode.node.startIndex)
  }

  getExactSequential (name: string, posNode: TSNode): ValueDecl {
    if (!this.sequential.has(name)) {
      throw new Error(`no sequential declaration for ${name} in scope:\n${this.scopeNode.text}`)
    }
    const decls = this.sequential.get(name)!
    const result = ArrayUtil.findLast(decls, decl => (
      decl.node.startIndex <= posNode.node.startIndex && decl.node.endIndex >= posNode.node.endIndex
    ))
    if (result === null) {
      throw new Error(`no sequential declaration for ${name} in scope:\n${this.scopeNode.text}`)
    }
    return result
  }

  returnType (typedExprs: TypedExprs): NominalReturnTypeInferred {
    if (this.throwValue !== null) {
      return {
        shape: NominalTypeShape.NEVER,
        return: this.throwStmt,
        typeAst: null
      }
    } else if (this.returnValue !== null) {
      const expr = typedExprs.get(this.returnValue)
      return {
        shape: expr?.shape ?? null,
        return: this.returnStmt,
        typeAst: expr?.ast ?? null
      }
    } else {
      return {
        shape: NominalTypeShape.VOID,
        return: this.returnStmt,
        typeAst: null
      }
    }
  }
}
 */