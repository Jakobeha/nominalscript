/*
import { TSParser, TSTree, SourceCode, TSNodeId } from 'ast/tree-sitter-wrapper'
import ns from 'tree-sitter-nominalscript'
import {
  ArrowParam,
  FormalParam,
  FunctionDecl,
  ImportStatement,
  isAtScope,
  scopeParentOf,
  NominalImportSpecifier,
  NominalType,
  NominalTypeDecl,
  ValueDecl,
  ValueImportSpecifier,
  AstBinding
} from 'ast/typed-ast'
import { AdditionalInfoArgs, FileDiagnostics, logError, ProjectDiagnostics } from 'diagnostics'
import { ImportCtx } from 'import-export/import-ctx'
import { mapNullable } from 'misc/misc'
import assert from 'node:assert'
import { Queries } from 'ast/queries'
import { ModuleCtx, ScopeChain } from 'analysis/scopes'
import { NominalFunctionType, NominalReturnTypeInferred, NominalTypeInferred, NominalTypeShape } from 'analysis/nominal-type-shape'
import { Exports, TranspileOutHeader } from 'import-export/exports'
import { GlobalBinding } from 'ast/bindings'
import { ArrayUtil } from 'misc/array-util'
import { GeneratorUtil } from 'misc/generator-util'
import { LazyPromise } from 'misc/lazy-promise'

export { ImportResolveCtx } from './import-export/import-resolve-ctx'
export { FileDiagnostics, ImportCtx, ProjectDiagnostics, SourceCode }

const { nominalscript: NOMINALSCRIPT } = ns

const PARSER = new TSParser(NOMINALSCRIPT)

/**
 * Transpile (compile) a NominalScript file into TypeScript
 *
 * @param scriptPath path to NominalScript you want to transpile
 * @param importCtx Context for resolving and caching imports
 * @param diagnostics Writer for logging errors, warnings, etc.
 * @returns A `Promise` of the header, which has a thunk that may be resumed to get a `Promise` of the TypeScript code as text.
 *          Or `null` and we log a diagnostic if we cannot resolve.
 */
export async function transpileFile (
  scriptPath: string,
  importCtx: ImportCtx,
  diagnostics: ProjectDiagnostics
): Promise<TranspileOutHeader | null> {
  const header = await importCtx.resolveAuxillaryAndCacheTranspile(scriptPath, async () =>
    await transpileFileNoCache(scriptPath, importCtx, diagnostics))
  if ('error' in header) {
    logError(
      `failed to transpile: ${header.error}`,
      diagnostics
    )
    return null
  }
  return header
}

async function transpileFileNoCache (
  scriptPath: string,
  importCtx: ImportCtx,
  diagnostics: ProjectDiagnostics
): Promise<TranspileOutHeader> {
  const code = await SourceCode.fromFile(scriptPath)
  const fileDiagnostics = diagnostics.get(scriptPath)
  return await transpileCode(code, scriptPath, importCtx, fileDiagnostics, diagnostics)
}

/**
 * Transpile (compile) NominalScript code into TypeScript
 *
 * @param code NominalScript code as text you want to transpile
 * @param importerPath For resolving imports: path to the code file, or `null` if nonexistent
 * @param importCtx Context for resolving and caching imports
 * @param fileDiagnostics Writer for logging errors, warnings, etc. for this file
 * @param projectDiagnostics Writer for logging errors, warnings, etc. for imported files
 * @returns a `Promise` of the TypeScript code as text.
 */
export async function transpileCode (
  code: SourceCode,
  importerPath: string | null,
  importCtx: ImportCtx,
  fileDiagnostics: FileDiagnostics,
  projectDiagnostics: ProjectDiagnostics
): Promise<TranspileOutHeader> {
  const ast = await PARSER.parse(code, fileDiagnostics)
  const exports = Exports.create()
  const finishTranspile = await transpileAstHeader(ast, exports, importerPath, importCtx, projectDiagnostics)
  return {
    exports,
    code: new LazyPromise(async () => {
      await finishTranspile()
      return SourceCode.fromString(ast.rootNode.text)
    })
  }
}

/**
 * Transpile (compile) NominalScript AST into TypeScript. The AST is modified in-place.
 * Calling this function only transpiles the header; you must call the thunk to
 * transpile the rest of the code.
 *
 * @param ast NominalScript ast as text you want to transpile, which gets converted into a valid TypeScript ast.
 * @param exports Will append nominal exports to this map
 * @param importerPath For resolving imports: path to the code file, or `null` if nonexistent
 * @param importCtx Context for resolving and caching imports
 * @param projectDiagnostics Writer for logging errors, warnings, etc. for imported files
 *                           (file diagnostics are in the ast)
 * @returns a thunk which transpiles the rest of the code (because we may not need to and
 *          presumably its more efficient to store intermediate values in a closure
 *          than do the redundant computation)
 */
export async function transpileAstHeader (
  ast: TSTree,
  exports: Exports,
  importerPath: string | null,
  importCtx: ImportCtx,
  projectDiagnostics: ProjectDiagnostics
): Promise<() => Promise<void>> {
  // TODO handle error nodes (don't just crash but ignore, then we can traverse error nodes and throw syntax errors)
  const ctx = ModuleCtx.create()
  const rootId = ast.rootNode.id
  // get also creates the root scope
  const rootScope = ctx.scopes.get(rootId)
  // Query all `nominal_type_declaraton`s and all imports.
  // Build map of nominal type names to their declarations
  // In global scope, build value map and map functions to their declarations.
  // Add imported decls (lazily forward resolved) to the nominal type names and global scope value map
  // Imports are lazily and narrowly resolved so that we allow inter-woven dependencies,
  // laziness tracks self-recursion so we explicitly fail on cycles
  //
  // Everything being a closure like this is inefficient but probably required
  // for us to resolve maximally inter-woven but non-cyclic dependencies.
  // In an optimal version we'd have a sum type e.g. for instantly resolved,
  // and perhaps store closure arguments instead of the closure itself.
  // But here we just need a first impl
  for (const match of Queries.NOMINAL_TYPE.matches(ast.rootNode)) {
    const typeDecl = new NominalTypeDecl(match.captures[0].node)
    const scopeParent = scopeParentOf(typeDecl.node)!
    if (scopeParent.id !== ast.rootNode.id) {
      logError(
        'Currently, nominal-type cannot be declared in modules or other child scopes', typeDecl.node
      )
    }

    const typeName = typeDecl.name.text
    const prevTypeDecl = ctx.nominalTypeDeclMap.get(typeName)
    if (prevTypeDecl !== null) {
      logError(
        `Duplicate nominal type declaration for '${typeName}'`, typeDecl.node,
        [`note: previous declaration for '${typeName}'`, prevTypeDecl.node]
      )
    }
    ctx.nominalTypeDeclMap.set(typeName, typeDecl)
  }
  for (const match of Queries.FUNCTION.matches(ast.rootNode)) {
    const funcDecl = new FunctionDecl(match.captures[0].node)
    const scopeParent = scopeParentOf(funcDecl.node)!
    const scope = ctx.scopes.get(scopeParent)
    const funcName = funcDecl.name.text
    const prevFuncDecl = scope.getHoisted(funcName)
    if (prevFuncDecl !== null) {
      logError(
        `Duplicate function declaration for '${funcName}'`, funcDecl.node,
        [`note: previous declaration for '${funcName}'`, prevFuncDecl.node],
        ['note: duplicate declaration in same scope', scopeParent]
      )
    }
    scope.addHoisted(funcDecl)
  }
  for (const match of Queries.VALUE.matches(ast.rootNode)) {
    const valueDecl = new ValueDecl(match.captures[0].node)
    const scopeParent = scopeParentOf(valueDecl.node)!
    const scope = ctx.scopes.get(scopeParent)
    scope.addSequential(valueDecl)
  }
  for (const match of Queries.IMPORT.matches(ast.rootNode)) {
    const importStmt = new ImportStatement(match.captures[0].node)
    const scopeParent = scopeParentOf(importStmt.node)!
    const scope = ctx.scopes.get(scopeParent)

    const module = new LazyPromise(async () => {
      const module = await importCtx.resolveAndCacheTranspile(
        importStmt.importPath,
        importerPath,
        async scriptPath => await transpileFileNoCache(scriptPath, importCtx, projectDiagnostics)
      )
      if ('error' in module) {
        logError(
          `Error importing module '${importStmt.importPath}': ${module.error}`,
          importStmt.node
        )
        return null
      }
      return module
    })
    for (const importSpec of importStmt.imports) {
      const originalName = importSpec.originalName.text
      const alias = importSpec.alias.text
      switch (importSpec.type) {
        case 'nominal-type': {
          assert(importSpec instanceof NominalImportSpecifier)
          if (scopeParent !== ast.rootNode) {
            logError(
              'Currently, nominal-type imports are not allowed in modules or other child scopes, because they are made visible everywhere',
              importSpec.node
            )
          }

          importSpec.nominalExport = module.map(module => {
            if (module === null) {
              return null
            }
            if (!module.exports.nominal.has(originalName)) {
              logError(
                `Imported nominal type '${originalName}' not found in module '${importStmt.importPathNode.text}'`,
                importSpec.node,
                ['note: module path is here', importStmt.importPathNode]
              )
            }
            return module.exports.nominal.get(originalName) ?? null
          })
          const prevDecl = ctx.nominalTypeDeclMap.get(alias)
          if (prevDecl !== null) {
            logError(
              `Duplicate nominal type declaration for '${alias}'`, importSpec.node,
              [`note: previous declaration for '${alias}'`, prevDecl.node]
            )
          }
          ctx.nominalTypeDeclMap.set(alias, importSpec)
          break
        }
        case 'value': {
          assert(importSpec instanceof ValueImportSpecifier)

          importSpec.valueExport = module.map(module => {
            if (module === null) {
              return null
            }
            if (!module.exports.value.has(originalName)) {
              logError(
                `Imported value '${originalName}' not found in module '${importStmt.importPathNode.text}'`,
                importSpec.node,
                ['note: module path is here', importStmt.importPathNode]
              )
            }
            return module.exports.value.get(originalName) ?? null
          })
          const prevDecl = scope.getHoisted(alias)
          if (prevDecl !== null) {
            logError(
              `Duplicate value declaration for '${alias}'`, importSpec.node,
              [`note: previous declaration for '${alias}'`, prevDecl.node]
            )
          }
          scope.addHoisted(importSpec)
          break
        }
        default:
          throw new Error(`Unhandled import specifier type: ${(importSpec as never as any).type as string}`)
      }
    }
  }

  // Query and prefill exports
  for (const match of Queries.EXPORT_ID.matches(ast.rootNode)) {
    const valueExportId = match.captureNamed('value_export_id')?.node
    const nominalExportId = match.captureNamed('nominal_export_id')?.node
    const isValue = nominalExportId === null
    const exportId = isValue ? valueExportId! : nominalExportId
    assert(exportId)

    // Only root scope is actually exported outside of the file, module and namespace
    // scopes get exported to the module or namespace
    const scopeParent = scopeParentOf(exportId)!
    if (scopeParent.id !== ast.rootNode.id) {
      continue
    }

    const exportAliasId = match.captureNamed('export_alias_id')?.node
    const originalName = exportId.text
    const alias = exportAliasId?.text ?? originalName
    if (exports.nominal.has(alias)) {
      logError(`Duplicate export for '${alias}'`, exportId)
    }
    if (isValue) {
      if (!rootScope.hasAny(originalName)) {
        logError(`No value declaration defined or imported for '${originalName}'`, exportId)
      }
      exports.value.set(alias, {
        originalName,
        decl: rootScope.getLast(originalName)?.resolveValue ?? LazyPromise.immediate(null)
      })
    } else {
      if (!ctx.nominalTypeDeclMap.has(originalName)) {
        logError(`No nominal type declaration defined or imported for '${originalName}'`, exportId)
      }
      exports.nominal.set(alias, {
        originalName,
        decl: ctx.nominalTypeDeclMap.get(originalName)?.resolveNominal ?? LazyPromise.immediate(null)
      })
    }
  }

  // Rest of the code is in a thunk: we don't have to run any of this if we're only
  // loading the file to read imported decls
  return async () => {
    // TODO: Add guards (in type analysis, but probably first in function decls we've already scanned. We have to wait until we're in the return block though)

    // Run type analysis (and scope analysis and other dependent analyses)
    // Instead of querying here we do an inorder traversal because we need to do inorder
    // But Queries.SCOPE roughly matches what we're using
    const scopes = new ScopeChain(ctx)
    const cursor = ast.walk()
    const skipNodes = new Set<TSNodeId>()
    let lastMove: 'start' | 'down' | 'right' | 'up' | 'end' = 'start'
    while (lastMove !== 'end') {
      const node = cursor.currentNode
      if (isAtScope(cursor)) {
        if (lastMove !== 'up') {
          scopes.push(node)
        } else {
          scopes.pop()
        }
      }

      // Nodes are traversed once or twice: once 'start' or 'down' or 'right',
      // once if they have children 'up'
      let skipChildren = false
      if (skipNodes.has(node.id)) {
        skipChildren = true
        skipNodes.delete(node.id)
      } else if (node.isDeleted()) {
        skipChildren = true
      } else if (node.isNamed) {
        switch (node.type) {
          case 'function_declaration':
          case 'generator_function_declaration':
          case 'function':
          case 'generator_function':
          case 'arrow_function': {
            const isDecl =
              node.type === 'function_declaration' ||
              node.type === 'generator_function_declaration'
            // We want to reuse params if a function decl because we can infer
            // param types in future calls. In an expression, we assign the type to the
            // node to do this
            const decl = !isDecl
              ? null
              : (scopes.getHoistedInTopScope(node.fieldChild('name')!.text) as FunctionDecl)
            assert(!isDecl || (decl !== null && (decl as any) instanceof FunctionDecl))
            const body = node.fieldChild('body')!
            const funScope = ctx.scopes.get(body)
            if (lastMove !== 'up') {
              // Declare parameters in function scope and assign explicit types
              // TODO: Scope nominal type params as well
              const parameters =
                // Decl parameters
                decl !== null
                  ? decl.formalParams
                // Expression formal parameters
                  : node.fieldChild('parameters')?.namedChildren?.map(x => new FormalParam(x)) ??
                // Arrow parameters
                [new ArrowParam(node.fieldChild('parameter')!)]
              funScope.setParams(parameters)

              // Don't traverse binding ids
              for (const param of parameters) {
                // Use cursor to traverse children because we need field name and it's
                // more efficient anyways
                const cursor = param.node.walk()
                cursor.gotoFirstChild()
                while (cursor.gotoNextSibling()) {
                  if (
                    // We *do* traverse values
                    cursor.currentFieldName !== 'value' ||
                    // We also don't traverse nominal type but it's unnecessary to add
                    // because that gets skipped anyways (deleted)
                    cursor.nodeType === 'nominal_type_annotation'
                  ) {
                    skipNodes.add(cursor.currentNodeId)
                  }
                }
              }
            } else {
              // Infer parameter and return types, and check return type if explicit.
              // If decl, the parameter types are automatically set after backwards-inference
              // and if we infer the return type we set it on the decl.
              // If expression, we assign inferred func type to the node itself

              // Backwards-infer param types
              const parameters = funScope.params!
              for (const param of parameters) {
                if (param.nominalType === null) {
                  param.setCustomInferredType(new LazyPromise(async () => ctx.typedExprs.backwardsInfer(param)))
                }
              }

              const explicitReturnType = mapNullable(
                node.fieldChild('nominal_return_type'),
                x => { x.delete(); return new NominalType(x.namedChild(0)!) }
              )

              // It's easier to just check if there is a type assigned instead of checking
              // if body itself is an expression, since there are many expression node
              // types. If it's not than there will never be a type assigned
              const inferredReturnTypes = mapNullable(
                ctx.typedExprs.get(body),
                ret => [{ shape: ret.shape, return: body, typeAst: ret.ast }]
                // We've seen the return types since lastMove === 'up'
              ) ?? Array.from(ctx.scopes.seenReturnTypes(funScope, ctx.typedExprs))

              if (explicitReturnType !== null) {
                // Check that explicit and inferred returned types match,
                // and print a descriptive error based on return Void etc.
                const logReturnError = (message: string, inferredReturnType: NominalReturnTypeInferred, ...notes: AdditionalInfoArgs[]): void => {
                  logError(
                    message, inferredReturnType.return ?? node,
                    ...notes,
                    explicitReturnType === null ? null : ['note: return type', explicitReturnType.node]
                  )
                }
                if (NominalTypeShape.isVoid(explicitReturnType.shape)) {
                  for (const inferredReturnType of inferredReturnTypes) {
                    if (inferredReturnType.shape === null) {
                      logReturnError('function doesn\'t return a value but this might (can\'t infer)', inferredReturnType)
                    } else if (!NominalTypeShape.isVoid(inferredReturnType.shape) &&
                               !NominalTypeShape.isNever(inferredReturnType.shape)) {
                      logReturnError('function doesn\'t return a value but this does', inferredReturnType)
                    }
                  }
                } else {
                  for (const inferredReturnType of inferredReturnTypes) {
                    if (inferredReturnType.shape === null) {
                      logReturnError('incorrect (non-nominal) return type', inferredReturnType)
                    } else if (NominalTypeShape.isVoid(inferredReturnType.shape)) {
                      logReturnError('expected a return value', inferredReturnType)
                    } else {
                      const { isAssignable, errors } = NominalTypeShape.isAssignableTo(inferredReturnType.shape, explicitReturnType.shape)
                      if (!isAssignable) {
                        logReturnError(
                          'incorrect return type', inferredReturnType,
                          ...errors.map(({ error }) => [`issue: ${error}`] as AdditionalInfoArgs)
                        )
                      }
                    }
                  }
                }
              }
              const returnType = explicitReturnType?.asInferred?.shape ??
                NominalReturnTypeInferred.tryUnifyAll('Return', inferredReturnTypes, { node })
              if (decl !== null && explicitReturnType === null && returnType !== null) {
                // We need to set the inferred return type on the decl
                // (if explicit it's already set)
                decl.setCustomInferredReturnType(returnType)
              }

              if (decl === null) {
                const funcType = FunctionDecl.funcType(
                  null,
                  FunctionDecl.nominalParams(node),
                  parameters,
                  returnType
                )
                // Assign the node a function type
                ctx.typedExprs.assign(node, {
                  shape: funcType,
                  // (function expression is its own type signature node)
                  ast: { node }
                })
              }
            }
            break
          }
          case 'variable_declarator': {
            const name = node.namedChild(0)!
            const nominalType = mapNullable(
              node.childOfType('nominal_type_annotation'),
              x => new NominalType(x.namedChild(0)!)
            )
            const value = node.fieldChild('value')
            const decl = scopes.getExactSequential(name.text, node)
            assert(decl.nominalType?.node.id === nominalType?.node.id)
            if (lastMove !== 'up') {
              if (value !== null && nominalType !== null) {
                // Require value to be nominal type
                ctx.typedExprs.require(value, nominalType.asInferred)
              }

              // Don't traverse binding ids
              skipNodes.add(name.id)
              // We also don't traverse nominal type but it's unnecessary to add
              // because that gets skipped anyways (deleted)
            } else if (value !== null && nominalType === null) {
              // Infer nominal type from value
              decl.setCustomInferredType(new LazyPromise(async () => scopes.inferType(value)))
            }
            // Already assigned via decl, and the node itself is a statement
            // ctx.typedExprs.assign(node, nominalType.asInferred)
            break
          }
          case 'return_statement': {
            if (lastMove === 'up') {
              scopes.addReturn(node, node.namedChild(0))
            }
            break
          }
          case 'throw_statement': {
            if (lastMove === 'up') {
              scopes.addThrow(node, node.namedChild(0))
            }
            break
          }
          case 'assignment_expression': {
            if (lastMove === 'up') {
              const left = node.namedChild(0)!
              const right = node.namedChild(1)!
              const leftType = scopes.inferType(left)
              if (leftType !== null) {
                ctx.typedExprs.require(right, leftType)
              }
              // Can't re-assign right type to the left id,
              // because of issues with scope and loops/break,
              // but we can assign it to the expression itself.
              // Also we need to assign the type because assignments are expressions
              // and thus can be inside other expressions
              const assignedType = leftType ?? scopes.inferType(right)
              if (assignedType !== null) {
                ctx.typedExprs.assign(node, assignedType)
              }
            }
            break
          }
          // case 'augmented_assignent_expression':
          // case 'binary_expression':
          // case 'update_expression':
          //   We will add support for declaring operations on nominal types, e.g.
          //   `operation; Liters + Liters -> Liters`
          //   `operation; Liters * Float -> Liters`
          //   And then call `ctx.typedOperations.get(left, op, right)`
          //   to ensure that such a declaration exists and get the return type
          //   ...But not yet
          case 'ternary_expression': {
            if (lastMove === 'up') {
              // const condition = node.namedChild(0)!
              const then = node.namedChild(1)!
              const else_ = node.namedChild(2)!
              const thenType = scopes.inferType(then)
              const elseType = scopes.inferType(else_)
              if (thenType !== null && elseType !== null) {
                const unified = NominalTypeInferred.tryUnify('Ternary branch', thenType, elseType, { node })
                if (unified !== null) {
                  ctx.typedExprs.assign(node, unified)
                }
              }
            }
            break
          }
          case 'sequence_expression': {
            if (lastMove === 'up') {
              const last = node.lastNamedChild()!
              const lastType = scopes.inferType(last)
              if (lastType !== null) {
                ctx.typedExprs.assign(node, lastType)
              }
            }
            break
          }
          case 'member_expression': {
            if (lastMove === 'up') {
              const obj = node.namedChild(0)!
              const member = node.fieldChild('property')!.text
              const isOptional = node.childOfType('optional_chain') !== null
              const objTypeRaw = scopes.inferType(obj)
              if (objTypeRaw !== null) {
                const objType = await ctx.nominalTypeDeclMap.superStructure2(objTypeRaw) ?? objTypeRaw
                if (!NominalTypeShape.isObject(objType.shape)) {
                  logError(
                    `Expected object type, got ${NominalTypeShape.print(objType.shape)}`, node,
                    ['note: type declared here', objTypeRaw.ast.node]
                  )
                } else {
                  const memberType = NominalTypeInferred.member(objType, member, isOptional)
                  if (memberType === null) {
                    logError(
                      `No such member ${member} in ${NominalTypeShape.print(objType.shape)}`, node,
                      ['note: type declared here', objTypeRaw.ast.node]
                    )
                  } else {
                    ctx.typedExprs.assign(node, memberType)
                  }
                }
              }
            }
            break
          }
          case 'subscript_expression': {
            if (lastMove === 'up') {
              const obj = node.namedChild(0)!
              let index: number | null = parseInt(node.fieldChild('index')!.text)
              if (isNaN(index)) {
                index = null
              }
              const isOptional = node.childOfType('optional_chain') !== null
              const arrayTypeRaw = scopes.inferType(obj)
              if (arrayTypeRaw !== null) {
                const arrayType = await ctx.nominalTypeDeclMap.superStructure2(arrayTypeRaw) ?? arrayTypeRaw
                if (!NominalTypeShape.isTuple(arrayType.shape) && !NominalTypeShape.isArray(arrayType.shape)) {
                  logError(
                    `Expected tuple or array type, got ${NominalTypeShape.print(arrayType.shape)}`, node,
                    ['note: type declared here', arrayTypeRaw.ast.node]
                  )
                } else {
                  const subscriptType = NominalTypeInferred.subscript(arrayType, index, isOptional)
                  if (subscriptType === null && index !== null) {
                    logError(
                      `Index ${index} out of bounds in ${NominalTypeShape.print(arrayType.shape)}`, node,
                      ['note: type declared here', arrayTypeRaw.ast.node]
                    )
                  } else if (subscriptType !== null) {
                    ctx.typedExprs.assign(node, subscriptType)
                  }
                }
              }
            }
            break
          }
          case 'await_expression': {
            if (lastMove === 'up') {
              const expr = node.namedChild(0)!
              const exprType = scopes.inferType(expr)
              if (exprType !== null) {
                const promiseType = await ctx.nominalTypeDeclMap.superPromise2(exprType)
                if (promiseType !== null) {
                  const awaitedType = NominalTypeInferred.await_(exprType)
                  if (awaitedType !== null) {
                    ctx.typedExprs.assign(node, awaitedType)
                  }
                }
              }
            }
            break
          }
          case 'call_expression': {
            if (lastMove === 'up') {
              const func = node.namedChild(0)!
              const funcIsNullable = node.childOfType('optional_chain') !== null
              const args = node.childOfType('arguments')
              if (args === null) {
                logError(
                  'Can\'t handle function which takes template string as args', node
                )
                break
              }
              const funcTypeRaw = scopes.inferType(func)

              if (funcTypeRaw !== null) {
                const funcType = await ctx.nominalTypeDeclMap.superStructure2(funcTypeRaw) ?? funcTypeRaw
                if (!NominalTypeShape.isFunction(funcType.shape)) {
                  logError(
                    'Can\'t call non-function', func,
                    funcType.ast !== null ? ['note: type is here', funcType.ast.node] : null
                  )
                } else {
                  if (funcType.shape.isNullable && !funcIsNullable) {
                    logError(
                      'Can\'t call nullable function without optional chain', func,
                      funcType.ast !== null ? ['note: type is here', funcType.ast.node] : null
                    )
                  }
                  const paramLenRange = NominalFunctionType.paramLenRange(funcType.shape)
                  if (args.namedChildren.length < paramLenRange.min) {
                    logError(
                      `Too few arguments to function, expected ${paramLenRange.min} but got ${args.namedChildren.length}`,
                      args
                    )
                  } else if (args.namedChildren.length > paramLenRange.max) {
                    logError(
                      `Too many arguments to function, expected ${paramLenRange.max} but got ${args.namedChildren.length}`,
                      args
                    )
                  }
                  for (let i = 0; i < args.namedChildren.length; i++) {
                    const arg = args.namedChildren[i]
                    const paramType = NominalFunctionType.paramType(funcType.shape, i)
                    if (paramType !== 'any' && paramType !== 'out-of-range') {
                      ctx.typedExprs.require(arg, {
                        shape: paramType,
                        ast: funcType.ast
                      })
                    }
                  }

                  if (funcType.shape.returnValue !== 'any') {
                    ctx.typedExprs.assign(node, {
                      shape: funcType.shape.returnValue,
                      ast: funcType.ast
                    })
                  }
                }
              }
            }
            break
          }
          case 'parenthesized_expression': {
            if (lastMove !== 'up') {
              const expr = node.namedChild(0)!
              const type = mapNullable(
                node.childOfType('nominal_type_annotation'),
                x => { x.delete(); return new NominalType(x.namedChild(0)!) }
              )
              if (type !== null) {
                ctx.typedExprs.require(expr, type.asInferred)
                ctx.typedExprs.assign(node, type.asInferred)
              }
            } else {
              const expr = node.namedChild(0)!
              const typeAnnotation = node.childOfType('nominal_type_annotation')
              if (typeAnnotation === null) {
                const inferredType = scopes.inferType(expr)
                if (inferredType !== null) {
                  ctx.typedExprs.assign(node, inferredType)
                }
              }
            }
            break
          }
          case 'this':
          case 'identifier':
          case 'shorthand_property_identifier': {
            const def = scopes.get(node.text, node)
            if (def === null) {
              logError(`Unresolved identifier '${node.text}'`, node)
              break
            }
            if (def instanceof AstBinding) {
              def.addUse(node)
              const type = await def.inferTypeFor(node)
              if (type === null) break
              ctx.typedExprs.assign(node, type)
            } else if (def instanceof GlobalBinding) {
              if (def.type === null) break
              ctx.typedExprs.assign(node, {
                shape: def.type,
                ast: { node }
              })
            } else {
              throw new Error(`Unexpected binding type ${def.constructor.name}`)
            }

            skipChildren = true // there are no children
            break
          }
          case 'true':
          case 'false': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.BOOLEAN,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'null': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.NULL,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'number': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.number(node.text),
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'string':
          case 'template_string': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.STRING,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'regex': {
            ctx.typedExprs.assign(node, {
              shape: NominalTypeShape.REGEX,
              ast: { node }
            })

            skipChildren = true // there are no children
            break
          }
          case 'object': {
            if (lastMove === 'up') {
              let fields: Array<[string | number, NominalTypeShape]> | null = []
              for (const child of node.namedChildren) {
                switch (child.type) {
                  case 'pair': {
                    const key = child.namedChild(0)!
                    const value = child.namedChild(1)!
                    const type = scopes.inferType(value)?.shape ?? NominalTypeShape.ANY
                    fields.push([key.text, type])
                    break
                  }
                  case 'shorthand_property_identifier': {
                    const name = child.namedChild(0)!
                    const type = scopes.inferType(name)?.shape ?? NominalTypeShape.ANY
                    fields.push([name.text, type])
                    break
                  }
                  case 'spread_element': {
                    const expr = child.namedChild(0)!
                    const spreadType = scopes.inferType(expr)
                    if (spreadType === null) {
                      fields = null
                    } else {
                      const spreadObjType = (await ctx.nominalTypeDeclMap.superStructure2(spreadType))?.shape ?? null
                      if (spreadObjType === null || !NominalTypeShape.isObject(spreadObjType)) {
                        logError(
                          'Cannot spread non-object type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          spreadObjType === null ? null : [`note: actual structure is ${NominalTypeShape.print(spreadObjType)}`]
                        )
                        fields = null
                      } else if (spreadObjType.isNullable) {
                        logError(
                          'Cannot spread nullable object type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          [`note: actual structure is ${NominalTypeShape.print(spreadObjType)}`]
                        )
                        fields = null
                      } else {
                        fields.push(...spreadObjType.fields)
                      }
                    }
                    break
                  }
                  case 'method_definition': {
                    const name = child.namedChild(0)!
                    const type = scopes.inferType(child)?.shape ?? NominalTypeShape.FUNCTION
                    fields.push([name.text, type])
                    break
                  }
                  default:
                    throw new Error(`unhandled object literal child type: ${child.type}`)
                }
                if (fields === null) {
                  break
                }
              }
              fields = mapNullable(fields, x => ArrayUtil.dedupedBy(x, ([key, _]) => key))

              ctx.typedExprs.assign(node, {
                shape: fields === null
                  ? NominalTypeShape.OBJECT
                  : NominalTypeShape.object(fields),
                ast: { node }
              })
            }
            break
          }
          case 'array': {
            if (lastMove === 'up') {
              let elems: NominalTypeShape[] | NominalTypeShape | null = []
              const pushElems = (...newElems: NominalTypeShape[]): NominalTypeShape[] | NominalTypeShape => {
                if (elems! instanceof Array) {
                  elems.push(...newElems)
                  return elems
                } else {
                  return GeneratorUtil.last(NominalTypeShape.tryUnifyAll([elems, ...newElems]))!
                }
              }
              const pushArray = (newElem: NominalTypeShape): NominalTypeShape[] | NominalTypeShape => {
                if (elems! instanceof Array) {
                  return GeneratorUtil.last(NominalTypeShape.tryUnifyAll([...elems, newElem]))!
                } else {
                  return GeneratorUtil.last(NominalTypeShape.tryUnify(elems, newElem, '_', '_', false))!
                }
              }
              for (const child of node.namedChildren) {
                switch (child.type) {
                  case 'spread_element': {
                    const expr = child.namedChild(0)!
                    const spreadType = scopes.inferType(expr)
                    if (spreadType === null) {
                      elems = null
                    } else {
                      const spreadArrayType = (await ctx.nominalTypeDeclMap.superStructure2(spreadType))?.shape ?? null
                      if (spreadArrayType === null ||
                          (!NominalTypeShape.isArray(spreadArrayType) && !NominalTypeShape.isTuple(spreadArrayType))) {
                        logError(
                          'Cannot spread non-array and non-tuple type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          spreadArrayType === null ? null : [`note: actual structure is ${NominalTypeShape.print(spreadArrayType)}`]
                        )
                        elems = null
                      } else if (spreadArrayType.isNullable) {
                        logError(
                          'Cannot spread nullable array type', expr,
                          ['note: type declared here', spreadType.ast.node],
                          [`note: actual structure is ${NominalTypeShape.print(spreadArrayType)}`]
                        )
                        elems = null
                      } else if (NominalTypeShape.isTuple(spreadArrayType)) {
                        elems = pushElems(...spreadArrayType.elems)
                      } else {
                        elems = pushArray(spreadArrayType.elem)
                      }
                    }
                    break
                  }
                  default: {
                    elems = pushElems(scopes.inferType(child)?.shape ?? NominalTypeShape.ANY)
                    break
                  }
                }
                if (elems === null) {
                  break
                }
              }

              ctx.typedExprs.assign(node, {
                shape: elems === null
                  ? NominalTypeShape.ARRAY
                  : elems instanceof Array
                    ? NominalTypeShape.tuple(elems)
                    : NominalTypeShape.array(elems),
                ast: { node }
              })
            }
            break
          }
          case 'nominal_wrap_expression':
          case 'nominal_wrap_unchecked_expression': {
            if (lastMove !== 'up') {
              const nominalType = new NominalType(node.namedChild(0)!)
              const expr = node.namedChild(1)!
              if (node.type === 'nominal_wrap_expression') {
                ctx.typedExprs.rtRequire(expr, nominalType.asInferred)
              }
              ctx.typedExprs.assign(node, nominalType.asInferred)
              node.replaceWithChild(expr)
            }
            break
          }
          case 'nominal_type_annotation':
          case 'nominal_type_identifier':
          case 'parenthesized_nominal_type':
          case 'generic_nominal_type':
          case 'object_nominal_type':
          case 'array_nominal_type':
          case 'tuple_nominal_type':
          case 'function_nominal_type':
          case 'nullable_nominal_type':
            logError(
              'Unhandled nominal type (nominal type in currently unsupported position)', node
            )
            skipChildren = true
            break
          default:
            break
        }
      }

      if (skipChildren) {
        // Note that lastMove is immediately set afterwards and gotoInorder doesn't
        // treat it as 'nextMove', so we aren't necessarily going up; we are only
        // not going down because lastMove is actually passed to gotoInorder to not
        // get a 'down' -> ... -> 'up' -> 'down' infinite loop. Maybe it should be
        // made more clear and something besides lastMove should be used instead...
        lastMove = 'up'
      }
      lastMove = cursor.gotoInorder(lastMove)
    }

    // Type analysis (and scope analysis etc.) are done.
    // But not we need to throw errors for bad types
    // (we wait until analysis is done because some of these errors are due to uninferred types,
    // and we may infer types at weird future times which make them disappear.
    // So it's easier to just wait until ALL analysis is done to raise these uninferred-errors and the general category)
    ctx.typedExprs.checkAll(ast)

    // Roadmap (partially outdated)
    // - Query all `function_declaration`s, `method_declaration`s, `function_signature`s, etc., in every scope build a map of the static functions' types. Remove their nominal type annotations, replace them with guarded versions and which call a generated unguarded version. Also build a map of them as ordinary global (lambda) constants to their nominal function types (already somewhat done, some of the others are somewhat done as well)
    // - Run type checking: remove and check every type annotation we can. Similar to the last step, we build a map of dynamic functions' (lambdas) types and generate guarded/unguarded versions (the difference is that we're also doing inference, so previous statements don't get the inferred types), and we map ordinary values to their types. All calls go to guarded functions by default since they have the original name; we replace checked calls with their unguarded versions if we're absolutely sure the types are ok. We also add inline guards to nominal type annotated expressions and initializers/assignments of nominal type annotated values when we can't infer their type, including annotated exceptions in `catch` clauses. Whenever we infer a type but it is wrong, we generate a type error but continue to generate with guards (which, unless we messed up somewhere, will always throw a runtime error). We remove all nominal type annotations from the analyzed declarations and checked identifiers/expressions, to mark they've been analyzed and because we need to strip them anyways. To summarize:
    //   - Each scope has 2 maps: functions and values. There is also a map of all (global) nominal type declarations
    //   - Imports have already been added to the global scope maps
    //   - We add analyzed declarations to a map in each scope
    //   - We remove type annotations of everything we analyze
    //   - We replace inferred and type-checked function and lambda calls with their unguarded versions
    //   - We add guards to nominal type annotated expressions and initializer/assignments of nominal type annotated values when we can't infer. Function calls which we can't infer already have guards
    //   - Whenever we infer a type but it is wrong, we generate a type error but continue to generate with guards
    // - Generate more type errors error for any nominal type annotation we don't handle
    // - Strip unused guarded / unguarded versions of functions which don't get exported
    //
    // In particular, remember to remove:
    // - FunctionDecl and ValueDecl nominal-type annotations
    // - Query nominal imports any exports (we already handle them and warn for child scopes)
    // and then log errors for anything we don't remove
  }
}

 */