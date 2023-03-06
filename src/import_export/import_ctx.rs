/*
import { ImportResolveCtx, ResolvedFatPath } from 'import-export/import-resolve-ctx'
import { TranspileOutHeader } from 'import-export/exports'

/** Caches imports */
class ImportCache {
  private readonly moduleToFatPath: Map<string, ResolvedFatPath | null> = new Map()
  private readonly fatPathToTranspileOut: Map<ResolvedFatPath, TranspileOutHeader | { error: string }> = new Map()

  /**
   * If module has already been resolved to a full path, return the cached result.
   * Otherwise calls `resolve`
   */
  async cacheResolveModule (
    module: string,
    resolve: () => Promise<ResolvedFatPath | null>
  ): Promise<ResolvedFatPath | null> {
    if (!this.moduleToFatPath.has(module)) {
      const fatPath = await resolve()
      this.moduleToFatPath.set(module, fatPath)
    }
    return this.moduleToFatPath.get(module)!
  }

  /**
   * If the path has already been partially or fully transpiled, returns the cached result.
   * Otherwis calls `transpile`
   */
  async cacheTranspile (
    fatPath: ResolvedFatPath,
    transpile: () => Promise<TranspileOutHeader | { error: string }>
  ): Promise<TranspileOutHeader | { error: string }> {
    if (!this.fatPathToTranspileOut.has(fatPath)) {
      const header = await transpile()
      this.fatPathToTranspileOut.set(fatPath, header)
    }
    return this.fatPathToTranspileOut.get(fatPath)!
  }
}

/**
 * Caches and resolves imports.
 * Typically construct via `ImportCtx.regular` (which is async because it reads tsconfig and other I/O),
 * but you can call `new` for fine-grained control over module resolution
 */
export class ImportCtx {
  /** Caches imports, does change when calling transpile */
  private readonly cache: ImportCache = new ImportCache()

  static async regular (modulePath: string): Promise<ImportCtx> {
    return new ImportCtx(await ImportResolveCtx.regular(modulePath))
  }

  constructor (
    /** Stateless relative to program (doesn't change when calling transpile) */
    private readonly resolve: ImportResolveCtx
  ) {}

  /**
   * Resolves the module path.
   * If already partially or fully transpiled, returns the cached result.
   * Otherwise, calls `transpile` with the actual script path
   */
  async resolveAndCacheTranspile (
    modulePath: string,
    importerPath: string | null,
    transpile: (scriptPath: string) => Promise<TranspileOutHeader | { error: string }>
  ): Promise<TranspileOutHeader | { error: string }> {
    const fatPath = await this.cache.cacheResolveModule(modulePath, async () => await ImportResolveCtx.locateFromModulePath(this.resolve, modulePath, importerPath))
    if (fatPath === null) {
      return { error: `import doesn't exist: ${modulePath}` }
    }
    return await this.cache.cacheTranspile(fatPath, async () => {
      if (fatPath.nominalscriptPath === null) {
        return { error: 'cannot transpile located module because it it not nominalscript and has no nominalscript declaration' }
      }
      return await transpile(fatPath.nominalscriptPath)
    })
  }

  /**
   * Resolves declarations / nominal exports and also checks that `scriptPath` is correct.
   * If already partially or fully transpiled, returns the cached result.
   * Otherwise, calls `transpile` with the actual script path
   */
  async resolveAuxillaryAndCacheTranspile (
    scriptPath: string,
    transpile: () => Promise<TranspileOutHeader | { error: string }>
  ): Promise<TranspileOutHeader | { error: string }> {
    const fatPath = await ImportResolveCtx.locateFromScriptPath(this.resolve, scriptPath)
    if (fatPath === null) {
      return { error: `script path doesn't exist: ${scriptPath}` }
    } else if (fatPath.nominalscriptPath !== scriptPath) {
      return { error: `script path is not a nominalscript path: ${scriptPath}` }
    }
    return await this.cache.cacheTranspile(fatPath, transpile)
  }
}

 */