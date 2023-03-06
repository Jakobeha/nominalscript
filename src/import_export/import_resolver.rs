use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use nonempty::NonEmpty;

/// Module resolution strategy *and* source root.
/// Usually generated from `ImportResolveCtx.regular`,
/// which takes a module root path and parses the tsconfig to load the other info.
/// But it's also customizable, probably moreso than necessary.
pub struct ImportResolver {
    /// Whether to resolve absolute paths from `moduleRootPath`/node_modules,
    /// *and* resolve node module cached imports in `moduleRootPath`/node_modules/`module`/out/nominal/`remainderPath`
    pub resolve_node_modules: bool,
    /// Relative paths from `absoluteImportBasePath` where we can resolve absolute imported sources.
    /// A record of glob to an array of possible expansions.
    ///
    /// This is the `"paths"` of your `tsconfig.json`
    pub absolute_import_source_paths: GlobPaths,
    /// Base path appended to paths where we resolve absolute imported sources.
    /// May be relative to `moduleRoot`.
    ///
    /// This is the `"baseUrl"` of your `tsconfig.json`
    pub absolute_import_base_path: PathBuf,
    /// Project root. This is the location of your `tsconfig.json`
    pub module_root_path: PathBuf,
}

pub type GlobPaths = HashMap<String, NonEmpty<String>>;

pub struct ResolvedNodeModulePath {
    pub node_module: PathBuf,
    pub remainder_path: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedPath {
    Regular { path: PathBuf },
    NodeModule {
        node_module: PathBuf,
        remainder_path: String,
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ResolvedFatPath {
    /// Path to .ns or .d.ns. If .ns then all other paths are null
    pub nominalscript_path: Option<PathBuf>,
    /// Path to .ts or .d.ts. If .ts then all other paths are null except nominalscript_path may be non-null
    pub typescript_path: Option<PathBuf>,
    /// Path to .js. If non-null then arbitrary_path is null
    pub javascript_path: Option<PathBuf>,
    /// Path if this is an arbitrary file which is not js/ts/ns (e.g. json). If non-null then javascript_path is null
    pub arbitrary_path: Option<PathBuf>,
}

impl ResolvedFatPath {
    pub fn ns(nominalscript_path: PathBuf) -> Self {
        assert_eq!(nominalscript_path.extension(), Some("ns".as_ref()));
        Self {
            nominalscript_path: Some(nominalscript_path),
            ..Default::default()
        }
    }

    pub fn ts(
        nominalscript_path: Option<PathBuf>,
        typescript_path: PathBuf
    ) -> Self {
        if let Some(nominalscript_path) = nominalscript_path.as_deref() {
            assert!(nominalscript_path.ends_with("d.ns"));
        }
        assert_eq!(typescript_path.extension(), Some("ts".as_ref()));
        Self {
            nominalscript_path,
            typescript_path: Some(typescript_path),
            ..Default::default()
        }
    }

    pub fn js(
        nominalscript_path: Option<PathBuf>,
        typescript_path: Option<PathBuf>,
        javascript_path: PathBuf
    ) -> Self {
        if let Some(nominalscript_path) = nominalscript_path.as_deref() {
            assert!(nominalscript_path.ends_with("d.ns"));
        }
        if let Some(typescript_path) = typescript_path.as_deref() {
            assert!(typescript_path.ends_with("d.ts"));
        }
        assert_eq!(javascript_path.extension(), Some("js".as_ref()));
        Self {
            nominalscript_path,
            typescript_path,
            javascript_path: Some(javascript_path),
            ..Default::default()
        }
    }

    pub fn arbitrary(
        nominalscript_path: Option<PathBuf>,
        typescript_path: Option<PathBuf>,
        arbitrary_path: PathBuf
    ) -> Self {
        if let Some(nominalscript_path) = nominalscript_path.as_deref() {
            assert!(nominalscript_path.ends_with("d.ns"));
        }
        if let Some(typescript_path) = typescript_path.as_deref() {
            assert!(typescript_path.ends_with("d.ts"));
        }
        Self {
            nominalscript_path,
            typescript_path,
            javascript_path: None,
            arbitrary_path: Some(arbitrary_path)
        }
    }

    pub fn null() -> Self {
        Default::default()
    }

    pub fn is_null(&self) -> bool {
        self.nominalscript_path.is_none()
            && self.typescript_path.is_none()
            && self.javascript_path.is_none()
            && self.arbitrary_path.is_none()
    }
}

impl Display for ResolvedFatPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut did_write = false;
        if let Some(nominalscript_path) = self.nominalscript_path.as_ref() {
            write!(f, "nominalscript @ {}", nominalscript_path.display())?;
            did_write = true;
        }
        if let Some(typescript_path) = self.typescript_path.as_ref() {
            if did_write { write!(", ")?; }
            write!(f, "typescript @ {}", typescript_path.display())?;
            did_write = true;
        }
        if let Some(javascript_path) = self.javascript_path.as_ref() {
            if did_write { write!(", ")?; }
            write!(f, "javascript @ {}", javascript_path.display())?;
            did_write = true;
        }
        if let Some(arbitrary_path) = self.arbitrary_path.as_ref() {
            if did_write { write!(", ")?; }
            write!(f, "arbitrary @ {}", arbitrary_path.display())?;
            did_write = true;
        }
        if !did_write {
            write!(f, "null")?;
        }
        Ok(())
    }
}

/*

export module ImportResolveCtx {
  export async function regular (moduleRootPath: string): Promise<ImportResolveCtx> {
    let tsConfig!: any
    try {
      tsConfig = await fs.readJson(pathlib.join(moduleRootPath, 'tsconfig.json'))
    } catch (e: any) {
      if (e.code !== 'ENOENT') {
        throw e
      }
      tsConfig = {}
    }

    return {
      resolveNodeModules: tsConfig?.compilerOptions?.moduleResolution?.toLowerCase()?.contains('node') ?? false,
      absoluteImportSourcePaths: tsConfig?.compilerOptions?.paths ?? {},
      absoluteImportBasePath: pathlib.join(moduleRootPath, tsConfig?.compilerOptions?.baseUrl ?? '.'),
      moduleRootPath,
      useCaches: true
    }
  }

  export async function locateFromModulePath (
    ctx: ImportResolveCtx,
    modulePath: string,
    importerPath: string | null = null
  ): Promise<ResolvedFatPath | null> {
    return await locateFromResolvedCandidates(ctx, resolveCandidatesForModulePath(ctx, modulePath, importerPath))
  }

  export async function locateFromScriptPath (
    ctx: ImportResolveCtx,
    scriptPath: string
  ): Promise<ResolvedFatPath | null> {
    return await locateFromResolvedCandidates(ctx, [removeExtension(scriptPath)])
  }

  export async function locateFromResolvedCandidates (ctx: ImportResolveCtx, candidates: ResolvedPath[]): Promise<ResolvedFatPath | null> {
    let nominalscriptDeclPath: `${string}.d.ns` | null = null
    let typescriptDeclPath: `${string}.d.ts` | null = null
    for (const candidate of candidates) {
      if (typeof candidate === 'string') {
        const scriptPathBase = candidate
        if (await fs.pathExists(scriptPathBase)) {
          // Don't check for .d.*s because if we resolve an absolute declaration,
          // we only want to import the declaration anyways
          if (scriptPathBase.endsWith('.ns')) {
            return {
              nominalscriptPath: scriptPathBase as `${string}.ns`,
              typescriptPath: null,
              javascriptPath: null,
              arbitraryPath: null
            }
          } else if (scriptPathBase.endsWith('.ts')) {
            return {
              nominalscriptPath: nominalscriptDeclPath,
              typescriptPath: scriptPathBase as `${string}.ts`,
              javascriptPath: null,
              arbitraryPath: null
            }
          } else if (scriptPathBase.endsWith('.js')) {
            return {
              nominalscriptPath: nominalscriptDeclPath,
              typescriptPath: typescriptDeclPath,
              javascriptPath: scriptPathBase as `${string}.js`,
              arbitraryPath: null
            }
          } else {
            return {
              nominalscriptPath: nominalscriptDeclPath,
              typescriptPath: typescriptDeclPath,
              javascriptPath: null,
              arbitraryPath: scriptPathBase
            }
          }
        }
        if (await fs.pathExists(`${scriptPathBase}.ns`)) {
          return {
            nominalscriptPath: `${scriptPathBase}.ns`,
            typescriptPath: null,
            javascriptPath: null,
            arbitraryPath: null
          }
        } else if (await fs.pathExists(`${scriptPathBase}.d.ns`)) {
          nominalscriptDeclPath = `${scriptPathBase}.d.ns`
        }
        if (await fs.pathExists(`${scriptPathBase}.ts`)) {
          return {
            nominalscriptPath: nominalscriptDeclPath,
            typescriptPath: `${scriptPathBase}.ts`,
            javascriptPath: null,
            arbitraryPath: null
          }
        } else if (await fs.pathExists(`${scriptPathBase}.d.ts`)) {
          typescriptDeclPath = `${scriptPathBase}.d.ts`
        }
        if (await fs.pathExists(`${scriptPathBase}.js`)) {
          return {
            nominalscriptPath: nominalscriptDeclPath,
            typescriptPath: typescriptDeclPath,
            javascriptPath: `${scriptPathBase}.js`,
            arbitraryPath: null
          }
        }
        // Failed to resolve (we don't check arbitrary extensions)
      } else {
        const { nodeModule, remainderPath } = candidate
        const nodeModulePath = pathlib.join(ctx.moduleRootPath, 'node_modules', nodeModule)
        const nominalscriptDeclTestPath = pathlib.join(nodeModulePath, 'out', 'nominal', 'lib', `${remainderPath}.d.ns`) as `${string}.d.ns`
        const typescriptDeclTestPath = pathlib.join(nodeModulePath, 'out', 'types', 'lib', `${remainderPath}.d.ts`) as `${string}.d.ts`
        const javascriptTestPath = pathlib.join(nodeModulePath, 'out', 'lib', `${remainderPath}.js`) as `${string}.js`
        const arbitraryTestPath = pathlib.join(nodeModulePath, 'out', 'resources', remainderPath)
        const nominalscriptPath = await fs.pathExists(nominalscriptDeclTestPath) ? nominalscriptDeclTestPath : nominalscriptDeclPath
        const typescriptPath = await fs.pathExists(typescriptDeclTestPath) ? typescriptDeclTestPath : typescriptDeclPath
        const javascriptPath = await fs.pathExists(javascriptTestPath) ? javascriptTestPath : null
        const arbitraryPath = javascriptPath === null && await fs.pathExists(arbitraryTestPath) ? arbitraryTestPath : null
        if (nominalscriptPath !== null || typescriptPath !== null || javascriptPath !== null || arbitraryPath !== null) {
          // Can prove that this satisfies ResolvedFatPath's extra constraints but TypeScript can't
          // TODO: remove lint disable and uncomment satisfies when esbuild supports it
          // eslint-disable-next-line @typescript-eslint/consistent-type-assertions
          return {
            nominalscriptPath,
            typescriptPath,
            javascriptPath,
            arbitraryPath
          } as ResolvedFatPath
          // } satisfies _ResolvedFatPath as ResolvedFatPath
        }
        // Failed to resolve a node module path
      }
    }
    return null
  }

  function resolveCandidatesForModulePath (
    ctx: ImportResolveCtx,
    modulePath: string,
    importerPath: string | null = null
  ): ResolvedPath[] {
    const importerDir = importerPath === null ? null : pathlib.dirname(importerPath)
    const relative = importerDir === null ? [] : [pathlib.join(importerDir, modulePath)]

    const isOnlyRelative = modulePath.startsWith('./') || modulePath.startsWith('../')
    const fromPaths = isOnlyRelative
      ? []
      : GlobPaths.resolve(ctx.absoluteImportSourcePaths, ctx.absoluteImportBasePath, modulePath)
    const fromNodeModules = isOnlyRelative || !ctx.resolveNodeModules
      ? []
      : resolveNodeModuleCandidates(modulePath)

    return [...relative, ...fromPaths, ...fromNodeModules]
  }

  function resolveNodeModuleCandidates (path: string): ResolvedNodeModulePath[] {
    const [nodeModule, ...remainder] = path.split('/')
    const remainderPath = remainder.join('/')
    return [{ nodeModule, remainderPath }]
  }
}

export module GlobPaths {
  export function resolve (globPaths: GlobPaths, basePath: string, path: string): string[] {
    const resolved: string[] = []
    for (const glob in globPaths) {
      if (minimatch(path, glob)) {
        resolved.push(...globPaths[glob].map(globPath => pathlib.join(basePath, globPath.replace('*', path))))
      }
    }
    return resolved
  }
}

 */