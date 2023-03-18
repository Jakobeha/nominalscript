use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::once;
use std::path::{Path, PathBuf};

use derive_more::{Display, Error, From};
use globset::{Glob, GlobSet, GlobSetBuilder};
use nonempty::NonEmpty;
use tsconfig::TsConfig;

use crate::import_export::export::ImportPath;
use crate::misc::{chain, mk_path};

/// Module resolution strategy *and* source root.
/// Usually generated from `ImportResolveCtx.regular`,
/// which takes a module root path and parses the tsconfig to load the other info.
/// But it's also customizable, probably moreso than necessary.
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Default)]
pub struct GlobPaths {
    pub globs: GlobSet,
    pub glob_paths: Vec<NonEmpty<PathBuf>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedPath {
    Regular { path: PathBuf },
    NodeModule {
        node_module: String,
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

#[derive(Debug, Display, Error, From)]
pub enum ImportResolverCreateError {
    #[display(fmt = "Failed to parse tsconfig.json: {}", error)]
    TSConfigError { error: tsconfig::ConfigError },
    IOError { error: std::io::Error },
}

#[derive(Debug, Clone, Display, Error)]
pub struct ResolveFailure;

impl ImportResolver {
    pub fn regular(module_root_path: PathBuf) -> Result<ImportResolver, ImportResolverCreateError> {
        let mut tsconfig_path = module_root_path;
        tsconfig_path.push("tsconfig.json");
        let tsconfig = match TsConfig::parse_file(&tsconfig_path) {
            Ok(tsconfig) => Some(tsconfig),
            Err(tsconfig::ConfigError::CouldNotFindFile(io_error))
            if io_error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(ImportResolverCreateError::TSConfigError { error }),
        };
        let mut module_root_path = tsconfig_path;
        module_root_path.pop();

        let (paths, base_url, module_resolution) = tsconfig
            .and_then(|tsconfig| tsconfig.compiler_options)
            .map(|compiler_options| (compiler_options.paths, compiler_options.base_url, compiler_options.module_resolution))
            .unwrap_or_default();
        Ok(ImportResolver {
            resolve_node_modules: module_resolution.as_ref()
                .filter(|module_resolution| matches!(module_resolution, tsconfig::ModuleResolutionMode::Node))
                .is_some(),
            absolute_import_source_paths: paths
                .map(GlobPaths::from)
                .unwrap_or_default(),
            absolute_import_base_path: module_root_path.join(base_url.unwrap_or_default()),
            module_root_path,
        })
    }

    pub fn locate(
        &self,
        module_path: &ImportPath,
        importer_path: Option<&Path>
    ) -> Result<ResolvedFatPath, ResolveFailure> {
        let candidates = self.resolve_candidates_for_module_path(module_path, importer_path);
        self.locate_from_resolved_candidates(candidates)
    }

    /// Resolves declaration files to get the fat version of `script_path`.
    pub fn fat_script_path(
        &self,
        script_path: &Path
    ) -> Result<ResolvedFatPath, ResolveFailure> {
        self.locate_from_resolved_candidates(once(ResolvedPath::Regular {
            path: script_path.with_extension("")
        }))
    }

    fn locate_from_resolved_candidates(
        &self,
        candidates: impl Iterator<Item=ResolvedPath>
    ) -> Result<ResolvedFatPath, ResolveFailure> {
        for candidate in candidates {
            match candidate {
                ResolvedPath::Regular { path } => {
                    if let Ok(fat_path) = Self::locate_from_regular_candidate(path) {
                        return Ok(fat_path);
                    }
                },
                ResolvedPath::NodeModule { node_module, remainder_path } => {
                    let node_module_path = mk_path!(&self.module_root_path, "node_modules", node_module);
                    let d_ns_path = if_exists(mk_path!(&node_module_path, "out", "nominal", "lib", remainder_path).with_extension("d.ns"));
                    let d_ts_path = if_exists(mk_path!(&node_module_path, "out", "types", "lib", remainder_path).with_extension("d.ts"));
                    let js_path = if_exists(mk_path!(&node_module_path, "out", "lib", remainder_path).with_extension("js"));
                    let arbitrary_path = if js_path.is_some() {
                        None
                    } else {
                        if_exists(mk_path!(&node_module_path, "out", "resources", remainder_path))
                    };
                    if d_ns_path.is_none() || d_ts_path.is_none() || js_path.is_none() || arbitrary_path.is_none() {
                        return Ok(ResolvedFatPath {
                            nominalscript_path: d_ns_path,
                            typescript_path: d_ts_path,
                            javascript_path: js_path,
                            arbitrary_path,
                        });
                    } else if let Ok(fat_path) = Self::locate_from_regular_candidate(mk_path!(node_module_path, "lib", remainder_path)) {
                        return Ok(fat_path);
                    }
                }
            }
        }
        Err(ResolveFailure)
    }

    fn locate_from_regular_candidate(path: PathBuf) -> Result<ResolvedFatPath, ResolveFailure> {
        let d_ns_path = if_exists(path.with_extension("d.ns"));
        let d_ts_path = if_exists(path.with_extension("d.ts"));
        if let Some(extension) = path.extension().map(|extension| extension.to_str().unwrap_or("?")) {
            // resolve path directly, we don't want to e.g. resolve a .ts into a .js
            if path.exists() {
                return match extension {
                    "ns" => Ok(ResolvedFatPath::ns(path)),
                    "ts" => Ok(ResolvedFatPath::ts(d_ns_path, path)),
                    "js" => Ok(ResolvedFatPath::js(d_ns_path, d_ts_path, path)),
                    _ => Ok(ResolvedFatPath::arbitrary(d_ns_path, d_ts_path, path)),
                }
            }
            Err(ResolveFailure)
        } else {
            // try to infer extension
            if let Some(ns_path) = if_exists(path.with_extension("ns")) {
                return Ok(ResolvedFatPath::ns(ns_path))
            } else if let Some(ts_path) = if_exists(path.with_extension("ts")) {
                return Ok(ResolvedFatPath::ts(d_ns_path, ts_path))
            } else if let Some(js_path) = if_exists(path.with_extension("js")) {
                return Ok(ResolvedFatPath::js(d_ns_path, d_ts_path, js_path))
            }
            // TypeScript doesn't infer arbitrary extensions so neither do we
            Err(ResolveFailure)
        }
    }

    fn resolve_candidates_for_module_path<'a>(
        &'a self,
        module_path: &'a ImportPath,
        importer_path: Option<&'a Path>
    ) -> impl Iterator<Item=ResolvedPath> + 'a {
        let importer_dir = importer_path.and_then(|importer_path| importer_path.parent());
        let relative = importer_dir
            .map(|importer_dir| importer_dir.join(module_path))
            .map(|path| ResolvedPath::Regular { path });
        let is_only_relative = module_path.starts_with("./") || module_path.starts_with("../");
        let from_paths = if is_only_relative {
            None
        } else {
            Some(
                self.absolute_import_source_paths.resolve(&self.absolute_import_base_path, module_path)
                    .map(|path| ResolvedPath::Regular { path })
            )
        }.into_iter().flatten();
        let from_node_modules = if is_only_relative || !self.resolve_node_modules {
            None
        } else {
            Some(Self::resolve_node_module_candidates(module_path))
        }.into_iter().flatten();
        chain!(relative, from_paths, from_node_modules)
    }

    fn resolve_node_module_candidates(module_path: &ImportPath) -> impl Iterator<Item=ResolvedPath> {
        module_path.split_once("/").map(|(node_module, remainder_path)| {
            ResolvedPath::NodeModule {
                node_module: node_module.to_string(),
                remainder_path: remainder_path.to_string()
            }
        }).into_iter()
    }
}

impl GlobPaths {
    pub fn resolve<'a>(&'a self, base_path: &'a Path, path: &'a str) -> impl Iterator<Item=PathBuf> + 'a {
        self.globs.matches(path).into_iter().flat_map(move |match_idx| {
            self.glob_paths[match_idx].clone().map(|resolved_path| {
                mk_path!(base_path, resolved_path, path)
            })
        })
    }
}

impl From<HashMap<String, Vec<String>>> for GlobPaths {
    fn from(value: HashMap<String, Vec<String>>) -> Self {
        value.into_iter().collect()
    }
}

impl FromIterator<(String, Vec<String>)> for GlobPaths {
    fn from_iter<T: IntoIterator<Item=(String, Vec<String>)>>(iter: T) -> Self {
        iter.into_iter()
            .filter(|(_, paths)| !paths.is_empty())
            .filter_map(|(glob, paths)| match Glob::new(&glob) {
                Err(err) => {
                    log::warn!("Invalid glob pattern: {}", err);
                    None
                }
                Ok(glob) => Some((glob, paths)),
            })
            .map(|(glob, paths)| (glob, NonEmpty::from_vec(paths).unwrap().map(PathBuf::from)))
            .collect()
    }
}

impl FromIterator<(Glob, NonEmpty<PathBuf>)> for GlobPaths {
    fn from_iter<T: IntoIterator<Item=(Glob, NonEmpty<PathBuf>)>>(iter: T) -> Self {
        let mut globs = GlobSetBuilder::new();
        let mut glob_paths = Vec::new();
        for (glob, paths) in iter {
            globs.add(glob);
            glob_paths.push(paths);
        }
        let globs = match globs.build() {
            Err(err) => {
                log::warn!("Invalid glob pattern set: {}", err);
                GlobSet::empty()
            }
            Ok(globs) => globs,
        };
        Self {
            globs,
            glob_paths,
        }
    }
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
            if did_write { write!(f, ", ")?; }
            write!(f, "typescript @ {}", typescript_path.display())?;
            did_write = true;
        }
        if let Some(javascript_path) = self.javascript_path.as_ref() {
            if did_write { write!(f, ", ")?; }
            write!(f, "javascript @ {}", javascript_path.display())?;
            did_write = true;
        }
        if let Some(arbitrary_path) = self.arbitrary_path.as_ref() {
            if did_write { write!(f, ", ")?; }
            write!(f, "arbitrary @ {}", arbitrary_path.display())?;
            did_write = true;
        }
        if !did_write {
            write!(f, "null")?;
        }
        Ok(())
    }
}

fn if_exists(path: PathBuf) -> Option<PathBuf> {
    if path.exists() {
        Some(path)
    } else {
        None
    }
}