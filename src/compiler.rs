use std::cell::Cell;
use std::collections::BTreeMap;
use std::convert::Infallible;
use std::ffi::OsStr;
use std::fs::{copy, create_dir, create_dir_all, remove_dir_all};
use std::iter::zip;
use std::mem::MaybeUninit;
use std::ops::Bound;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::thread::park;

use camino::Utf8Path;
use join_lazy_fmt::Join;
use notify_debouncer_mini::{DebouncedEvent, DebounceEventHandler, DebounceEventResult, new_debouncer};
use notify_debouncer_mini::notify::{RecommendedWatcher, RecursiveMode};
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use rustc_arena_modified::TypedArena;
use walkdir::WalkDir;

use crate::compiler::output::FatalError;
pub use crate::compiler::output::Output;
use crate::misc::{PathPatriciaMap, ResultFilterErr};
use crate::package::Package;

mod output;

pub type Never = Infallible;

pub struct Compiler {
    packages: BTreeMap<PathBuf, PackageCompiler>,
}

pub struct IncrementalCompiler {
    compiler: Compiler,
    // debouncer needs to be kept alive while [IncrementalCompiler] is, but it's not actually used
    #[allow(unused)]
    debouncer: Debouncer,
}

struct PackageCompiler {
    path: PathBuf,
    latest: Option<Package>,
}

struct RecompileEventHandler(*mut IncrementalCompiler);

unsafe impl Send for RecompileEventHandler {}

type Debouncer = notify_debouncer_mini::Debouncer<RecommendedWatcher>;

const WATCH_TIMEOUT: std::time::Duration = std::time::Duration::from_millis(100);
const SRC_DIR_NAMES: [&'static str; 3] = ["src", "bin", "tests"];
const OUT_DIR_NAME: &'static str = "out";
const NOMINALSCRIPT_EXTENSION: &'static str = "ns";

impl Compiler {
    pub fn run(package_paths: impl IntoIterator<Item=PathBuf>) -> ! {
        exit(Self::try_new(package_paths).unwrap_or_else(|err| err.exit()).run_batch())
    }

    pub fn try_new(package_paths: impl IntoIterator<Item=PathBuf>) -> Result<Self, FatalError> {
        Ok(Self {
            packages: package_paths.into_iter().map(|path| {
                PackageCompiler::try_new(path.clone()).map(|package| (path, package))
            }).collect::<Result<BTreeMap<PathBuf, PackageCompiler>, FatalError>>()?
        })
    }

    /// Compile all packages fully from scratch, and return an exit code if the process should exit
    fn run_batch(&mut self) -> i32 {
        self.packages.par_iter_mut()
            .map(|(_, package)| package.run_batch())
            .reduce(Output::new, |a, b| a + b)
            .report()
    }
}

impl IncrementalCompiler {
    pub fn run(package_paths: impl IntoIterator<Item=PathBuf>) -> ! {
        match Self::try_run(package_paths) {
            Ok(never) => match never {},
            Err(err) => err.exit()
        }
    }

    fn try_run(package_paths: impl IntoIterator<Item=PathBuf>) -> Result<Never, FatalError> {
        // Create a 'static pointer. Why leak? Because we're going to run this for the rest of the
        // program, there's no "exit and do something else"
        let ptr = Box::leak::<'static>(Box::new(MaybeUninit::<IncrementalCompiler>::uninit()));
        // Create the compiler before we do things which require us to manually drop
        let mut compiler = Compiler::try_new(package_paths)?;
        // Start by running a batch compilation
        compiler.run_batch();
        // Initialize the debouncer and report errors before we write droppable data.
        let mut debouncer = new_debouncer(
            WATCH_TIMEOUT,
            None,
            RecompileEventHandler(ptr.as_mut_ptr())
        )?;
        // Have the debouncer watch for changes in package sources
        for path in compiler.packages.keys() {
            debouncer.watcher().watch(path, RecursiveMode::Recursive)?;
        }
        // Of course, if either of the above fail we leak memory, but we're leaking anyways
        // Write fields and forget. Afterward, RecompileEventHandler can get a mutable reference
        // to the data we just wrote
        ptr.write(IncrementalCompiler {
            compiler,
            debouncer
        });
        loop {
            park();
        }
    }
}

impl DebounceEventHandler for RecompileEventHandler {
    fn handle_event(&mut self, event: DebounceEventResult) {
        // SAFETY: This method is only called by the Debouncer which is created and stored within
        // the RecompileEventHandler. The pointed-to IncrementalCompiler has a static lifetime. We
        // also assume it's not called multiple times on separate threads AND not called before the
        // pointed-to IncrementalCompiler is contstructed. If so, then this is the only reference
        // and it's live, hence we can use
        let ptr = unsafe { &mut *self.0 };
        match event {
            Ok(mut events) => {
                events.sort_by(|lhs, rhs| lhs.path.cmp(&rhs.path));
                // We have to return references here because we are storing the previous package
                // compiler in `current_package` while we get the next one. Since we can't have a
                // mutable reference and immutable reference which overlap exist at the same time,
                // we return and store in `current_package` a mutable pointer instead of a mutable
                // reference. We also need the borrow to `ptr` to end before we would dereference
                // `current_package`, so we also return and store in `current_path` a const pointer
                // instead of a shared reference.
                fn event_package(
                    event: &DebouncedEvent,
                    ptr: &IncrementalCompiler
                ) -> (*const Path, *mut PackageCompiler) {
                    let (path, compiler) = ptr.compiler.packages
                        // See https://stackoverflow.com/questions/66130661/why-impl-rangeboundst-for-ranget-requires-t-to-be-sized and
                        // https://github.com/rust-lang/rust/pull/64327 for why we don't use `..&event.path` or `..&*event.path` here
                        .range::<Path, _>((Bound::Unbounded, Bound::Excluded(&*event.path)))
                        .next_back()
                        .expect("File watcher returned event for unknown path");
                    // Since it's not obvious, the path should be the prefix because of
                    // lexicographic order
                    assert!(path.starts_with(&event.path));
                    (path.as_path() as *const _, compiler as *const _ as *mut _)
                }
                // ```
                // for events in events.linear_group_by_key(|event| event_package(event, ptr)) {
                //     event_package(&events[0], ptr).1.recompile(events)
                // }
                // ```
                // But satisfies the borrow checker and only calls event_path once
                let (mut current_path, mut current_package) = (None::<*const Path>, None::<*mut PackageCompiler>);
                let mut current_idx = 0;
                for idx in 0..events.len() {
                    let event = &events[idx];
                    let (path, package) = event_package(event, ptr);
                    // SAFETY: `path` and `package` are live
                    if Some(path) != current_path {
                        if let Some(old_package) = current_package {
                            // SAFETY:
                            // - `old_package` is live
                            //   - `ptr.compiler.packages` is live
                            //   - we haven't inserted or removed elements since we retrieved, and
                            //     [BTreeMap] has stable deref
                            // - We have no other live references to data in `old_package`
                            //   - Importantly, we have no live shared references to `ptr`, because
                            //     `current_path` and `current_package` are both raw pointers.
                            unsafe { &mut *old_package }.recompile(&events[current_idx..idx]);
                        }
                        current_path = Some(path);
                        current_package = Some(package);
                        current_idx = idx;
                    }
                }
            }
            Err(event_errors) => {
                log::warn!("File watcher errors: {}", ", ".join(event_errors));
            }
        }
    }
}

impl PackageCompiler {
    pub fn try_new(path: PathBuf) -> Result<Self, FatalError> {
        if !SRC_DIR_NAMES.iter().any(|dir_name| path.join(dir_name).is_dir()) {
            return Err(FatalError::InvalidPackageRoot);
        }
        // Ensure that we can actually write by creating the out dir. Package will create the respective
        // subdirectories
        let out_dir = path.join(OUT_DIR_NAME);
        create_dir_all(&out_dir)
            .filter_err(|e| !matches!(e.kind(), std::io::ErrorKind::AlreadyExists))
            .map_err(|source| FatalError::IoError { action: format!("creating out dir ({})", out_dir.display()), source })?;
        Ok(Self {
            path,
            latest: None
        })
    }

    /// Compile the package fully from scratch, and return the number of errors and warnings
    pub fn run_batch(&mut self) -> Output {
        let mut output = Output::new();

        // Remove old outputs
        let output_root = self.path.join(OUT_DIR_NAME);
        if let Err(err) = remove_dir_all(&output_root) {
            eprintln!("Error removing old output root directory before batch compilation ({}):\n  {}", output_root.display(), err);
            output.num_errors += 1;
            return output
        }
        if let Err(err) = create_dir(&output_root) {
            eprintln!("Error creating new output root directory before batch compilation ({}):\n  {}", output_root.display(), err);
            output.num_errors += 1;
            return output
        }

        // Get paths AND copy non-nominalscript files
        let tmp_root_dir_owner = TypedArena::new();
        let num_errors = Cell::new(output.num_errors);
        let paths = SRC_DIR_NAMES.iter()
            .map(|dir_name| self.path.join(dir_name))
            .filter(|path| path.exists())
            .flat_map(|root_dir| {
                let root_dir = tmp_root_dir_owner.alloc(root_dir);
                WalkDir::new(root_dir)
                    .follow_links(true)
                    .into_iter()
                    .map(move |e| (root_dir, e))
            })
            .filter_map(|(root_dir, entry)| match entry {
                Err(err) => {
                    eprintln!("Error processing file in {}:\n  {}", root_dir.display(), err);
                    num_errors.set(num_errors.get() + 1);
                    None
                }
                Ok(entry) => Some((root_dir.clone(), entry))
            })
            .filter_map(|(root_dir, entry)| {
                let path = entry.path();
                let relative_path = path.strip_prefix(&root_dir).unwrap();
                let output_path = {
                    let mut output_path = root_dir;
                    output_path.push(OUT_DIR_NAME);
                    output_path.push(relative_path);
                    output_path
                };
                if entry.file_type().is_dir() {
                    if let Err(err) = create_dir(&output_path) {
                        eprintln!("Error creating output subdirectory at {}:\n  {}", output_path.display(), err);
                        num_errors.set(num_errors.get() + 1);
                    }
                    None
                } else if path.extension() != Some(OsStr::new(NOMINALSCRIPT_EXTENSION)) {
                    if let Err(err) = copy(&path, &output_path) {
                        eprintln!("Error copying non-nominalscript file from {} to {}:\n  {}", path.display(), output_path.display(), err);
                        num_errors.set(num_errors.get() + 1);
                    }
                    None
                } else {
                    let Some(path) = Utf8Path::from_path(&path) else {
                        eprintln!("Error: path is not utf-8, so nominalscript file will be skipped: {}", path.display());
                        num_errors.set(num_errors.get() + 1);
                        return None
                    };
                    Some((path.to_path_buf(), output_path))
                }
            })
            .collect::<PathPatriciaMap<PathBuf>>();
        output.num_errors = num_errors.into_inner();

        // Build package with input files
        let package = Package::build(paths.keys(), &mut output);

        // Write output files
        for ((path, file_output), (path2, file_output_path)) in zip(package.outputs(), paths) {
            assert_eq!(path, path2, "Path output path tree is different (maybe has different order) than input path tree");
            if let Err(err) = file_output.write_to_file(&file_output_path) {
                eprintln!("Error writing output file to {}:\n  {}", file_output_path.display(), err);
                output.num_errors += 1;
            }
        }

        // Done
        self.latest = Some(package);
        output
    }

    /// Recompile according to the given events
    pub fn recompile(&mut self, events: &[DebouncedEvent]) {
        todo!("Recompile with events: {:?}", events)
    }
}

#[cfg(test)]
mod tests {
    use std::iter::once;
    use std::path::PathBuf;

    use test_log::test;

    use crate::compiler::{Compiler, PackageCompiler};

    /// Test the batch compiler
    #[test]
    pub fn test_batch_compile() {
        let mut compiler = compiler();
        let exit_code = compiler.run_batch();
        assert_eq!(exit_code, 0);
        assert_eq!(exit_code, 0);
    }

    /// Test batch and incremental via package compiler
    #[test]
    pub fn test_package_compile() {
        let mut compiler = package_compiler();
        let output = compiler.run_batch();
        assert_eq!(output.num_errors, 0);
        assert_eq!(output.num_warnings, 0);
        // TODO: Incremental recompile
    }

    fn compiler() -> Compiler {
        Compiler::try_new(once(package_path())).expect("Fatal error")
    }

    fn package_compiler() -> PackageCompiler {
        PackageCompiler::try_new(package_path()).expect("Fatal error")
    }

    fn package_path() -> PathBuf {
        let mut package_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        package_path.push("test-resources/nominalscript-package");
        package_path
    }
}
