use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fs::{copy, create_dir, create_dir_all, remove_dir_all};
use std::iter::zip;
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::process::exit;
use std::thread::park;

use notify_debouncer_mini::{DebouncedEvent, DebounceEventHandler, DebounceEventResult, new_debouncer};
use notify_debouncer_mini::notify::{RecommendedWatcher, RecursiveMode, Watcher};
use rayon::iter::IntoParallelRefMutIterator;
use walkdir::WalkDir;

use crate::compiler::output::{FatalError, Output};
use crate::misc::{PathPatriciaMap, ResultFilterErr};
use crate::output::{FatalError, Output};

mod output;

pub struct Compiler {
    packages: BTreeMap<PathBuf, PackageCompiler>,
}

pub struct IncrementalCompiler {
    compiler: Compiler,
    debouncer: Debouncer,
}

struct PackageCompiler {
    path: PathBuf,
    latest: Option<Package>,
}

struct RecompileEventHandler(*mut IncrementalCompiler);

type Debouncer = notify_debouncer_mini::Debouncer<RecommendedWatcher>;

const WATCH_TIMEOUT: std::time::Duration = std::time::Duration::from_millis(100);
const SRC_DIR_NAMES: [&'static str; 3] = ["src", "bin", "tests"];
const OUR_DIR_NAME: &'static str = "out";
const NOMINALSCRIPT_EXTENSION: &'static str = "ns";

impl Compiler {
    pub fn run(package_paths: impl IntoIterator<Item=PathBuf>) -> ! {
        exit(Self::try_new(package_paths).unwrap_or_else(FatalError::exit).run_batch())
    }

    pub fn try_new(package_paths: impl IntoIterator<Item=PathBuf>) -> Result<Self, FatalError> {
        Ok(Self {
            packages: package_paths.into_iter().map(|path| {
                PackageCompiler::try_new(path.clone()).map(|package| (path, package))
            }).collect()?
        })
    }

    /// Compile all packages fully from scratch, and return an exit code if the process should exit
    fn run_batch(&mut self) -> i32 {
        self.packages.par_iter_mut()
            .map(|(_, package)| package.run_batch())
            .fold(Output::new(), |a, b| a + b).report()
    }
}

impl IncrementalCompiler {
    pub fn run(package_paths: impl IntoIterator<Item=PathBuf>) -> ! {
        Self::try_run(package_paths).unwrap_or_else(FatalError::exit)
    }

    fn try_run(package_paths: impl IntoIterator<Item=PathBuf>) -> Result<!, FatalError> {
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
        for package in &compiler.packages {
            debouncer.watcher().watch(package.path(), RecursiveMode::Recursive)?;
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
                events.sort_by_key(|event| &event.path);
                fn event_package<'a>(
                    event: &DebouncedEvent,
                    ptr: &'a mut IncrementalCompiler
                ) -> (&'a Path, &'a mut PackageCompiler) {
                    let (path, compiler) = ptr.compiler.packages
                        .range_mut(..&event.path)
                        .next_back()
                        .expect("File watcher returned event for unknown path");
                    // Since it's not obvious, the path should be the prefix because of
                    // lexicographic order
                    assert!(path.starts_with(&event.path));
                    (path, compiler)
                }
                // ```
                // for events in events.linear_group_by_key(|event| event_package(event, ptr)) {
                //     event_package(&events[0], ptr).1.recompile(events)
                // }
                // ```
                // But satisfies the borrow checker and only calls event_path once
                let mut current_package = None;
                let mut current_idx = 0;
                for idx in 0..events.len() {
                    let event = &events[idx];
                    let package = event_package(event, ptr).1;
                    if Some(package) != current_package {
                        current_package = Some(package);
                        package.recompile(&events[current_idx..idx]);
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
        if let Err(err) = create_dir(output_root) {
            eprintln!("Error creating new output root directory before batch compilation ({}):\n  {}", output_root.display(), err);
            output.num_errors += 1;
            return output
        }

        // Get paths AND copy non-nominalscript files
        let paths = SRC_DIR_NAMES.iter()
            .map(|dir_name| self.path.join(dir_name))
            .filter(|path| path.exists())
            .flat_map(|root_dir| WalkDir::new(&root_dir)
                .follow_links(true)
                .into_iter()
                .map(|e| (root_dir, e)))
            .filter_map(|(root_dir, entry)| match entry {
                Err(err) => {
                    eprintln!("Error processing file in {}:\n  {}", root_dir.display(), err);
                    output.num_errors += 1;
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
                    if let Err(err) = create_dir(output_path) {
                        eprintln!("Error creating output subdirectory at {}:\n  {}", output_path.display(), err);
                        output.num_errors += 1;
                    }
                    None
                } else if path.extension() != Some(OsStr::new(NOMINALSCRIPT_EXTENSION)) {
                    if let Err(err) = copy(path, output_path) {
                        eprintln!("Error copying non-nominalscript file at {}:\n  {}", output_path.display(), err);
                        output.num_errors += 1;
                    }
                    None
                } else {
                    Some((path, output_path))
                }
            })
            .collect::<PathPatriciaMap<PathBuf>>();

        // Build package with input files
        let mut package = Package::build(paths.keys(), &mut output);

        // Write output files
        for ((path, output), (path2, output_path)) in zip(package.outputs, paths) {
            assert_eq!(path, path2, "Path output path tree is different (maybe has different order) than input path tree");
            if let Err(err) = output.write_to_file(output_path) {
                eprintln!("Error writing output file to {}:\n  {}", output_path.display(), err);
                output.num_errors += 1;
            }
        }

        // Done
        self.latest = package;
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

    use crate::compiler::{Compiler, PackageCompiler, run};

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
