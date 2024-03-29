#![doc = include_str!("../README.md")]

/// Static analysis and type system
pub mod analyses;
/// Diagnostics (errors/warnings/etc) and logging
pub mod diagnostics;
/// Typed AST and wrapper for tree-sitter
pub mod ast;
/// Import resolution, exports. Also includes the compiled output
pub mod import_export;
/// Project datastructure which contains everything
mod project;
/// Utilities which could go in any crate
pub mod misc;
/// This is the file with the one function (+ wrappers) which transpiles everything.
///
/// It really is just one massive function.
/// Maybe I will end up refactoring out a lot of the functionality like I've already done for the
/// type analysis and scope resolution, so it will be less massive.
/// But honestly, it's just a really straightforward algorithm, and has a lot of cases to handle
/// for all of JS/TS's different expressions, but all of the cases are really straightforward
/// (with the less straightforward stuff in those other modules)
pub mod compile;

use std::fs::{create_dir, create_dir_all};
use std::path::PathBuf;
use std::process::exit;
use clap::{arg, crate_authors, crate_description, crate_version, Command, value_parser};
use derive_more::{Display, Error, From};
use walkdir::WalkDir;
pub use project::*;
use crate::import_export::import_resolver::ImportResolverCreateError;
use crate::misc::ResultFilterErr;

/// Run the program
fn main() {
    let mut m = Command::new("NominalScript Transpiler")
        .author(crate_authors!())
        .version(crate_version!())
        .about("Transpiles a NominalScript package (transpiles from lib/, bin/, and tests/ into out/)")
        .arg(arg!(
            <package_path>
            "The project's path"
        ).value_parser(value_parser!(PathBuf)))
        .after_help(crate_description!())
        .get_matches();
    let package_path = m.remove_one::<PathBuf>("package_path").expect("arg was required");
    match run(package_path) {
        Ok(Output { num_warnings, num_errors }) => {
            if num_errors > 0 {
                if num_warnings > 0 {
                    eprintln!("Failed with {} errors, {} warnings", num_errors, num_warnings);
                } else {
                    eprintln!("Failed with {} errors", num_errors);
                }
                exit(1);
            } else if num_warnings > 0 {
                eprintln!("Succeeded but with {} warnings", num_warnings);
            } else {
                eprintln!("Succeeded");
            }
        },
        Err(error) => {
            eprintln!("Fatal error: {}", error);
            exit(2);
        }
    }
}

pub struct Output {
    num_warnings: usize,
    num_errors: usize
}

#[derive(Debug, Display, Error, From)]
pub enum FatalError {
    #[display(fmt = "Directory must be package root AKA have at least one of package.json or out/, and at least one of src/, lib/, bin/, or tests/")]
    InvalidPackageRoot,
    ImportResolverCreateError(ImportResolverCreateError),
    #[display(fmt = "When {}: {}", action, source)]
    IoError { action: String, #[error(source)] source: std::io::Error },
    #[display(fmt = "When traversing {}: {}", path_desc, source)]
    WalkDirError { path_desc: String, #[error(source)] source: walkdir::Error }
}

const SRC_DIR_NAMES: [&'static str; 4] = ["src", "lib", "bin", "tests"];
const TYPESCRIPT_OUT_DIR_NAME: &'static str = "out/typescript";

pub fn run(package_path: PathBuf) -> Result<Output, FatalError> {
    if (!package_path.join("out").is_dir() && !package_path.join("package.json").is_dir()) || !SRC_DIR_NAMES.iter().any(|dir_name| package_path.join(dir_name).is_dir()) {
        return Err(FatalError::InvalidPackageRoot);
    }

    let p = Project::regular(package_path.clone(), true)?;
    let out_dir = package_path.join(TYPESCRIPT_OUT_DIR_NAME);
    create_dir_all(&out_dir)
        .filter_err(|e| !matches!(e.kind(), std::io::ErrorKind::AlreadyExists))
        .map_err(|source| FatalError::IoError { action: format!("creating out dir ({})", out_dir.display()), source })?;
    for src_dir in SRC_DIR_NAMES.iter()
        .map(|dir_name| package_path.join(dir_name))
        .filter(|path| path.exists()) {
        for path in WalkDir::new(&src_dir).follow_links(true) {
            let path = path.map_err(|source| FatalError::WalkDirError { path_desc: format!("{}", src_dir.file_name().unwrap().to_string_lossy()), source })?;
            if path.file_type().is_file() {
                let rel_path = path.path().strip_prefix(&src_dir).unwrap();
                let mut out_path = out_dir.join(rel_path);
                out_path.set_extension("ts");
                let out_subdir = out_path.parent().unwrap();
                create_dir(out_subdir)
                    .filter_err(|e| !matches!(e.kind(), std::io::ErrorKind::AlreadyExists))
                    .map_err(|source| FatalError::IoError { action: format!("creating out sub-directory ({})", out_subdir.display()), source })?;
                let transpiled = match p.transpile_file(path.path()) {
                    Ok(transpiled) => transpiled,
                    Err(TranspileError::AlreadyTranspiled(_)) => {
                        eprintln!("compiler warning: traversed the same file twice! This is possible but only with bad symbolic links. Skipping...");
                        continue
                    }
                    Err(TranspileError::ImportError(_)) => {
                        // The ImportError already gets logged, and we want to skip, not abort,
                        // in order to parse the most we can
                        continue
                    }
                };
                std::fs::write(&out_path, transpiled).map_err(|source| FatalError::IoError { action: format!("writing transpiled file ({})", out_path.display()), source })?;
            }
        }
    }
    Ok(Output {
        num_errors: p.diagnostics.count_errors(),
        num_warnings: p.diagnostics.count_warnings()
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use test_log::test;
    use crate::run;

    #[test]
    pub fn test_a_project() {
        let mut package_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        package_path.push("test-resources/nominalscript-package");
        let output = run(package_path).expect("Fatal error");
        assert_eq!(output.num_errors, 0);
        assert_eq!(output.num_warnings, 0);
    }
}