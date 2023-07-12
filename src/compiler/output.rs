use std::ops::Add;
use std::process::exit;
use thiserror::Error;

#[derive(Debug, Default)]
pub struct Output {
    pub num_warnings: usize,
    pub num_errors: usize
}

#[derive(Debug, Error)]
pub enum FatalError {
    #[error("Directory must be package root AKA have at least one of src/, bin/, or tests/")]
    InvalidPackageRoot,
    #[error("Package path must be valid UTF-8, sorry")]
    InvalidPackagePath,
    #[error("{0}")]
    NotifyError(#[from] notify_debouncer_mini::notify::Error),
    #[error("When {action}: {source}")]
    IoError { action: String, #[source] source: std::io::Error },
    #[error("When traversing {path_desc}: {source}")]
    WalkDirError { path_desc: String, #[source] source: walkdir::Error }
}

impl Output {
    pub fn new() -> Self {
        Self::default()
    }

    /// Print and return an exit code if we want to exit node
    pub fn report(self) -> i32 {
        let Output { num_warnings, num_errors } = self;
        if num_errors > 0 {
            if num_warnings > 0 {
                eprintln!("Failed with {} errors, {} warnings", num_errors, num_warnings);
            } else {
                eprintln!("Failed with {} errors", num_errors);
            }
            1
        } else if num_warnings > 0 {
            eprintln!("Succeeded but with {} warnings", num_warnings);
            0
        } else {
            eprintln!("Succeeded");
            0
        }
    }
}

impl Add for Output {
    type Output = Output;

    fn add(self, rhs: Self) -> Self::Output {
        Output {
            num_warnings: self.num_warnings + rhs.num_warnings,
            num_errors: self.num_errors + rhs.num_errors,
        }
    }
}

impl FatalError {
    pub fn exit(self) -> ! {
        eprintln!("Fatal error: {}", self);
        exit(2)
    }
}