use std::collections::HashMap;
use std::path::{Path, PathBuf};
use crate::syntax::nodes::ProgramTree;

/// Collection of syntax trees at their relative paths. Stores all syntax in a NominalScript package
#[derive(Debug)]
pub struct SyntaxPackage {
    files: HashMap<PathBuf, ProgramTree>
}

impl SyntaxPackage {
    /// Create an empty package
    pub fn new() -> Self {
        SyntaxPackage {
            files: HashMap::new()
        }
    }

    /// Add syntax to a specified path. **Panics** if already added at the path.
    pub fn add(&mut self, path: PathBuf, parsed: ProgramTree) {
        let None = self.files.insert(path, parsed) else {
            panic!("Syntax already added at path");
        };
    }

    /// Get parsed syntax at the specified path
    pub fn get(&self, path: &PathBuf) -> Option<&ProgramTree> {
        self.files.get(path)
    }

    /// Get parsed syntax at the specified path (mutable)
    pub fn get_mut(&mut self, path: &Path) -> Option<&mut ProgramTree> {
        self.files.get_mut(path)
    }
}