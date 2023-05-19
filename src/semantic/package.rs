use std::collections::HashMap;
use std::path::{Path, PathBuf};
use crate::syntax::nodes::ProgramTree;

/// Contains all semantic data in a NominalScript package
#[derive(Debug)]
pub struct SemanticPackage<'tree> {
    files: HashMap<&'tree Path, ToplevelScope<'tree>>
}

impl<'tree> SemanticPackage<'tree> {
    /// Create an empty package
    pub fn new() -> Self {
        SemanticPackage {
            files: HashMap::new()
        }
    }

    /// Add empty toplevel scope to a path. **Panics** if already added
    pub fn add(&mut self, path: &'tree Path) {
        let None = self.files.insert(path, TopLevelScope::new()) else {
            panic!("Semantic data already added at path: {}", path.display());
        };
    }

    /// Get toplevel scope at the specified path
    pub fn get(&self, path: &Path) -> Option<&TopLevelScope<'tree>> {
        self.files.get(path)
    }

    /// Get toplevel scope at the specified path (mutable)
    pub fn get_mut(&mut self, path: &Path) -> Option<&mut TopLevelScope<'tree>> {
        self.files.get_mut(path)
    }
}