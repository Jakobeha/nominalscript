use std::collections::HashMap;
use std::path::Path;
use crate::semantic::ann::Ann;
use crate::semantic::scope::{Scope, TopLevelScope};

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
    pub fn add(&mut self, path: &'tree Path, ann: Ann<'tree>) {
        let None = self.files.insert(path, TopLevelScope::new(ann)) else {
            panic!("Semantic data already added at path: {}", path.display());
        };
    }

    /// Get toplevel scope at the specified path
    pub fn get(&self, path: &Path) -> Option<Scope<'tree>> {
        self.files.get(path).map(Scope::from)
    }
}