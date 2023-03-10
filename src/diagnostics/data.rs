use std::collections::{btree_set, BTreeSet, HashMap};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use log::debug;
use smallvec::SmallVec;
use crate::ast::tree_sitter::TSRange;
use derive_more::Display;

#[derive(Debug)]
pub struct ProjectDiagnostics {
    global: BTreeSet<GlobalDiagnostic>,
    by_file: HashMap<PathBuf, FileDiagnostics>,
}

#[derive(Debug)]
pub struct FileDiagnostics {
    diagnostics: BTreeSet<FileDiagnostic>
}

#[derive(Debug, Clone)]
pub struct GlobalDiagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub additional_info: SmallVec<[AdditionalInfo; 4]>
}

#[derive(Debug, Clone)]
pub struct FileDiagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub additional_info: SmallVec<[AdditionalInfo; 4]>,
    pub location: TSRange
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    #[display(fmt = "error")]
    Error,
    #[display(fmt = "warning")]
    Warning,
    #[display(fmt = "info")]
    Info,
    #[display(fmt = "debug")]
    Debug
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdditionalInfo {
    pub type_: AdditionalInfoType,
    pub message: String,
    pub location: Option<TSRange>
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AdditionalInfoType {
    #[display(fmt = "issue")]
    Issue,
    #[display(fmt = "hint")]
    Hint,
    #[display(fmt = "note")]
    Note,
}

impl ProjectDiagnostics {
    pub fn new() -> Self {
        Self {
            global: BTreeSet::new(),
            by_file: HashMap::new(),
        }
    }

    pub fn insert_global(&mut self, diagnostic: GlobalDiagnostic) {
        let did_insert = self.global.insert(diagnostic);
        if !did_insert {
            debug!("Duplicate diagnostic: {}", diagnostic);
        }
    }

    pub fn iter_global(&self) -> impl Iterator<Item=&GlobalDiagnostic> {
        self.global.iter()
    }

    pub fn file(&mut self, path: impl AsRef<Path>) -> &mut FileDiagnostics {
        self.by_file.entry(path).or_default()
    }
}

impl FileDiagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: BTreeSet::new(),
        }
    }

    pub fn insert(&mut self, diagnostic: FileDiagnostic) {
        let did_insert = self.diagnostics.insert(diagnostic);
        if !did_insert {
            debug!("Duplicate diagnostic: {}", diagnostic);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item=&FileDiagnostic> {
        self.diagnostics.iter()
    }
}

impl<'a> IntoIterator for &'a FileDiagnostics {
    type Item = FileDiagnostic;
    type IntoIter = btree_set::Iter<'a, Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.iter()
    }
}

impl FileDiagnostic {
    pub fn from_global(global: GlobalDiagnostic, location: TSRange) -> Self {
        Self {
            level: global.level,
            message: global.message,
            additional_info: global.additional_info,
            location
        }
    }

    pub fn add_info(&mut self, info: impl Iterator<Item=AdditionalInfo>) {
        self.additional_info.extend(info);
    }
}

impl GlobalDiagnostic {
    pub fn add_info(&mut self, info: impl Iterator<Item=AdditionalInfo>) {
        self.additional_info.extend(info);
    }
}

// region display
impl Display for ProjectDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for diagnostic in &self.global {
            writeln!(f, "{}", diagnostic)?;
        }
        for (path, diagnostics) in &self.by_file {
            for diagnostic in diagnostics {
                write!(f, "{}:", path.display())?;
                writeln!(f, "{}", diagnostic)?;
            }
        }
        Ok(())
    }
}

impl Display for GlobalDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.level, self.message)?;
        for info in &self.additional_info {
            write!(f, "\n  {}", info)?;
        }
        Ok(())
    }
}

impl Display for FileDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{} {}: {}", self.location.start_point, self.location.end_point, self.level, self.message)?;
        for info in &self.additional_info {
            write!(f, "\n  {}", info)?;
        }
        Ok(())
    }
}

impl Display for AdditionalInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)?;
        if let Some(loc) = self.location {
            write!(f, " (at {}-{})", loc.start_point, loc.end_point)?;
        }
        write!(f, ": {}", self.message)?;
        Ok(())
    }
}
// endregion

// region eq ord
impl PartialEq<GlobalDiagnostic> for GlobalDiagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
            && self.level == other.level
            && self.message == other.message
    }
}

impl Eq for GlobalDiagnostic {}

impl PartialOrd<GlobalDiagnostic> for GlobalDiagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GlobalDiagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.level.cmp(&other.level)
            .then(self.message.cmp(&other.message))
    }
}

impl PartialEq<FileDiagnostic> for FileDiagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
            && self.level == other.level
            && self.message == other.message
    }
}

impl Eq for FileDiagnostic {}

impl PartialOrd<FileDiagnostic> for FileDiagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FileDiagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.location.cmp(&other.location)
            .then(self.level.cmp(&other.level))
            .then(self.message.cmp(&other.message))
    }
}
// endregion