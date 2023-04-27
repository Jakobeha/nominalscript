use std::cell::RefCell;
use std::collections::{btree_set, BTreeSet};
use std::env::VarError;
use std::fmt::{Debug, Display, Formatter};

use derive_more::Display;
use join_lazy_fmt::Join;
use elsa::FrozenMap;
use lazy_static::lazy_static;
use log::{debug, error};
use smallvec::SmallVec;

use crate::ast::tree_sitter::TSRange;
use crate::import_export::ModulePath;
use crate::misc::FrozenMapIter;

pub struct ProjectDiagnostics {
    /// Prints diagnostics immediately. We still have to store them because we allow loggers to make
    /// duplicates
    print_globals_immediately: bool,
    global: RefCell<BTreeSet<GlobalDiagnostic>>,
    by_file: FrozenMap<ModulePath, Box<FileDiagnostics>>,
}

#[derive(Debug)]
pub struct FileDiagnostics {
    /// Prints diagnostics immediately. We still have to store them because we allow loggers to make
    /// duplicates
    print_locals_immediately: bool,
    path: ModulePath,
    diagnostics: RefCell<BTreeSet<FileDiagnostic>>
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

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoggerLevel {
    DontLog,
    DoLog { level: DiagnosticLevel }
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

macro_rules! impl_count_level_shorthands {
    () => {
    pub fn count_errors(&self) -> usize {
        self.count_level(DiagnosticLevel::Error)
    }

    pub fn count_warnings(&self) -> usize {
        self.count_level(DiagnosticLevel::Warning)
    }

    pub fn count_infos(&self) -> usize {
        self.count_level(DiagnosticLevel::Info)
    }

    pub fn count_debugs(&self) -> usize {
        self.count_level(DiagnosticLevel::Debug)
    }
    }
}

impl ProjectDiagnostics {
    pub fn new(print_immediately: bool) -> Self {
        Self {
            print_globals_immediately: print_immediately,
            global: RefCell::new(BTreeSet::new()),
            by_file: FrozenMap::new(),
        }
    }

    pub fn file(&self, path: &ModulePath) -> &FileDiagnostics {
        if let Some(file) = self.by_file.get(path) {
            return file
        }
        let print_immediately = self.print_globals_immediately;
        self.by_file.insert(path.clone(), Box::new(FileDiagnostics::new(path.clone(), print_immediately)))
    }

    pub fn insert_global(&self, diagnostic: GlobalDiagnostic) {
        let mut global = self.global.borrow_mut();
        if let Some(existing) = global.get(&diagnostic) {
            debug!("Duplicate diagnostic was replaced: {}", existing);
        }
        if self.print_globals_immediately && RUST_LOG.logs(diagnostic.level) {
            eprintln!("{}", diagnostic);
        }
        global.insert(diagnostic);
    }

    /// Iterator requires an owned reference because we allow insertion behind shared references
    pub fn iter_global(&mut self) -> impl Iterator<Item=&GlobalDiagnostic> {
        self.global.get_mut().iter()
    }

    /// Prints diagnostics immediately. We still have to store them because we allow loggers to make
    /// duplicates
    pub fn set_print_immediately(&mut self, print_immediately: bool) {
        self.print_globals_immediately = print_immediately;
        for file in self.by_file.as_mut().values_mut() {
            file.print_locals_immediately = print_immediately;
        }
    }

    pub fn count_level(&self, level: DiagnosticLevel) -> usize {
        self.global.borrow().iter().filter(|d| d.level == level).count() +
            FrozenMapIter::new(&self.by_file).map(|(_, fds)| fds.count_level(level)).sum::<usize>()
    }

    impl_count_level_shorthands!();
}

impl FileDiagnostics {
    pub fn new(path: ModulePath, print_immediately: bool) -> Self {
        Self {
            path,
            print_locals_immediately: print_immediately,
            diagnostics: RefCell::new(BTreeSet::new()),
        }
    }

    pub fn insert(&self, diagnostic: FileDiagnostic) {
        let mut diagnostics = self.diagnostics.borrow_mut();
        if let Some(existing) = diagnostics.get(&diagnostic) {
            debug!("Duplicate diagnostic was replaced: {}", existing);
        }
        if self.print_locals_immediately && RUST_LOG.logs(diagnostic.level) {
            eprintln!("{}:{}", self.path.path().display(), diagnostic);
        }
        diagnostics.insert(diagnostic);
    }

    /// Iterator requires an owned reference because we allow insertion behind shared references
    pub fn iter(&mut self) -> impl Iterator<Item=&FileDiagnostic> {
        self.diagnostics.get_mut().iter()
    }

    pub fn count_level(&self, level: DiagnosticLevel) -> usize {
        self.diagnostics.borrow().iter().filter(|d| d.level == level).count()
    }

    impl_count_level_shorthands!();

}

impl<'a> IntoIterator for &'a mut FileDiagnostics {
    type Item = &'a FileDiagnostic;
    type IntoIter = btree_set::Iter<'a, FileDiagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.get_mut().iter()
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

lazy_static! {
    pub static ref RUST_LOG: LoggerLevel = LoggerLevel::get_rust_log();
}

impl LoggerLevel {
    pub fn logs(&self, level: DiagnosticLevel) -> bool {
        match self {
            LoggerLevel::DontLog => false,
            LoggerLevel::DoLog { level: log_level } => *log_level >= level
        }
    }

    const VALID_RUST_LOG_VALUES: [(&'static str, LoggerLevel); 17] = [
        ("off", LoggerLevel::DontLog),
        ("none", LoggerLevel::DontLog),
        ("error", LoggerLevel::DoLog { level: DiagnosticLevel::Error }),
        ("warn", LoggerLevel::DoLog { level: DiagnosticLevel::Warning }),
        ("info", LoggerLevel::DoLog { level: DiagnosticLevel::Info }),
        ("debug", LoggerLevel::DoLog { level: DiagnosticLevel::Debug }),
        ("Off", LoggerLevel::DontLog),
        ("None", LoggerLevel::DontLog),
        ("Error", LoggerLevel::DoLog { level: DiagnosticLevel::Error }),
        ("Warn", LoggerLevel::DoLog { level: DiagnosticLevel::Warning }),
        ("Info", LoggerLevel::DoLog { level: DiagnosticLevel::Info }),
        ("Debug", LoggerLevel::DoLog { level: DiagnosticLevel::Debug }),
        ("0", LoggerLevel::DontLog),
        ("1", LoggerLevel::DoLog { level: DiagnosticLevel::Error }),
        ("2", LoggerLevel::DoLog { level: DiagnosticLevel::Warning }),
        ("3", LoggerLevel::DoLog { level: DiagnosticLevel::Info }),
        ("4", LoggerLevel::DoLog { level: DiagnosticLevel::Debug }),
    ];

    fn get_rust_log() -> LoggerLevel {
        match std::env::var("RUST_LOG") {
            Ok(level) => {
                match Self::VALID_RUST_LOG_VALUES.iter().find(|(str, _)| level.as_str() == *str) {
                    None => {
                        error!(
                        "Invalid RUST_LOG value: {}.\nValid ones are: [{}]",
                        level,
                        ", ".join(Self::VALID_RUST_LOG_VALUES.iter().map(|(str, _)| str))
                    );
                        LoggerLevel::default()
                    }
                    Some((_, level)) => *level
                }
            },
            Err(VarError::NotPresent) => LoggerLevel::default(),
            Err(VarError::NotUnicode(str)) => {
                error!("Invalid RUST_LOG value: not unicode ({})", str.to_string_lossy());
                LoggerLevel::default()
            }
        }
    }
}

impl Default for LoggerLevel {
    fn default() -> Self {
        LoggerLevel::DoLog { level: DiagnosticLevel::Info }
    }
}

// region display
impl Display for ProjectDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let global_borrow = self.global.borrow();
        for diagnostic in &*global_borrow {
            writeln!(f, "{}", diagnostic)?;
        }
        for (_, diagnostics) in FrozenMapIter::new(&self.by_file) {
            let diagnostics_borrow = diagnostics.diagnostics.borrow();
            for diagnostic in &*diagnostics_borrow {
                write!(f, "{}:", diagnostics.path.path().display())?;
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
        write!(f, "{} {}: {}", self.location, self.level, self.message)?;
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
            write!(f, " (at {})", loc)?;
        }
        write!(f, ": {}", self.message)?;
        Ok(())
    }
}
// endregion

// region eq ord
impl PartialEq<GlobalDiagnostic> for GlobalDiagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level
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

impl Debug for ProjectDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProjectDiagnostics")
            .field("global", &self.global)
            .field("by_file", &"...")
            .finish()
    }
}