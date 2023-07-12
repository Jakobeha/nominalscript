use std::cell::{Ref, RefCell};
use std::collections::{BTreeMap, HashSet};
use std::env::VarError;
use std::fmt::{Debug, Display, Formatter};
use std::mem::take;
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;
use nonempty::NonEmpty;
use smallvec::SmallVec;
use streaming_iterator::StreamingIterator;
use typed_arena_nomut::Arena;
use yak_sitter::Range;

use crate::concrete::tree_sitter::TSRange;
use crate::misc::DisplayWithCtx;
use crate::semantic::arena::AnnArenaPhase;

/// Diagnostic logging phases, map 1-1 with [AnnArenaPhase].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticsPhase {
    /// Insert new diagnostics, and prints if `print_immediately` is set. Maps to [AnnArenaPhase::Insertion]
    Recording,
    /// Print collected diagnostics, unless `print_immediately` is set. Maps to [AnnArenaPhase::Retrieval]
    Retrieval,
    /// Remove diagnostics for nodes which have been removed. Maps to [AnnArenaPhase::Removal]
    Removal
}


/// Diagnostics which don't belong to any particular file, but the package itself
#[derive(Debug, Clone)]
pub struct PackageDiagnostics {
    /// If set, diagnostics are printed immediately after being recorded. Useful for debugging
    ///
    /// We still store all diagnostics because we allow loggers to make duplicates
    pub print_immediately: bool,
    /// Phase
    phase: DiagnosticsPhase,
    /// The diagnostics
    diagnostics: Arena<PackageDiagnostic>,
    /// Diagnostics already recorded
    already_recorded: RefCell<HashSet<*const PackageDiagnostic>>,
}

/// Diagnostics for a source file
#[derive(Debug)]
pub struct FileDiagnostics {
    /// If set, diagnostics are printed immediately after being recorded. Useful for debugging
    ///
    /// We still store all diagnostics because we allow loggers to make duplicates
    pub print_immediately: bool,
    /// File's path
    path: PathBuf,
    state: FileDiagnosticsState
}

#[derive(Default)]
enum FileDiagnosticsState {
    Recording {
        diagnostics: Arena<FileDiagnostic>,
        /// Diagnostics already recorded
        already_recorded: RefCell<HashSet<*const FileDiagnostic>>,
    },
    Retrieval {
        /// The diagnostics, in order of their ranges
        ordered_diagnostics: Vec<FileDiagnostic>,
    },
    Removal {
        /// The diagnostics, in order of their ranges, with `None` for removed ones
        diagnostics: Vec<Option<FileDiagnostic>>,
        /// Map of byte offsets to indices.
        ///
        /// At each offset, indices of diagnostics containing any points until the next entry's offset
        indices_at_point: BTreeMap<usize, NonEmpty<usize>>
    },
    #[default]
    Broken
}

#[derive(Debug, Clone)]
pub struct PackageDiagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub additional_info: SmallVec<[AdditionalInfo; 4]>
}

#[derive(Debug, Clone)]
pub struct FileDiagnostic {
    pub location: Range,
    pub level: DiagnosticLevel,
    pub message: String,
    pub additional_info: SmallVec<[AdditionalInfo; 4]>,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Debug
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoggerLevel {
    DontLog,
    DoLog { level: DiagnosticLevel }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdditionalInfo {
    pub location: Option<Range>,
    pub type_: AdditionalInfoType,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AdditionalInfoType {
    Issue,
    Hint,
    Note,
}

macro_rules! impl_count_level_shorthands {
    () => {
    /// Count how many "error" diagnostics were recorded
    pub fn count_errors(&self) -> usize {
        self.count_level(DiagnosticLevel::Error)
    }

    /// Count how many "warning" diagnostics were recorded
    pub fn count_warnings(&self) -> usize {
        self.count_level(DiagnosticLevel::Warning)
    }

    /// Count how many "info" diagnostics were recorded
    pub fn count_infos(&self) -> usize {
        self.count_level(DiagnosticLevel::Info)
    }

    /// Count how many "debug" diagnostics were recorded
    pub fn count_debugs(&self) -> usize {
        self.count_level(DiagnosticLevel::Debug)
    }
    }
}

impl PackageDiagnostics {
    /// Create a instance with no diagnostics in the [DiagnosticsPhase::Recording] phase
    pub fn new(print_immediately: bool) -> Self {
        Self {
            print_immediately,
            phase: DiagnosticsPhase::Recording,
            diagnostics: Arena::new(),
            already_recorded: RefCell::new(HashSet::new()),
        }
    }

    /// Record diagnostic, logging if `print_immediately` is set
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Recording] phase
    pub fn insert(&self, diagnostic: PackageDiagnostic) {
        self.assert_phase("insert", DiagnosticsPhase::Recording);
        if let Some(already_recorded) = self.get_already_recorded(&diagnostic) {
            log::debug!("Redundant diagnostic:\n1.  {}\n2.  {}", already_recorded, diagnostic);
            return;
        }
        let diagnostic = self.do_insert(diagnostic);
        if self.print_immediately && RUST_LOG.logs(diagnostic.level) {
            diagnostic.log_to_rust();
        }
    }

    /// Iterate diagnostics in the order they were inserted.
    /// 
    /// **Panics** if not in the [DiagnosticsPhase::Retrieval] phase
    pub fn iter(&self) -> typed_arena_nomut::Iter<'_, PackageDiagnostic> {
        self.assert_phase("iterate", DiagnosticsPhase::Retrieval);
        self.diagnostics.iter()
    }
    
    /// Count how many diagnostics of a certain level there are.
    pub fn count_level(&self, level: DiagnosticLevel) -> usize {
        self.assert_phase("count", DiagnosticsPhase::Retrieval);
        self.diagnostics.iter().filter(|d| d.level == level).count()
    }

    impl_count_level_shorthands!();

    /// Remove all diagnostics.
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Removal] phase
    pub fn clear(&mut self) {
        self.assert_phase("clear", DiagnosticsPhase::Removal);
        *self.diagnostics = Arena::new();
        self.already_recorded.borrow_mut().clear();
    }

    /// Transition from [DiagnosticsPhase::Recording] to [DiagnosticsPhase::Retrieval]
    pub fn finish_recording(&mut self) {
        self.assert_phase("finish recording", DiagnosticsPhase::Recording);
        self.phase = DiagnosticsPhase::Retrieval;
    }

    /// Transition from [DiagnosticsPhase::Retrieval] to [DiagnosticsPhase::Removal]
    pub fn finish_retrieval(&mut self) {
        self.assert_phase("finish retrieval", DiagnosticsPhase::Retrieval);
        self.phase = DiagnosticsPhase::Removal;
    }

    /// Transition from [DiagnosticsPhase::Removal] to [DiagnosticsPhase::Recording]
    pub fn finish_removal(&mut self) {
        self.assert_phase("finish removal", DiagnosticsPhase::Removal);
        self.phase = DiagnosticsPhase::Recording;
    }

    fn assert_phase(&self, verb: &'static str, phase: DiagnosticsPhase) {
        assert_eq!(self.phase, phase, "Can only {} diagnostics in the {:?} phase", verb, phase);
    }

    fn get_already_recorded(&self, diagnostic: &PackageDiagnostic) -> Option<Ref<'_, PackageDiagnostic>> {
        Ref::filter_map(self.already_recorded.borrow(), |a| {
            // SAFETY: Raw pointer points to element in arena
            a.get(&(diagnostic as *const _)).map(|&d| unsafe { &*d })
        }).ok()
    }

    fn do_insert(&self, diagnostic: PackageDiagnostic) -> &PackageDiagnostic {
        let diagnostic = self.diagnostics.alloc(diagnostic);
        self.already_recorded.borrow_mut().insert(diagnostic as *const _);
        diagnostic
    }
}

impl<'a> IntoIterator for &'a PackageDiagnostics {
    type Item = &'a PackageDiagnostic;
    type IntoIter = typed_arena_nomut::Iter<'a, PackageDiagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl FileDiagnostics {
    pub fn new(path: PathBuf, print_immediately: bool) -> Self {
        Self {
            path,
            print_immediately,
            state: FileDiagnosticsState::Recording {
                diagnostics: Arena::new(),
                already_recorded: RefCell::new(HashSet::new()),
            },
        }
    }

    /// Record diagnostic, logging if `print_immediately` is set
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Recording] phase
    pub fn insert(&self, diagnostic: FileDiagnostic) {
        let FileDiagnosticsState::Recording { diagnostics, already_recorded } = &self.state else {
            panic!("Can only insert diagnostics in the Recording phase")
        };
        if let Some(already_recorded) = Self::get_already_recorded(already_recorded, &diagnostic) {
            log::debug!("Redundant diagnostic:\n1.  {}\n2.  {}", already_recorded.with_ctx(&self.path), diagnostic.with_ctx(&self.path));
            return;
        }
        let diagnostic = Self::do_insert(diagnostics, already_recorded, diagnostic);
        if self.print_immediately && RUST_LOG.logs(diagnostic.level) {
            diagnostic.log_to_rust();
        }
    }

    /// Iterate diagnostics in order of their ranges.
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Retrieval] phase
    pub fn iter(&self) -> std::slice::Iter<'_, FileDiagnostic> {
        let FileDiagnosticsState::Retrieval { ordered_diagnostics } = &self.state else {
            panic!("Can only iterate diagnostics in the Retrieval phase")
        };
        ordered_diagnostics.iter()
    }

    /// Count how many diagnostics of a certain level there are.
    pub fn count_level(&self, level: DiagnosticLevel) -> usize {
        let FileDiagnosticsState::Retrieval { ordered_diagnostics } = &self.state else {
            panic!("Can only count diagnostics in the Retrieval phase")
        };
        ordered_diagnostics.iter().filter(|d| d.level == level).count()
    }

    impl_count_level_shorthands!();

    /// Remove all diagnostics within a specified byte range
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Removal] phase
    pub fn remove_in_byte_range(&mut self, range: std::ops::Range<usize>) {
        let FileDiagnosticsState::Removal { diagnostics, indices_at_point } = &mut self.state else {
            panic!("Can only remove diagnostics in the Removal phase")
        };
        // Same code as in [AnnArena] to remove diagnostics in a range
        let before_range_start = indices_at_point.range(..range.start).next_back().into_iter();
        let in_range = indices_at_point.range(range);
        let all_indices = before_range_start.chain(in_range).flat_map(|(_, indices)| indices).copied();
        for index in all_indices {
            // We don't want to remove from the vec because that has bad time complexity.
            // Instead we save all "removing" for when we construct the arena.
            diagnostics[index] = None
        }
    }

    /// Remove all diagnostics.
    ///
    /// **Panics** if not in the [DiagnosticsPhase::Removal] phase
    pub fn clear(&mut self) {
        let FileDiagnosticsState::Removal { diagnostics, indices_at_point } = &mut self.state else {
            panic!("Can only clear diagnostics in the Removal phase")
        };
        diagnostics.clear();
        indices_at_point.clear();
    }

    /// Transition from [DiagnosticsPhase::Recording] to [DiagnosticsPhase::Retrieval]
    pub fn finish_recording(&mut self) {
        let FileDiagnosticsState::Recording { diagnostics, .. } = take(&mut self.state) else {
            panic!("Can only finish recording diagnostics in the Recording phase")
        };

        // Sort diagnostics by range
        let mut ordered_diagnostics = diagnostics.into_vec();
        ordered_diagnostics.sort_by_key(|d| d.range);
        self.state = FileDiagnosticsState::Retrieval { ordered_diagnostics }
    }

    /// Transition from [DiagnosticsPhase::Retrieval] to [DiagnosticsPhase::Removal]
    pub fn finish_retrieval(&mut self) {
        let FileDiagnosticsState::Retrieval { ordered_diagnostics } = take(&mut self.state) else {
            panic!("Can only finish retrieval diagnostics in the Retrieval phase")
        };
        // TODO: The logic in [AnnArena]
        ordered_diagnostics;
    }

    /// Transition from [DiagnosticsPhase::Removal] to [DiagnosticsPhase::Recording]
    pub fn finish_removal(&mut self) {
        let FileDiagnosticsState::Removal { diagnostics: diagnostics2, .. } = take(&mut self.state) else {
            panic!("Can only finish removal diagnostics in the Removal phase")
        };

        // Reconstruct arena
        let diagnostics = Arena::new();
        let diagnostics2 = diagnostics.alloc_extend(diagnostics2.into_iter().filter_map(|d| d));
        let already_recorded = RefCell::new(diagnostics2.map(|d| d as *const _).collect::<HashSet<_>>());
        self.state = FileDiagnosticsState::Recording { diagnostics, already_recorded }
    }

    fn assert_phase(&self, verb: &'static str, phase: DiagnosticsPhase) {
        assert_eq!(self.phase, phase, "Can only {} diagnostics in the {:?} phase", verb, phase);
    }

    fn get_already_recorded(already_recorded: &RefCell<HashSet<*const FileDiagnostic>>, diagnostic: &FileDiagnostic) -> Option<Ref<'_, FileDiagnostic>> {
        Ref::filter_map(already_recorded.borrow(), |a| {
            // SAFETY: Raw pointer points to element in arena
            a.get(&(diagnostic as *const _)).map(|&d| unsafe { &*d })
        }).ok()
    }

    fn do_insert<'a>(diagnostics: &'a Arena<FileDiagnostic>, already_recorded: &'a RefCell<HashSet<*const FileDiagnostic>>, diagnostic: FileDiagnostic) -> &'a FileDiagnostic {
        let diagnostic = diagnostics.alloc(diagnostic);
        already_recorded.borrow_mut().insert(diagnostic as *const _);
        diagnostic
    }
}

impl<'a> IntoIterator for &'a mut FileDiagnostics {
    type Item = &'a FileDiagnostic;
    type IntoIter = std::slice::Iter<'a, FileDiagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl PackageDiagnostic {
    pub fn add_info(&mut self, info: impl Iterator<Item=AdditionalInfo>) {
        self.additional_info.extend(info);
    }
}

impl FileDiagnostic {
    pub fn from_package(package: PackageDiagnostic, location: TSRange) -> Self {
        Self {
            level: package.level,
            message: package.message,
            additional_info: package.additional_info,
            location
        }
    }

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
                        log::error!(
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
                log::error!("Invalid RUST_LOG value: not unicode ({})", str.to_string_lossy());
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
impl Display for PackageDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.assert_phase("display", DiagnosticsPhase::Retrieval);
        for diagnostic in self.diagnostics.iter() {
            writeln!(f, "{}", diagnostic)?;
        }
        Ok(())
    }
}

impl Display for FileDiagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.assert_phase("display", DiagnosticsPhase::Retrieval);
        for diagnostic in self.diagnostics.iter() {
            writeln!(f, "{}", diagnostic.with_ctx(&self.path))?;
        }
        Ok(())
    }
}

impl Display for PackageDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.level, self.message)?;
        for info in &self.additional_info {
            write!(f, "\n  {}", info)?;
        }
        Ok(())
    }
}

impl DisplayWithCtx<Path> for FileDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>, path: &Path) -> std::fmt::Result {
        write!(f, "{} @ {}:{}: {}", self.level, path.display(), self.location, self.message)?;
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

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticLevel::Error => write!(f, "error"),
            DiagnosticLevel::Warning => write!(f, "warning"),
            DiagnosticLevel::Info => write!(f, "info"),
            DiagnosticLevel::Debug => write!(f, "debug"),
        }
    }
}

impl Display for AdditionalInfoType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AdditionalInfoType::Issue => write!(f, "issue"),
            AdditionalInfoType::Hint => write!(f, "hint"),
            AdditionalInfoType::Note => write!(f, "note"),
        }
    }
}
// endregion

// region eq ord
impl PartialEq<PackageDiagnostic> for PackageDiagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level
            && self.message == other.message
    }
}

impl Eq for PackageDiagnostic {}

impl PartialOrd<PackageDiagnostic> for PackageDiagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PackageDiagnostic {
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