use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::env::VarError;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use join_lazy_fmt::Join;
use lazy_static::lazy_static;
use nonempty::NonEmpty;
use smallvec::SmallVec;
use yak_sitter::Node;

use crate::semantic::storage::{Ann, DerivedNodeStore, HasStore};

/// All diagnostics for a package
#[derive(Debug)]
pub struct Diagnostics<'tree> {
    /// If set, diagnostics are printed immediately after being recorded. Useful for debugging.
    ///
    /// We still store all diagnostics because we allow loggers to remove duplicates.
    print_immediately: bool,
    /// The diagnostics
    diagnostics: RefCell<HashMap<Ann<'tree>, NonEmpty<Diagnostic<'tree>>>>,
}

/// A diagnostic in a package
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic<'tree> {
    /// Location tied to the diagnostic.
    ///
    /// This is an annotation like used in semantic nodes, and diagnostics are like semantic nodes,
    /// but not quite. The key difference is that multiple diagnostics can be derived from the same
    /// annotation, as long as they have different messages. If we were to have a separate Rust type
    /// for each diagnostic type, diagnostics could be considered semantic nodes.
    pub loc: Ann<'tree>,
    /// Diagnostic level AKA error, warning, info, or debug
    pub level: DiagnosticLevel,
    /// Diagnostic message (formatted string)
    pub message: String,
    /// Ex: hints or root causes
    pub additional_info: SmallVec<[AdditionalInfo<'tree>; 4]>
}

/// Display a diagnostic without its log level. Used internally to abstract regular printing and
/// [Diagnostic::log_to_rust]
struct DisplayDiagnosticWithoutLevel<'a, 'tree>(&'a Diagnostic<'tree>);

/// Diagnostic level: how important is the diagnostic, and is it a bad thing or just debug message?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    /// Diagnostic must be fixed, prevents successful compilation
    Error,
    /// Diagnostic should be fixed but doesn't prevent successful compilation
    Warning,
    /// Diagnostic doesn't have to be addressed, is shown to the user
    Info,
    /// Diagnostic doesn't have to be addressed and is only shown in debug mode
    Debug
}

/// What kinds of diagnostics we log. This isn't quite the level of the diagnostics themselves but
/// similar.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoggerLevel {
    /// Don't print any diagnostics
    DontLog,
    /// Print diagnostics with level greater than or equal to `level`
    DoLog { level: DiagnosticLevel }
}

/// Additional info tied to diagnostics like hints or root causes
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AdditionalInfo<'tree> {
    /// Location tied to additional info.
    pub loc: Ann<'tree>,
    /// Whether this is a hint, note, or root cause
    pub type_: AdditionalInfoType,
    /// Additional info message
    pub message: String,
}

/// Type of additional info tied to diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AdditionalInfoType {
    /// This points towards the root cause or a contributing cause
    Issue,
    /// This is a suggestion for how to fix the diagnostic, or if it's a note or debug diagnostic,
    /// how to write better code
    Hint,
    /// Any other type of additional info which isn't a hint or issue
    Note,
}

impl<'tree> Diagnostics<'tree> {
    /// Create an instance with no diagnostics.
    pub fn new(print_immediately: bool) -> Self {
        Self {
            print_immediately,
            diagnostics: RefCell::new(HashMap::new()),
        }
    }

    /// Log the given diagnostic
    pub fn insert(&self, diagnostic: Diagnostic<'tree>) {
        let do_print = || {
            if self.print_immediately && RUST_LOG.logs(diagnostic.level) {
                diagnostic.log_to_rust();
            }
        };

        let mut diagnostics = self.diagnostics.borrow_mut();
        match diagnostics.entry(diagnostic.loc) {
            Entry::Vacant(entry) => {
                do_print();
                entry.insert(NonEmpty::new(diagnostic));
            },
            Entry::Occupied(mut entry) => {
                let diagnostics = entry.get_mut();
                match diagnostics.binary_search_by_key(&&diagnostic.message, |d| &d.message) {
                    Ok(_) => log::debug!("Redundant diagnostic:\n  {}", diagnostic),
                    Err(insertion_index) => {
                        do_print();
                        diagnostics.insert(insertion_index, diagnostic)
                    }
                }
            }
        }
    }

    /// Count how many diagnostics of a certain level *including file diagnostics* were recorded
    pub fn count_level(&self, level: DiagnosticLevel) -> usize {
        self.diagnostics.borrow().values()
            .map(|diagnostics| diagnostics.iter().filter(|diagnostic| diagnostic.level == level).count())
            .sum()
    }

    /// Count how many "error" diagnostics *including file diagnostics* were recorded
    pub fn count_errors(&self) -> usize {
        self.count_level(DiagnosticLevel::Error)
    }

    /// Count how many "warning" diagnostics *including file diagnostics* were recorded
    pub fn count_warnings(&self) -> usize {
        self.count_level(DiagnosticLevel::Warning)
    }

    /// Count how many "info" diagnostics *including file diagnostics* were recorded
    pub fn count_infos(&self) -> usize {
        self.count_level(DiagnosticLevel::Info)
    }

    /// Count how many "debug" diagnostics *including file diagnostics* were recorded
    pub fn count_debugs(&self) -> usize {
        self.count_level(DiagnosticLevel::Debug)
    }

    /// Remove all diagnostics.
    pub fn clear(&self) {
        self.diagnostics.borrow_mut().clear();
    }

    /// Remove diagnostics with the given annotation
    pub fn remove_at(&self, ann: Ann<'tree>) {
        self.diagnostics.borrow_mut().remove(&ann);
    }
}

impl<'tree> Diagnostic<'tree> {
    /// Set the location if [Ann::Intrinsic], otherwise set to [Ann::Derived]
    pub fn add_loc(
        &mut self,
        loc: Ann<'tree>,
        store: impl HasStore<'tree, DerivedNodeStore<'tree>>
    ) {
        self.loc.merge_with(loc, store);
    }

    /// Can add one or more information. This is called `add_info` not only because "info" is
    /// plural, but mainly because macros like [crate::issue], [crate::note], and [crate::hint]
    /// return an iterator, in order to make macros like [crate::issue_if] and [crate::note_if]
    /// have the same type.
    pub fn add_info(&mut self, info: impl Iterator<Item=AdditionalInfo<'tree>>) {
        self.additional_info.extend(info);
    }

    /// Print the diagnostic using the [log] crate.
    pub fn log_to_rust(&self) {
        let level = self.level.rust_log_level();

        log::log!(level, "{}", DisplayDiagnosticWithoutLevel(self));
    }
}

impl DiagnosticLevel {
    pub fn rust_log_level(&self) -> log::Level {
        match self {
            DiagnosticLevel::Error => log::Level::Error,
            DiagnosticLevel::Warning => log::Level::Warn,
            DiagnosticLevel::Info => log::Level::Info,
            DiagnosticLevel::Debug => log::Level::Debug
        }
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
struct DisplaySource<'a, 'tree>(&'a Node<'tree>);

impl<'tree> Display for Diagnostics<'tree> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for diagnostics in self.diagnostics.borrow().values() {
            for diagnostic in diagnostics {
                writeln!(f, "{}", diagnostic)?;
            }
        }
        Ok(())
    }
}

impl<'tree> Display for Diagnostic<'tree> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.level)?;
        match self.loc.num_sources() {
            0 => {},
            1 => write!(f, " @ {}", DisplaySource(self.loc.first_source().unwrap()))?,
            _ => write!(f, " @ {}", ", ".join(self.loc.sources().map(DisplaySource)))?,
        };
        write!(f, ": {}", self.message)?;
        for info in &self.additional_info {
            write!(f, "\n  {}", info)?;
        }
        Ok(())
    }
}

impl<'a, 'tree> Display for DisplayDiagnosticWithoutLevel<'a, 'tree> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0.loc.num_sources() {
            0 => {},
            1 => write!(f, "@ {}: ", DisplaySource(self.0.loc.first_source().unwrap()))?,
            _ => write!(f, "@ {}: ", ", ".join(self.0.loc.sources().map(DisplaySource)))?,
        };
        write!(f, "{}", self.0.message)?;
        for info in &self.0.additional_info {
            write!(f, "\n  {}", info)?;
        }
        Ok(())
    }
}

impl<'tree> Display for AdditionalInfo<'tree> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)?;
        match self.loc.num_sources() {
            0 => {},
            1 => write!(f, " (at {})", DisplaySource(self.loc.first_source().unwrap()))?,
            _ => write!(f, " (at {})", ", ".join(self.loc.sources().map(DisplaySource)))?
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

impl<'a, 'tree> Display for DisplaySource<'a, 'tree> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = self.0.path() {
            write!(f, "{}:", path.display())?;
        }
        write!(f, "{}", self.0.position_range())
    }
}
// endregion