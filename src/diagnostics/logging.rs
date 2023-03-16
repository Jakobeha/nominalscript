use std::path::Path;
use std::ptr::NonNull;
use smallvec::SmallVec;
use crate::diagnostics::{FileDiagnostic, FileDiagnostics, GlobalDiagnostic, ProjectDiagnostics};
use crate::misc::lazy_alt::LazyError;

/// Allows you to log project (global or file) diagnostics.
///
/// Methods which can log project diagnostics take this as a parameter named `e: &mut ProjectLogger<'_>`.
/// They should not take [ProjectDiagnostics] in order to separate logging from printing,
/// though the separation isn't actually necessary or used for anything.
#[derive(Debug)]
pub struct ProjectLogger<'a>(&'a ProjectDiagnostics);

/// Allows you to log file diagnostics.
///
/// Methods which can log file diagnostics take this as a parameter `e: &mut FileLogger<'_>`.
/// They should not take [FileDiagnostics] in order to separate logging from printing,
/// though the separation isn't actually necessary or used for anything.
#[derive(Debug)]
pub struct FileLogger<'a>(&'a FileDiagnostics);

/// Allows you to log diagnostics for a type.
///
/// Type methods which can log diagnostics (e.g. unification) take this as a parameter named `e: TypeLogger` or
/// `mut e: TypeLogger`. Instead of passing `e` directly to multiple children, you must use `e.with_context(...)`
#[derive(Debug)]
pub struct TypeLogger<'a, 'b: 'a, 'c: 'b>(_TypeLogger<'a, 'b, 'c>);

#[derive(Debug)]
enum _TypeLogger<'a, 'b: 'a, 'c: 'b> {
    Base { base: TypeLoggerBase<'b, 'c> },
    Derived { base: &'a mut TypeLoggerBase<'b, 'c> }
}

#[derive(Debug)]
struct TypeLoggerBase<'b, 'c: 'b> {
    file: &'b FileLogger<'c>,
    inferred_loc: TSNode<'b>,
    // RULE A: these are always active references (though may have different lifetimes)
    context: SmallVec<[TypeLocPtr; 2]>
}

impl<'a> ProjectLogger<'a> {
    pub fn new(diagnostics: &'a ProjectDiagnostics) -> Self {
        Self(diagnostics)
    }

    pub fn file(&self, path: impl AsRef<Path>) -> FileLogger<'a> {
        FileLogger(self.0.file(path))
    }

    pub fn log(&self, diagnostic: GlobalDiagnostic) {
        self.0.insert_global(diagnostic)
    }
}

impl<'a> FileLogger<'a> {
    pub fn new(diagnostics: &'a FileDiagnostics) -> Self {
        Self(diagnostics)
    }

    pub fn log(&self, diagnostic: FileDiagnostic) {
        self.0.insert(diagnostic)
    }

    pub fn unwrap_import_result<T>(
        &self,
        result: Result<T, LazyError>,
        def: TSNode<'_>,
        use_: Option<TSNode<'_>>
    ) -> Option<T> {
        match result {
            Ok(result) => Some(result),
            Err(error) => {
                let msg = match error {
                    LazyError::CycleDetected => "recursive import",
                    LazyError::ForcePanicked => "import caused a panic"
                };
                error!(self, "{}", msg => def;
                    note_if!(use_ => use_, "used here" => use_)
                );
                None
            }
        }
    }
}

impl<'c> FileLogger<'c> {
    pub fn type_<'b>(&'b self, inferred_loc: TSNode<'b>) -> TypeLogger<'b, 'b, 'c> {
        TypeLogger(_TypeLogger::Base { base: TypeLoggerBase {
            file: self,
            inferred_loc,
            context: SmallVec::new()
        } })
    }
}

impl<'a, 'b: 'a, 'c: 'b> TypeLogger<'a, 'b, 'c> {
    pub fn with_context<'a2>(
        &'a2 mut self,
        context: TypeLoc<'_>
    ) -> TypeLogger<'a2, 'b, 'c> {
        // ^ RULE A -> will pop context on derived drop
        self.base().context.push(context.as_ptr());
        TypeLogger(_TypeLogger::Derived { base })
    }

    fn base(&mut self) -> &mut TypeLoggerBase<'b, 'c> {
        match &mut self.0 {
            _TypeLogger::Base { base } => base,
            _TypeLogger::Derived { base } => base
        }
    }

    pub fn log(&mut self, diagnostic: GlobalDiagnostic) {
        let base = self.base();
        let mut diagnostic = FileDiagnostic::from_global(
            diagnostic,
            base.inferred_loc.range()
        );
        for context in &base.context {
            // v RULE A
            let context = unsafe { context.as_ref() };
            diagnostic.add_info(note!("in {}", context));
        }
        base.file.log(diagnostic)
    }
}

impl<'a, 'b: 'a, 'c: 'b> Drop for _TypeLogger<'a, 'b, 'c> {
    fn drop(&mut self) {
        match self {
            _TypeLogger::Base { base } => {
                // Sanity check
                debug_assert!(base.context.is_empty());
            },
            _TypeLogger::Derived { base } => {
                // ^ RULE A: Must pop top context as it's no longer live
                base.context.pop();
            }
        }
    }
}

macro_rules! log {
    ($e:expr, $level:expr, $format:literal $(, $arg:expr)* $(,)? $(;
         $additional_info:expr)* $(;)?) => {
        $e.log($crate::diagnostics::GlobalDiagnostic {
            level: $level,
            message: format!($format $(, $arg)*),
            additional_info: $crate::misc::chain![
                $($additional_info),*
            ].collect::<::smallvec::SmallVec<_>>(),
        })
    };
    ($e:expr, $level:expr, $format:literal $(, $arg:expr)* $(,)? => $loc:expr $(;
         $additional_info:expr)* $(;)?) => {
        $e.log($crate::diagnostics::FileDiagnostic {
            level: $level,
            message: format!($format $(, $arg)*),
            location: $loc.range(),
            additional_info: $crate::misc::chain![
                $($additional_info),*
            ].collect::<::smallvec::SmallVec<_>>(),
        })
    };
}
pub(crate) use log;

macro_rules! error {
    ($e:expr, $( $arg:tt )*) => {
        $crate::diagnostics::log!($e, $crate::diagnostics::DiagnosticLevel::Error, $( $arg )*)
    };
}
pub(crate) use error;

macro_rules! warning {
    ($e:expr, $( $arg:tt )*) => {
        $crate::diagnostics::log!($e, $crate::diagnostics::DiagnosticLevel::Warning, $( $arg )*)
    };
}
pub(crate) use warning;

macro_rules! info {
    ($e:expr, $( $arg:tt )*) => {
        $crate::diagnostics::log!($e, $crate::diagnostics::DiagnosticLevel::Info, $( $arg )*)
    };
}
pub(crate) use info;

macro_rules! debug {
    ($e:expr, $( $arg:tt )*) => {
        $crate::diagnostics::log!($e, $crate::diagnostics::DiagnosticLevel::Debug, $( $arg )*)
    };
}
pub(crate) use debug;

macro_rules! additional_info {
    ($type_:expr, $format:literal $(, $arg:expr)* $(,)?) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $type_,
            message: format!($format $(, $arg)*),
            location: None,
        })
    };
    ($type_:expr, $format:literal $(, $arg:expr)* $(,)? => $loc:expr) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $type_,
            message: format!($format $(, $arg)*),
            location: Some($loc.range()),
        })
    };
}
pub(crate) use additional_info;

macro_rules! issue {
    ($($arg:tt)*) => {
        $crate::diagnostics::additional_info!($crate::diagnostics::AdditionalInfoType::Issue, $($arg)*)
    };
}
pub(crate) use issue;

macro_rules! hint {
    ($($arg:tt)*) => {
        $crate::diagnostics::additional_info!($crate::diagnostics::AdditionalInfoType::Hint, $($arg)*)
    };
}
pub(crate) use hint;

macro_rules! note {
    ($($arg:tt)*) => {
        $crate::diagnostics::additional_info!($crate::diagnostics::AdditionalInfoType::Note, $($arg)*)
    };
}
pub(crate) use note;
use crate::ast::tree_sitter::TSNode;

macro_rules! issue_if {
    ($optional_expr:expr => $optional_id:ident, $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| issue!($($arg)*))
    };
}
pub(crate) use issue_if;

macro_rules! hint_if {
    ($optional_expr:expr => $optional_id:ident, $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| hint!($($arg)*))
    };
}
pub(crate) use hint_if;

macro_rules! note_if {
    ($optional_expr:expr => $optional_id:ident, $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| note!($($arg)*))
    };
}
pub(crate) use note_if;
use crate::analyses::types::{TypeLoc, TypeLocPtr};

#[cfg(test)]
mod tests {
    use std::path::Path;
    use crate::diagnostics::{ProjectDiagnostics, ProjectLogger, error, warning, info, debug, issue, hint, note};
    use crate::ast::tree_sitter::TSParser;
    use tree_sitter_typescript::language_typescript;

    #[test]
    pub fn test() {
        let code = "
            function hello() {
                console.log('hello');
            }
        ";
        let mut parser = TSParser::new(language_typescript()).unwrap();
        let tree = parser.parse_string(String::from(code)).unwrap();
        let a_node = tree.root_node().named_child(0).unwrap();
        let b_node = a_node.named_child(1).unwrap();

        let mut diagnostics = ProjectDiagnostics::new();
        let p = Path::new("p");
        let mut e = ProjectLogger::new(&mut diagnostics);
        error!(e, "hello");
        warning!(e, "hello");
        info!(e, "hello");
        debug!(e, "hello");
        error!(e.file(p), "hello" => a_node);
        warning!(e.file(p), "hello {}", "hello" => a_node);
        info!(e.file(p), "hello {:?} {:?}", "hello", a_node => a_node);
        debug!(e.file(p), "hello" => a_node);
        error!(e, "hello";
            issue!("hello"));
        warning!(e, "hello";
            issue!("hello");
            hint!("hello"));
        info!(e, "hello";
            issue!("hello");
            hint!("hello");
            note!("hello"));
        debug!(e, "hello";
            issue!("hello {:?}", "hello");
            hint!("hello");
            note!("hello"));
        error!(e, "hello";
            issue!("hello {:?}", "hello");
            hint!("hello" => b_node);
            note!("hello"));
        warning!(e.file(p), "hello" => a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" => b_node);
            note!("hello"));
        info!(e.file(p), "hello" => a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" => b_node);
            Some(5).into_iter().flat_map(|n| note!("hello {}", n)));
        debug!(e.file(p), "hello" => a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" => b_node);
            note_if!(Some(5) => n, "hello {}", n));
        error!(e.file(p), "hello" => a_node;
            issue!("hello {:?}", "hello");
            hint_if!(None => n, "hello {}", n);
            hint!("hello" => b_node));
        warning!(e.file(p), "hello" => a_node;
            issue_if!(Some(5) => n, "hello {}", n);
            hint!("hello {:?}", "hello");
            hint!("hello" => b_node));
    }
}