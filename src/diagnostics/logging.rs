use std::cell::RefCell;

use smallvec::SmallVec;

use crate::analyses::types::{ThinType, TypeLoc};
use crate::concrete::tree_sitter::TSNode;
use crate::diagnostics::{FileDiagnostic, FileDiagnostics, GlobalDiagnostic, ProjectDiagnostics};
use crate::import_export::ModulePath;
use crate::{note, hint, issue};

/// Allows you to log project (global or file) diagnostics.
///
/// Methods which can log project diagnostics take this as a parameter named `e: &mut ProjectLogger<'_>`.
/// They should not take [ProjectDiagnostics] in order to separate logging from printing,
/// though the separation isn't actually necessary or used for anything.
#[derive(Debug)]
pub struct ProjectLogger<'a>(&'a ProjectDiagnostics);

/// Allows you to log file diagnostics.
///
/// Methods which can log file diagnostics take this as a parameter `e: &FileLogger<'_>`.
/// They should not take [FileDiagnostics] in order to separate logging from printing,
/// though the separation isn't actually necessary or used for anything.
#[derive(Debug)]
pub struct FileLogger<'a>(&'a FileDiagnostics);

/// Allows you to log diagnostics for a type.
///
/// Type methods which can log diagnostics (e.g. unification) take this as a parameter named `e: TypeLogger` or
/// `mut e: TypeLogger`. Instead of passing `e` directly to multiple children, you must use `e.with_context(...)`
#[derive(Debug)]
pub struct TypeLogger<'a, 'b, 'tree>(_TypeLogger<'a, 'b, 'tree>);

#[derive(Debug)]
enum _TypeLogger<'a, 'b, 'tree> {
    Base { base: TypeLoggerBase<'b, 'tree> },
    Derived { base: &'a TypeLoggerBase<'b, 'tree> },
    Ignore
}

#[derive(Debug)]
struct TypeLoggerBase<'b, 'tree> {
    diagnostics: &'b FileDiagnostics,
    info: TypeCheckInfo<'tree>,
    // RULE A: This contains the context of the current TypeLogger:
    // - pushes when a new TypeLogger is created via with_context
    // - pops when that TypeLogger is destroyed.
    // If these are pointers, they are always active references though may have different lifetimes
    // (but they're not so currently we don't have to do anything unsafe)
    context: RefCell<SmallVec<[TypeLoc; 2]>>
}

/// The additional info displayed to the user in type-check errors
#[derive(Debug)]
pub struct TypeCheckInfo<'tree> {
    pub loc_node: TSNode<'tree>,
    pub assigned_info: TypeInfo<'tree>,
    pub required_info: TypeInfo<'tree>,
}

/// Part of the additional info displayed to the user in type-check errors;
/// the part for the assigned or required type
#[derive(Debug)]
pub struct TypeInfo<'tree> {
    pub thin_type: ThinType<'tree>,
    pub defined_value: Option<TSNode<'tree>>,
    pub explicit_type: Option<TSNode<'tree>>,
}

impl<'a> ProjectLogger<'a> {
    pub fn new(diagnostics: &'a ProjectDiagnostics) -> Self {
        Self(diagnostics)
    }

    pub fn file(&self, path: &ModulePath) -> FileLogger<'a> {
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

    pub fn type_<'b, 'tree>(&'b self, info: TypeCheckInfo<'tree>) -> TypeLogger<'b, 'b, 'tree> {
        TypeLogger(_TypeLogger::Base { base: TypeLoggerBase {
            diagnostics: self.0,
            info,
            context: RefCell::new(SmallVec::new())
        } })
    }
}

impl<'a, 'b, 'tree> TypeLogger<'a, 'b, 'tree> {
    pub fn ignore() -> TypeLogger<'a, 'b, 'tree> {
        TypeLogger(_TypeLogger::Ignore)
    }

    pub fn with_context<'a2>(
        &'a2 self,
        context: TypeLoc
    ) -> TypeLogger<'a2, 'b, 'tree> {
        // ^ RULE A -> will pop context on derived drop
        match self.base() {
            None => TypeLogger::ignore(),
            Some(base) => {
                base.context.borrow_mut().push(context);
                TypeLogger(_TypeLogger::Derived { base })
            }
        }
    }

    fn base(&self) -> Option<&TypeLoggerBase<'b, 'tree>> {
        match &self.0 {
            _TypeLogger::Base { base } => Some(base),
            _TypeLogger::Derived { base } => Some(base),
            _TypeLogger::Ignore => None
        }
    }

    pub fn log(&self, diagnostic: GlobalDiagnostic) {
        let Some(base) = self.base() else {
            return
        };
        let TypeCheckInfo {
            loc_node,
            assigned_info: TypeInfo {
                thin_type: assigned_type,
                defined_value: assigned_value,
                explicit_type: assigned_explicit_type
            },
            required_info: TypeInfo {
                thin_type: required_type,
                defined_value: required_value,
                explicit_type: required_explicit_type
            }
        } = &base.info;
        let mut diagnostic = FileDiagnostic::from_global(diagnostic, loc_node.range());
        let context_borrow = base.context.borrow();
        for context in &*context_borrow {
            diagnostic.add_info(note!("in {}", context));
        }
        diagnostic.add_info(issue!("assigned type: `{}`", assigned_type));
        diagnostic.add_info(issue!("required type: `{}`", required_type));
        if let Some(assigned_value) = assigned_value {
            diagnostic.add_info(hint!("assigned type inferred here" => assigned_value));
        }
        if let Some(required_value) = required_value {
            diagnostic.add_info(hint!("required type inferred here" => required_value));
        }
        if let Some(assigned_explicit_type) = assigned_explicit_type {
            diagnostic.add_info(hint!("assigned type defined here" => assigned_explicit_type));
        }
        if let Some(required_explicit_type) = required_explicit_type {
            diagnostic.add_info(hint!("required type defined here" => required_explicit_type));
        }
        base.diagnostics.insert(diagnostic)
    }
}

impl<'a, 'b, 'tree> Drop for _TypeLogger<'a, 'b, 'tree> {
    fn drop(&mut self) {
        match self {
            _TypeLogger::Base { base } => {
                // Sanity check
                debug_assert!(base.context.borrow().is_empty());
            },
            _TypeLogger::Derived { base } => {
                // ^ RULE A: Must pop top context as it's no longer live
                base.context.borrow_mut().pop();
            },
            _TypeLogger::Ignore => {}
        }
    }
}

#[macro_export]
macro_rules! log {
    ($level:expr, $format:literal @ $loc:ident $(, $arg:expr)* $(;
         $additional_info:expr)* $(;)?) => {
        $loc.logger().log($crate::diagnostics::FileDiagnostic {
            level: $level,
            message: format!($format $(, $arg)*),
            location: $loc.range(),
            additional_info: $crate::misc::chain![
                $($additional_info),*
            ].collect::<::smallvec::SmallVec<_>>(),
        })
    };
    ($level:expr, $format:literal @ $loc:expr $(, $arg:expr)* $(;
         $additional_info:expr)* $(;)?) => {{
        let loc = $loc;
        $crate::log!($level, $format @ loc $(, $arg)* $(; $additional_info)*);
    }};
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        $crate::log!($crate::diagnostics::DiagnosticLevel::Error, $($arg)*)
    };
}

#[macro_export]
macro_rules! warning {
    ($($arg:tt)*) => {
        $crate::log!($crate::diagnostics::DiagnosticLevel::Warning, $($arg)*)
    };
}

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => {
        $crate::log!($crate::diagnostics::DiagnosticLevel::Info, $($arg)*)
    };
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        $crate::log!($crate::diagnostics::DiagnosticLevel::Debug, $($arg)*)
    };
}

#[macro_export]
macro_rules! additional_info {
    ($type_:expr, $format:literal @ $loc:ident $(, $arg:expr)* $(,)?) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $type_,
            message: format!($format $(, $arg)*),
            location: Some($loc.range()),
        })
    };
    ($type_:expr, $format:literal @ $loc:expr $(, $arg:expr)* $(,)?) => {{
        let loc = $loc;
        $crate::additional_info!($type_, $format @ loc $(, $arg)*)
    }};
    ($type_:expr, $format:literal @? $loc:ident $(, $arg:expr)* $(,)?) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $type_,
            message: format!($format $(, $arg)*),
            location: $loc.map(|l| l.range()),
        })
    };
    ($type_:expr, $format:literal @? $loc:expr $(, $arg:expr)* $(,)?) => {{
        let loc = $loc;
        $crate::additional_info!($type_, $format @? loc $(, $arg)*)
    }};
    ($type_:expr, $format:literal $(, $arg:expr)* $(,)?) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $type_,
            message: format!($format $(, $arg)*),
            location: None,
        })
    };
}

#[macro_export]
macro_rules! issue {
    ($($arg:tt)*) => {
        $crate::additional_info!($crate::diagnostics::AdditionalInfoType::Issue, $($arg)*)
    };
}

#[macro_export]
macro_rules! hint {
    ($($arg:tt)*) => {
        $crate::additional_info!($crate::diagnostics::AdditionalInfoType::Hint, $($arg)*)
    };
}

#[macro_export]
macro_rules! note {
    ($($arg:tt)*) => {
        $crate::additional_info!($crate::diagnostics::AdditionalInfoType::Note, $($arg)*)
    };
}

#[macro_export]
macro_rules! issue_if {
    ($optional_id:ident => $($arg:tt)*) => {
        issue_if!($optional_id, $optional_id => $($arg)*)
    };
    ($optional_expr:expr, $optional_id:ident => $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| $crate::issue!($($arg)*))
    };
}

#[macro_export]
macro_rules! hint_if {
    ($optional_id:ident => $($arg:tt)*) => {
        hint_if!($optional_id, $optional_id => $($arg)*)
    };
    ($optional_expr:expr, $optional_id:ident => $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| $crate::hint!($($arg)*))
    };
}

#[macro_export]
macro_rules! note_if {
    ($optional_id:ident => $($arg:tt)*) => {
        note_if!($optional_id, $optional_id => $($arg)*)
    };
    ($optional_expr:expr, $optional_id:ident => $($arg:tt)*) => {
        $optional_expr.into_iter().flat_map(|$optional_id| $crate::note!($($arg)*))
    };
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use tree_sitter_typescript::language_typescript;
    use type_sitter_lib::UntypedNode;
    use yak_sitter::Parser;

    use crate::{debug, error, hint, info, issue, note, warning, hint_if, note_if, issue_if};
    use crate::diagnostics::{ProjectDiagnostics, ProjectLogger};
    use crate::import_export::ModulePath;

    #[test]
    pub fn test() {
        let code = "
            function hello() {
                console.log('hello');
            }
        ";
        let mut parser = Parser::new(language_typescript()).unwrap();
        let tree = parser.parse_string(String::from(code), None, None, ()).unwrap();
        let root = UntypedNode::new(tree.root_node());
        let a_node = UntypedNode::new(tree.root_node().named_child(0).unwrap());
        let b_node = UntypedNode::new(a_node.named_child(1).unwrap());

        let mut diagnostics = ProjectDiagnostics::new(false);
        let p = ModulePath::ns(PathBuf::from("p.ns"));
        let e = ProjectLogger::new(&mut diagnostics);
        error!("hello" @ root);
        warning!("hello" @ root);
        info!("hello" @ root);
        debug!("hello" @ root);
        error!("hello" @ { a_node });
        warning!("hello {}" @ a_node, "hello");
        info!("hello {:?} {:?}" @ { a_node }, "hello", a_node);
        debug!("hello" @ a_node);
        error!("hello" @ root;
            issue!("hello"));
        warning!("hello" @ root;
            issue!("hello");
            hint!("hello"));
        info!("hello" @ root;
            issue!("hello");
            hint!("hello");
            note!("hello"));
        debug!("hello" @ root;
            issue!("hello {:?}", "hello");
            hint!("hello");
            note!("hello"));
        error!("hello" @ root;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note!("hello"));
        warning!("hello" @ { a_node };
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note!("hello"));
        info!("hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            Some(5).into_iter().flat_map(|n| note!("hello {}", n)));
        debug!("hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note_if!(Some(5), n => "hello {}", n));
        error!("hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint_if!(None::<String>, n => "hello {}", n);
            hint!("hello" @ b_node));
        warning!("hello" @ a_node;
            issue_if!(Some(5), n => "hello {}", n);
            hint!("hello {:?}", "hello");
            hint!("hello" @ b_node));
        let n = Some(5);
        warning!("hello" @ a_node;
            issue_if!(n => "hello {}", n);
            hint!("hello {:?}", "hello");
            hint!("hello" @ b_node));
    }
}