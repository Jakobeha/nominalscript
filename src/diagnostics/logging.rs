#[macro_export]
macro_rules! log {
    ($e:expr, $level:expr, $format:literal @ $loc:expr $(, $arg:expr)* $(;
         $additional_info:expr)* $(;)?) => {
        $e.insert($crate::diagnostics::Diagnostic {
            level: $level,
            message: format!($format $(, $arg)*),
            location: $loc,
            additional_info: $crate::misc::chain![
                $($additional_info),*
            ].collect::<::smallvec::SmallVec<_>>(),
        })
    };
    ($e:expr, $level:expr, $format:literal @? $loc:expr $(, $arg:expr)* $(;
         $additional_info:expr)* $(;)?) => {
        crate::log!($e, $level, $format @ $loc.unwrap_or_default() $(, $arg)* $(;
            $additional_info)*)
    };
    ($e:expr, $level:expr, $format:literal $(, $arg:expr)* $(;
         $additional_info:expr)* $(;)?) => {
        crate::log!($e, $level, $format @ $crate::semantic::Ann::Intrinsic $(, $arg)* $(;
            $additional_info)*)
    };
}

#[macro_export]
macro_rules! error {
    ($e:expr, $($arg:tt)*) => {
        $crate::log!($e, $crate::diagnostics::DiagnosticLevel::Error, $($arg)*)
    };
}

#[macro_export]
macro_rules! warning {
    ($e:expr, $($arg:tt)*) => {
        $crate::log!($e, $crate::diagnostics::DiagnosticLevel::Warning, $($arg)*)
    };
}

#[macro_export]
macro_rules! info {
    ($e:expr, $($arg:tt)*) => {
        $crate::log!($e, $crate::diagnostics::DiagnosticLevel::Info, $($arg)*)
    };
}

#[macro_export]
macro_rules! debug {
    ($e:expr, $($arg:tt)*) => {
        $crate::log!($e, $crate::diagnostics::DiagnosticLevel::Debug, $($arg)*)
    };
}

#[macro_export]
macro_rules! additional_info {
    ($r#type:expr, $format:literal @ $loc:expr $(, $arg:expr)* $(,)?) => {
        ::std::iter::once($crate::diagnostics::AdditionalInfo {
            type_: $r#type,
            message: format!($format $(, $arg)*),
            location: $loc,
        })
    };
    ($r#type:expr, $format:literal @? $loc:expr $(, $arg:expr)* $(,)?) => {
        $crate::additional_info($r#type, $format @ $loc.unwrap_or_default() $(, $arg)*)
    };
    ($r#type:expr, $format:literal $(, $arg:expr)* $(,)?) => {
        $crate::additional_info($r#type, $format @ $crate::semantic::Ann::Intrinsic $(, $arg)*)
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
    use tree_sitter_typescript::language_typescript;
    use type_sitter_lib::UntypedNode;
    use yak_sitter::Parser;

    use crate::{debug, error, hint, hint_if, info, issue, issue_if, note, note_if, warning};
    use crate::diagnostics::Diagnostics;

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

        let e = Diagnostics::new(false);
        error!(e, "hello" @ root);
        warning!(e, "hello" @ root);
        info!(e, "hello" @ root);
        debug!(e, "hello" @ root);
        error!(e, "hello" @ { a_node });
        warning!(e, "hello {}", "hello");
        warning!(e, "hello {}" @ a_node, "hello");
        info!(e, "hello {:?} {:?}" @ { a_node }, "hello", a_node);
        debug!(e, "hello" @ a_node);
        error!(e, "hello" @ root;
            issue!("hello"));
        warning!(e, "hello" @ root;
            issue!("hello");
            hint!("hello"));
        info!(e, "hello" @ root;
            issue!("hello");
            hint!("hello");
            note!("hello"));
        debug!(e, "hello" @ root;
            issue!("hello {:?}", "hello");
            hint!("hello");
            note!("hello"));
        error!(e, "hello" @ root;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note!("hello"));
        warning!(e, "hello" @ { a_node };
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note!("hello"));
        info!(e, "hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            Some(5).into_iter().flat_map(|n| note!("hello {}", n)));
        debug!(e, "hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint!("hello" @ b_node);
            note_if!(Some(5), n => "hello {}", n));
        error!(e, "hello" @ a_node;
            issue!("hello {:?}", "hello");
            hint_if!(None::<String>, n => "hello {}", n);
            hint!("hello" @ b_node));
        warning!(e, "hello" @ a_node;
            issue_if!(Some(5), n => "hello {}", n);
            hint!("hello {:?}", "hello");
            hint!("hello" @ b_node));
        let n = Some(5);
        warning!(e, "hello {}" @? Some(a_node), 10;
            issue_if!(n => "hello {}", n);
            hint!("hello {:?}", "hello");
            hint!("hello" @? Some(b_node)));
    }
}