use lazy_static::lazy_static;
use crate::ast::tree_sitter::TSQuery;
use tree_sitter_nominalscript::language_nominalscript;

const CREATE_ID_STR: &'static str = "
    (export_specifier (nominal_type_identifier) @nominal_export_id alias: (identifier)? @export_alias_id)
    (export_specifier (identifier) @value_export_id alias: (identifier)? @export_alias_id)
    (export_statement (nominal_type_declaration (nominal_type_identifier) @nominal_export_id))
    (export_statement (function_declaration (identifier) @value_export_id))
    (export_statement (function_signature (identifier) @value_export_id))
    (export_statement (lexical_declaration (variable_declarator (identifier) @value_export_id)))
    (export_statement (variable_declaration (variable_declarator (identifier) @value_export_id)))
";

const NOMINAL_TYPE_STR: &'static str = "
    (nominal_type_declaration) @type_decl
";

const PROGRAM_TO_EXTRACT_NOMINAL_TYPE_STR: &'static str = "
    (program
      (nominal_type_declaration
          (nominal_type_annotation (_) @nominal_type)))
";

const IMPORT_STR: &'static str = "
    (import_statement) @import
";

// Function signature = decl in our terminology, since we only care about the
// nominal type annotation
const FUNCTION_STR: &'static str = "
    (function_declaration) @function
       (generator_function_declaration) @function
       (function_signature) @function
";

const VALUE_STR: &'static str = "
    (variable_declarator) @value
";

lazy_static! {
    pub static ref CREATE_ID: TSQuery = TSQuery::new(language_nominalscript(), CREATE_ID_STR).unwrap();
    pub static ref NOMINAL_TYPE: TSQuery = TSQuery::new(language_nominalscript(), NOMINAL_TYPE_STR).unwrap();
    pub static ref PROGRAM_TO_EXTRACT_NOMINAL_TYPE: TSQuery = TSQuery::new(language_nominalscript(), PROGRAM_TO_EXTRACT_NOMINAL_TYPE_STR).unwrap();
    pub static ref IMPORT: TSQuery = TSQuery::new(language_nominalscript(), IMPORT_STR).unwrap();
    pub static ref FUNCTION: TSQuery = TSQuery::new(language_nominalscript(), FUNCTION_STR).unwrap();
    pub static ref VALUE: TSQuery = TSQuery::new(language_nominalscript(), VALUE_STR).unwrap();
}