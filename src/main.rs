#![doc = include_str!("../README.md")]

/// Static analysis and type system
pub mod analyses;
/// Diagnostics (errors/warnings/etc) and logging
pub mod diagnostics;
/// Typed AST and wrapper for tree-sitter
pub mod ast;
/// Import resolution, exports. Also includes the compiled output
pub mod import_export;
/// Utilities which could go in any crate
pub mod misc;

/// Run the program
fn main() {
    println!("Hello, world!");
}
