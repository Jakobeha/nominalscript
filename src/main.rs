#![doc = include_str!("../README.md")]

/// Static analysis and type system
pub mod analyses;
/// Diagnostics (errors/warnings/etc) and logging
pub mod diagnostics;
/// Typed AST and wrapper for tree-sitter
pub mod ast;
/// Import resolution, exports. Also includes the compiled output
pub mod import_export;
/// Project datastructure which contains everything
mod project;
/// Utilities which could go in any crate
pub mod misc;
/// This is the file with the one function (+ wrappers) which transpiles everything.
///
/// It really is just one massive function.
/// Maybe I will end up refactoring out a lot of the functionality like I've already done for the
/// type analysis and scope resolution, so it will be less massive.
/// But honestly, it's just a really straightforward algorithm, and has a lot of cases to handle
/// for all of JS/TS's different expressions, but all of the cases are really straightforward
/// (with the less straightforward stuff in those other modules)
pub mod compile;

pub use project::*;

/// Run the program
fn main() {
    println!("Hello, world!");
}
