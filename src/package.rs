use std::io;
use std::marker::PhantomData;

use camino::Utf8Path;
use self_cell::self_cell;

use crate::compiler::Output;
use crate::diagnostics::Diagnostics;
use crate::error;
use crate::misc::PathPatriciaMap;
use crate::semantic::scope::Scope;
use crate::storage::root_set::RootSet;
use crate::syntax::nodes::ProgramTree;
use crate::syntax::PARSER;

/// A package is a self-contained unit of files which depend on one another, and are compiled
/// together.
///
/// A package contains the data-structures from all phases. To batch compile, we sequentially
/// generate each phase up to the output phase. To recompile (incremental), we update the AST using
/// tree-sitter, remove data from deleted and modified AST nodes, and re-insert data from created
/// and modified AST nodes.
///
/// ## Compilation Overview
///
/// Compilation is divided into phases, with each phase depending on the previous ones. Furthermore,
/// the data in each phase satisfies the following properties:
///
/// - **Order-irrelevant:** Doesn't matter which order we insert data as long as we insert the same
///   data. This implies commutativity ($a_p + b_p = b_p + a_p$) and associativity
///   ($(a_p + b_p) + c_p = a_p + (b_p + c_p)$).
/// - **Idempotent:** Doesn't matter how many times we insert the same data ($a_p + a_p = a_p$).
/// - **Monotonic:** Inserting more data in a phase will only insert more data in future phases,
///   removing data from a phase will only remove data from future phases. Every piece of data in a
///   phase can be traced to data in past phases (dependencies) and future phases (dependents);
///   the data will only be altered if one of the dependencies is altered, and if altered it will
///   only alter the dependents. ($a_p \subseteq b_p \iff a_{p+1} \subseteq b_{p+1}$)
/// - **Deterministic:** Given the same input, the output will always be the same.
///
/// Satisfying these properties enable incremental recompilation: we simply remove old data and
/// insert new data, and the result will be the same as if we never had the old data and never
/// didn't have the new data in the first place $C(a_s) - C(b_s) + C(c_s) = C((a + b - c)_s)$.
///
/// TODO: A package may also depend on a files outside of the package, and recompile if
///     those files change, but this isn't yet implemented.
pub struct Package(Repr);

self_cell!(
    struct Repr {
        owner: PackageSyntax,

        #[not_covariant]
        dependent: PackageSemantic,
    }

    impl {Debug}
);

#[derive(Debug)]
pub struct PackageSyntax {
    /// AST nodes
    sources: FileMap<ProgramTree>,
}

#[derive(Debug)]
pub struct PackageSemantic<'tree> {
    /// Diagnostics
    diagnostics: Diagnostics<'tree>,
    /// Definitions
    definitions: PackageDefinitions<'tree>,
    /// Expressions, including type definitions and trivially-inferred types (most types)
    expressions: PackageExpressions<'tree>,
    /// Type inference (re-declare the trivial types, and resolve constrained type holes)
    type_inference: PackageTypeInference<'tree>,
    /// Type check results
    type_check: PackageTypeCheck<'tree>,
    /// Re-printed source removing all type annotations, and inserting runtime type checks and guard
    /// code
    output: PackageOutput<'tree>
}

unsafe impl Send for Package {}

pub type FileMap<T> = PathPatriciaMap<T>;

#[derive(Debug)]
pub struct PackageDefinitions<'tree> {
    scopes: RootSet<'tree, Scope<'tree>>
}

#[derive(Debug)]
pub struct PackageExpressions<'tree> {
    _p: PhantomData<&'tree ()>,
}

#[derive(Debug)]
pub struct PackageTypeInference<'tree> {
    _p: PhantomData<&'tree ()>,
}

#[derive(Debug)]
pub struct PackageTypeCheck<'tree> {
    _p: PhantomData<&'tree ()>,
}

#[derive(Debug)]
pub struct PackageOutput<'tree> {
    _p: PhantomData<&'tree ()>,
}

#[derive(Debug)]
pub struct FileOutput {

}

impl Package {
    pub fn build(paths: impl Iterator<Item=impl AsRef<Utf8Path>>, output: &mut Output) -> Package {
        // ???: Move setting print_immediately to true to somewhere else? Where do we use it false?
        let diagnostics = Diagnostics::new(true);

        let sources = paths.filter_map(|path| {
            match PARSER.lock().parse_file(path.as_ref().as_std_path(), None, ()) {
                Ok(source) => Some((path, ProgramTree::try_from(source).unwrap())),
                Err(err) => {
                    error!(&diagnostics, "@ {}: failed to parse, {}", path.as_ref(), err);
                    None
                }
            }
        }).collect::<FileMap<ProgramTree>>();

        Package(Repr::new(
            PackageSyntax {
                sources
            },
            |syntax| {
                let definitions = PackageDefinitions::build(&sources, &diagnostics);
                let expressions = PackageExpressions::build(&sources, &diagnostics, &definitions);
                let type_inference = PackageTypeInference::build(&sources, &diagnostics, &definitions, &expressions);
                let type_check = PackageTypeCheck::build(&sources, &diagnostics, &definitions, &expressions, &type_inference);
                let output = PackageOutput::build(&sources, &diagnostics, &definitions, &expressions, &type_inference, &type_check);
                PackageSemantic {
                    diagnostics,
                    definitions,
                    expressions,
                    type_inference,
                    type_check,
                    output,
                }
            }
        ))
    }

    pub fn outputs(&self) -> impl Iterator<Item=(&Utf8Path, &FileOutput)> {
        todo!();
        std::iter::empty()
    }
}

impl FileOutput {
    pub fn write_to_file(&self, path: &Utf8Path) -> io::Result<()> {
        todo!()
    }
}