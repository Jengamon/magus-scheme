// This is the IR format that is compiled into Chunks.
//
// It is convertable to and from StackValues as well as Datum

use gc_arena::{Collect, Gc, Mutation};

use crate::{
    bytecode::{Chunk, ChunkPtr},
    environment::StackEnvironmentPtr,
};

/// Values that can be at the head of a list
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub enum ListHead<'gc> {
    /// Most of the time, this is the variant.
    Program(Gc<'gc, Program<'gc>>),

    // Keywords that we want to be able to detect, and handle as needed.
    // These are positionally dependant (everything except `Import`, `Include`,
    // and `IncludeCi` by virtue of usage)

    // Import and inclusion syntax
    Import,
    Only,
    Except,
    Prefix,
    Rename,
    Include,
    IncludeCi,
    // library syntax
    DefineLibrary,
    Export,
    // when explicitly used in defining libraries
    Begin,
    IncludeLibraryDeclarations,
    CondExpand,
    Library,
    And,
    Or,
    Not,
    // when used in cond-expand
    Else,
}

/// This pretty much corresponds directly with external representations.
///
/// This is distinct from GAst, so that:
/// - we can resolve abbreviations
/// - premark certain identifiers (`import`, `define-library`, and friends)
///   that are always available, and handle some symbol resolution ahead-of-time
/// - macros can generate this value w/o needing actual source (the source location of the macro is used
///   for this)
///
/// because of this, a program explicitly does *not* have any source information.
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub enum Program<'gc> {
    Integer(i64),
    Inexact(f64),
    // TODO complex numbers
    EmptyList,
    List {
        /// A list is generally the form of an invocation, and we want to detect anything
        /// dealing with the module system at this level (so the compiler can interpret these invocations)
        head: ListHead<'gc>,
        body: Vec<Gc<'gc, Program<'gc>>>,
    },
    DottedList {
        pre_dot: Vec<Gc<'gc, Program<'gc>>>,
        dot: Gc<'gc, Program<'gc>>,
    },
}

impl<'gc> Program<'gc> {}

/*
TODO Prevent the definition of macros of any primitive form, meaning:
define
lambda
if
set!
(when/if supported) include, include-ci

then provide Rust-side impls for the rest of the standard library (and/or mix it with
Scheme-impls)
*/

/// Definition for a "standard chunk"
pub type StandardChunk = Chunk<256>;
pub type StandardChunkPtr<'gc> = ChunkPtr<'gc, 256>;

pub type LibraryName = Vec<LibraryNameItem>;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibraryNameItem {
    Identifier(lasso::Spur),
    Integer(u64),
}

/// Interface for Scheme libraries defined in Rust
pub trait Module {
    // TODO native modules have 2 parts: a part that interacts with the compiler, and the
    // part that interacts with the interpreter. This should basically be something like the
    // `define-library` interface.
}

/// Container for Rust-defined modules
///
/// These can interact with Scheme data, but are *not* allowed to hold onto that data, signified
/// by *not* implementing [`Collect`].
pub struct World {}

// Compiles Programs into Chunks
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Compiler<'gc> {
    // The Compiler builds the definition of Scheme-defined modules
    // and uses those definitions to create an environment, which is part of
    // the output in addition to the chunk
    /// Environment used for compiling
    ///
    /// This is solely used for import statements by the compiler.
    /// The resulting environment as used by the interpreter as a starting point (so that
    /// the interpreter has *no clue* about the import system)
    pub env: StackEnvironmentPtr<'gc>,
    // TODO Add macro env for imports of macros
}

impl<'gc> Compiler<'gc> {
    /// Output a [`StandardChunk`] from an input program, importing anything necessary into
    /// [`Self::env`]
    pub fn compile(&mut self, mc: &Mutation<'gc>, program: Program<'gc>) -> StandardChunk {
        todo!()
    }
}
