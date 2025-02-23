// This is the IR format that is compiled into Chunks.
//
// It is convertable to and from StackValues as well as Datum

use std::{collections::HashMap, sync::Arc};

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc, Mutation};
use lasso::Interner;

use crate::{
    GAstNode, ValuePtr,
    bytecode::{Bytecode, Chunk, ChunkPtr, Constant, SourceData},
    environment::StackEnvironmentPtr,
    general_parser::GeneralParserError,
    interpreter::syntax::MacroPtr,
    lexer::LexerError,
};

/// Values that can be at the head of a list
#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
pub enum ListHead<'gc> {
    Program(Gc<'gc, Program<'gc>>),

    // Keywords that we want to be able to detect, and handle as needed.
    // (import ...) all show up before the actual content of a program, specifying requirements
    // and (define-library ...) are library definitions.
    //
    // Both must be top-level to have this behavior.
    Import,
    DefineLibrary,
}

pub type ProgramPtr<'gc> = Gc<'gc, Program<'gc>>;
/// This corresponds directly with external representations.
///
/// This is distinct from GAst, so that:
/// - we can resolve abbreviations
/// - premark certain identifiers (`import`, `define-library`, and friends)
///   that are always available, and handle some symbol resolution ahead-of-time
/// - macros can generate this value w/o needing actual source (the source location of the macro is used
///   for this)
///
/// because of this, a program explicitly does *not* have any source information.
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub enum ProgramData<'gc> {
    Integer(i64),
    Inexact(f64),
    // TODO complex numbers
    String(#[collect(require_static)] lasso::Spur),
    Symbol(#[collect(require_static)] lasso::Spur),
    Bool(bool),
    Char(char),
    // TODO bytevector
    Labeled {
        label: usize,
        item: ProgramPtr<'gc>,
    },
    LabelRef(usize),
    EmptyList,
    List {
        /// A list is generally the form of an invocation, and we want to detect anything
        /// dealing with the module system at this level (so the compiler can interpret these invocations)
        head: ListHead<'gc>,
        body: Vec<ProgramPtr<'gc>>,
    },
    DottedList {
        pre_dot: Vec<ProgramPtr<'gc>>,
        dot: ProgramPtr<'gc>,
    },
}
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct Program<'gc> {
    data: ProgramData<'gc>,
    // If this program came from source, this is the info
    #[collect(require_static)]
    source: SourceData,
}

// TODO Provide nice ways of "mutation" that create new programs
// (or just provide a visitor API that instead can return a value)

impl<'gc> Program<'gc> {
    pub fn new(data: ProgramData<'gc>, source: SourceData) -> Self {
        Self { data, source }
    }
}

pub trait ProgramDataTransform<T, C = ()> {
    fn visit_program_data(&mut self, program: &ProgramData<'_>, ctx: C) -> T {
        match program {
            ProgramData::Integer(int) => self.visit_integer(*int, ctx),
            ProgramData::Inexact(inexact) => self.visit_inexact(*inexact, ctx),
            ProgramData::String(string_id) => self.visit_string(*string_id, ctx),
            ProgramData::Symbol(symbol_id) => self.visit_symbol(*symbol_id, ctx),
            ProgramData::Bool(b) => self.visit_bool(*b, ctx),
            ProgramData::Char(c) => self.visit_char(*c, ctx),
            ProgramData::Labeled { label, item } => self.visit_labeled(*label, *item, ctx),
            ProgramData::LabelRef(label_ref) => self.visit_label_ref(*label_ref, ctx),
            ProgramData::EmptyList => self.visit_empty_list(ctx),
            ProgramData::List { head, body } => self.visit_list(*head, body.as_slice(), ctx),
            ProgramData::DottedList { pre_dot, dot } => {
                self.visit_dotted_list(pre_dot.as_slice(), *dot, ctx)
            }
        }
    }
    fn visit_program(&mut self, ptr: ProgramPtr<'_>, ctx: C) -> T {
        self.visit_program_data(&ptr.data, ctx)
    }
    fn visit_integer(&mut self, integer: i64, ctx: C) -> T;
    fn visit_inexact(&mut self, inexact: f64, ctx: C) -> T;
    fn visit_string(&mut self, string_id: lasso::Spur, ctx: C) -> T;
    fn visit_symbol(&mut self, symbol_id: lasso::Spur, ctx: C) -> T;
    fn visit_bool(&mut self, b: bool, ctx: C) -> T;
    fn visit_char(&mut self, c: char, ctx: C) -> T;
    fn visit_labeled(&mut self, label: usize, item: ProgramPtr<'_>, ctx: C) -> T;
    fn visit_label_ref(&mut self, label_ref: usize, ctx: C) -> T;
    fn visit_empty_list(&mut self, ctx: C) -> T;
    fn visit_list(&mut self, head: ListHead<'_>, body: &[ProgramPtr<'_>], ctx: C) -> T;
    fn visit_dotted_list(&mut self, pre_dot: &[ProgramPtr<'_>], dot: ProgramPtr<'_>, ctx: C) -> T;
}

// TODO If we use/had specialization, we maybe could provide default impls if T: Default

pub trait ParseProgram {
    type Error;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error>;
}

#[derive(thiserror::Error, Debug)]
pub enum StringProgramError {
    #[error("failed to parse code")]
    GeneralParse(Box<[GeneralParserError]>),
    #[error(transparent)]
    GAst(#[from] GAstProgramError),
}
impl<T: AsRef<str>> ParseProgram for T {
    type Error = StringProgramError;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error> {
        let gast = crate::general_parse(self);
        if !gast.errors().is_empty() {
            return Err(StringProgramError::GeneralParse(gast.into_errors().into()));
        }

        Ok(crate::Module::cast(gast.syntax())
            .expect("ICE: top-level code cannot be Module")
            .parse_program(mc, interner, case_insensitive)?)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum GAstProgramError {}
impl ParseProgram for crate::Module {
    type Error = GAstProgramError;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error> {
        todo!()
    }
}
// TODO impl IntoProgram for GAst types (Module, Datum, etc.)

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

/// Some macros are special, and are resolved at compile-time, given special access to the parts of
/// a `Chunk`, and can add instructions to them, but are unable to suspend, are only given shared
/// access to themself (rather than unique), and have no access to environments.
pub trait Primitive {
    /// Evaluate a macro, until it finishes
    ///
    /// # Parameters
    /// - `mc`: [`gc_arena`] mutation context
    /// - `args`: external representation corresponding to passed in syntax
    fn evaluate<'gc>(
        &self,
        mc: &Mutation<'gc>,
        compiler: &mut Compiler,
        interner: &mut lasso::Rodeo,
        world: &World,
        constants: &mut Vec<Constant>,
        // TODO Chunk constants
        // TODO Chunk lambdas
        args: &[Program<'gc>],
    ) -> anyhow::Result<Box<[Bytecode]>>;
}
// TODO we are very stringent, and might relax this in the future...
type ArcPrimitive = Arc<dyn Primitive + Sync + Send + 'static>;

// TODO contemplate Arc or Rc?
// ...benchmark to see?
// From the results, an Arc is a middle of the road solution
// that isn't as expensive to copy as a Box, but keeps us thread-safe
// (so World can still be Send)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub struct LibraryName(Arc<[LibraryNameItem]>);
impl FromIterator<LibraryNameItem> for LibraryName {
    fn from_iter<T: IntoIterator<Item = LibraryNameItem>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LibraryNameItem {
    Identifier(lasso::Spur),
    Integer(u64),
}
// FIXME when creating a Library name for *defining* a library
// reserve (prevent the definition of) libraries where the first element is
// either "scheme", "srfi" (reserved for report impl and SRFI impl respectively),
// or "magus" (reserved for use by "World" modules that want a unique namespace)
// TODO something we will *definitely* support is SRFI 1: List library
// cuz that's where `filter` is, but that should just show up as a Module that one can include
// in a world

/// Interface for Scheme libraries defined in Rust
pub trait Module {
    fn syntax<'gc>(&self, mc: &Mutation<'gc>, symbol: &str) -> Option<MacroPtr<'gc>>;
    fn primitive(&self, symbol: &str) -> Option<ArcPrimitive>;
    fn value<'gc>(&self, mc: &Mutation<'gc>, symbol: &str) -> Option<ValuePtr<'gc>>;
}

/// Container for Rust-defined modules
///
/// These can interact with Scheme data, but are *not* allowed to hold onto that data, signified
/// by *not* implementing [`Collect`].
///
/// This allows this struct to be shared across threads.
#[derive(Default, Clone)]
pub struct World {
    // basically a HashSet of <LibraryName, Arc<dyn Module + Send>>
    // We keep things thread-safe, so that the World can be sent across threads (common definitions
    // of modules and compiler primitives defined Rust-side)
    modules: FxHashMap<LibraryName, Arc<dyn Module + Send + Sync + 'static>>,
}

#[derive(thiserror::Error, Debug)]
#[error("library name already exists")]
pub struct LibraryNameExists;

impl World {
    pub fn insert(
        &mut self,
        name: LibraryName,
        module: impl Module + Send + Sync + 'static,
    ) -> Result<(), LibraryNameExists> {
        if self.modules.contains_key(&name) {
            return Err(LibraryNameExists);
        }

        self.modules.insert(name, Arc::new(module));
        Ok(())
    }

    pub fn library(&self, name: &LibraryName) -> Option<&(dyn Module + Send + Sync)> {
        self.modules.get(name).map(|m| m.as_ref())
    }

    pub fn forget(
        &mut self,
        name: &LibraryName,
    ) -> Option<Arc<dyn Module + Send + Sync + 'static>> {
        self.modules.remove(name)
    }
}

/// A container for Scheme-defined modules (using `define-library` at the top level)
#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct LocalWorld<'gc> {
    modules: HashMap<LibraryName, Gc<'gc, ()>>,
}

// Compiles Programs into Chunks
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Compiler<'gc> {
    // The Compiler builds the definition of Scheme-defined modules
    // and uses those definitions to create an environment, which is part of
    // the output in addition to the chunk
    /// Compilers store the modules that have been defined in source using `define-library`
    /// here.
    local_world: LocalWorld<'gc>,
}

#[derive(thiserror::Error, Debug)]
pub enum CompilerError {}

impl<'gc> Compiler<'gc> {
    /// Compile an list of programs into a [`Chunk`]
    ///
    /// # Parameters
    /// - `mc`
    /// - `interner`
    /// - `world`
    pub fn compile(
        &mut self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        world: &World,
        program: &[ProgramPtr<'gc>],
    ) -> Result<Chunk<'gc>, CompilerError> {
        // At the top-level, we interpret the list-heads of Import and
        // DefineLibrary, otherwise it is code to compile

        // once we encounter a piece that is interpreted as code,
        // we turn off the import system (import and define-library), and interpret
        // them as symbols.
        let mut library_system = true;

        todo!()
    }
}
