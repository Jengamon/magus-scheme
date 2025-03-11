use core::fmt;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    num::NonZero,
    rc::Rc,
    sync::Arc,
};

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    ValuePtr,
    bytecode::{Bytecode, Chunk, ChunkPtr, Constant, SourceData},
    environment::{Environment, StackEnvironmentPtr},
    interpreter::Includer,
    runtime::lambda::CompiledLambdaPtr,
};

mod program_parsers;
pub use program_parsers::GeneralParseErrors;

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

impl ListHead<'_> {
    pub fn into_symbol(self, interner: &mut lasso::Rodeo) -> Option<lasso::Spur> {
        let import = interner.get_or_intern_static("import");
        let define_library = interner.get_or_intern_static("define-library");
        match self {
            ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(_)) => {
                Some(match p.data {
                    ProgramData::Symbol(s) => s,
                    _ => unreachable!(),
                })
            }
            ListHead::Import => Some(import),
            ListHead::DefineLibrary => Some(define_library),
            _ => None,
        }
    }
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
/// - for set forms (syntax-rules, etc)
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
    pub data: ProgramData<'gc>,
    // If this program came from source, this is the info
    #[collect(require_static)]
    pub source: Option<SourceData>,
}

impl<'gc> Program<'gc> {
    pub fn display<R: lasso::Resolver>(
        program: ProgramPtr<'gc>,
        resolver: R,
    ) -> DisplayableProgram<'gc, R> {
        DisplayableProgram {
            program,
            resolver: Rc::new(resolver),
        }
    }
}

#[derive(Debug)]
pub struct DisplayableProgram<'gc, R: lasso::Resolver> {
    program: ProgramPtr<'gc>,
    resolver: Rc<R>,
}

impl<T: lasso::Resolver> fmt::Display for DisplayableProgram<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.program.data {
            ProgramData::Integer(i) => write!(f, "{i}"),
            ProgramData::Inexact(fl) => write!(f, "{fl}"),
            ProgramData::String(spur) => todo!(),
            ProgramData::Symbol(spur) => todo!(),
            ProgramData::Bool(_) => todo!(),
            ProgramData::Char(_) => todo!(),
            ProgramData::Labeled { label, item } => todo!(),
            ProgramData::LabelRef(_) => todo!(),
            ProgramData::EmptyList => todo!(),
            ProgramData::List { head, body } => todo!(),
            ProgramData::DottedList { pre_dot, dot } => todo!(),
        }
    }
}

// TODO Provide nice ways of "mutation" that create new programs
// (or just provide a visitor API that instead can return a value)

impl<'gc> Program<'gc> {
    pub fn new(data: ProgramData<'gc>, source: Option<SourceData>) -> Self {
        Self { data, source }
    }
}

pub trait ProgramVisitor {
    fn visit_program_data(&mut self, program: &ProgramData<'_>) {
        match program {
            ProgramData::Integer(int) => self.visit_integer(*int),
            ProgramData::Inexact(inexact) => self.visit_inexact(*inexact),
            ProgramData::String(string_id) => self.visit_string(*string_id),
            ProgramData::Symbol(symbol_id) => self.visit_symbol(*symbol_id),
            ProgramData::Bool(b) => self.visit_bool(*b),
            ProgramData::Char(c) => self.visit_char(*c),
            ProgramData::Labeled { label, item } => self.visit_labeled(*label, *item),
            ProgramData::LabelRef(label_ref) => self.visit_label_ref(*label_ref),
            ProgramData::EmptyList => self.visit_empty_list(),
            ProgramData::List { head, body } => self.visit_list(*head, body.as_slice()),
            ProgramData::DottedList { pre_dot, dot } => {
                self.visit_dotted_list(pre_dot.as_slice(), *dot)
            }
        }
    }
    fn visit_program(&mut self, ptr: ProgramPtr<'_>) {
        self.visit_program_data(&ptr.data)
    }
    fn visit_integer(&mut self, integer: i64) {
        let _ = integer;
    }
    fn visit_inexact(&mut self, inexact: f64) {
        let _ = inexact;
    }
    fn visit_string(&mut self, string_id: lasso::Spur) {
        let _ = string_id;
    }
    fn visit_symbol(&mut self, symbol_id: lasso::Spur) {
        let _ = symbol_id;
    }
    fn visit_bool(&mut self, b: bool) {
        let _ = b;
    }
    fn visit_char(&mut self, c: char) {
        let _ = c;
    }
    fn visit_labeled(&mut self, label: usize, item: ProgramPtr<'_>) {
        let _ = (label, item);
    }
    fn visit_label_ref(&mut self, label_ref: usize) {
        let _ = label_ref;
    }
    fn visit_empty_list(&mut self) {}
    fn visit_list(&mut self, head: ListHead<'_>, body: &[ProgramPtr<'_>]) {
        let _ = (head, body);
    }
    fn visit_dotted_list(&mut self, pre_dot: &[ProgramPtr<'_>], dot: ProgramPtr<'_>) {
        let _ = (pre_dot, dot);
    }
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

// TODO impl IntoProgram for GAst types (Module, Datum, etc.)

/*
NOTE Nah
TODO Prevent the definition of macros of any primitive form, meaning:
define
lambda
if
set!
(when/if supported) include, include-ci

then provide Rust-side impls for the rest of the standard library (and/or mix it with
Scheme-impls)
*/

pub struct SyntaxContext<'a, 'gc> {
    pub mc: &'a Mutation<'gc>,
    pub interner: &'a mut lasso::Rodeo,
    pub world: &'a World,
    constants: &'a mut Vec<Constant>,
    lambdas: &'a mut Vec<CompiledLambdaPtr<'gc>>,
    promises: &'a mut Vec<Box<[Bytecode]>>,
    upvalues: &'a mut usize,
}

impl<'gc> SyntaxContext<'_, 'gc> {
    pub fn push_constant(&mut self, c: Constant) -> usize {
        if let Some(p) = self.constants.iter().position(|constant| constant == &c) {
            p
        } else {
            let idx = self.constants.len();
            self.constants.push(c);
            idx
        }
    }

    pub fn add_lambda(&mut self, ptr: CompiledLambdaPtr<'gc>) -> usize {
        if let Some(p) = self.lambdas.iter().position(|lptr| Gc::ptr_eq(*lptr, ptr)) {
            p
        } else {
            let idx = self.lambdas.len();
            self.lambdas.push(ptr);
            idx
        }
    }

    // Internal method to create an upvalue reference
    pub(crate) fn add_upvalue(&mut self) -> usize {
        // Upvalues should be comparable to see if they are referencing the same out-of-scope value
        let idx = *self.upvalues;
        *self.upvalues += 1;
        idx
    }

    pub fn constants(&self) -> impl IntoIterator<Item = Constant> {
        self.constants.clone()
    }

    pub fn lambdas(&self) -> impl IntoIterator<Item = CompiledLambdaPtr<'gc>> {
        self.lambdas.clone()
    }

    pub fn promises(&self) -> impl IntoIterator<Item = Box<[Bytecode]>> {
        self.promises.clone()
    }

    pub fn upvalues(&self) -> usize {
        *self.upvalues
    }
}

impl<'gc> std::ops::Deref for SyntaxContext<'_, 'gc> {
    type Target = Mutation<'gc>;
    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

pub trait Transformer<'gc>: Syntax {}
impl<'gc, T: Syntax + Collect<'gc>> Transformer<'gc> for T {}
pub type TransformerPtr<'gc> = Gc<'gc, dyn Transformer<'gc>>;

/// The result of a syntax evaluation
pub enum SyntaxReturn<'gc> {
    /// This is code to add to the compiling chunk
    Code(Box<[Bytecode]>),
    /// This is a transformer that can be used by a syntax item
    /// to transform code
    Transformer(Gc<'gc, dyn Transformer<'gc>>),
}

impl<'gc> SyntaxReturn<'gc> {
    pub fn into_bytecode(self) -> impl IntoIterator<Item = Bytecode> + use<> {
        match self {
            Self::Code(code) => code,
            // A transformer's runtime value is (eq? '())
            Self::Transformer(_) => Box::from([Bytecode::PushNull]),
        }
    }

    pub fn into_transformer(self) -> Option<TransformerPtr<'gc>> {
        match self {
            Self::Code(_) => None,
            Self::Transformer(trans) => Some(trans),
        }
    }
}

/// this is a macro, it interacts with compiler code as we compile
/// `define-syntax`, for example, is a primitive that creates a primitive and inserts it into
/// the local world
pub trait Syntax: std::fmt::Debug {
    /// Evaluate a macro, until it finishes
    ///
    /// # Parameters
    /// - `mc`: [`gc_arena`] mutation context
    /// - `args`: external representation corresponding to passed in syntax
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>>;

    /// Registers this syntax item as a definition
    fn is_definition(&self, _ptr: ProgramPtr<'_>) -> bool {
        false
    }

    /// Registers this syntax item as a container
    ///
    /// Affects how definitions are registered. A container syntax is considered a
    /// definition if all of it's components are definitions (or containers of only definitions)
    fn is_container(&self, _ptr: ProgramPtr<'_>) -> bool {
        false
    }
}
// TODO we are very stringent, and might relax this in the future...
pub type ArcSyntax = Arc<dyn Syntax + Sync + Send + 'static>;

// TODO contemplate Arc or Rc?
// ...benchmark to see?
// From the results, an Arc is a middle of the road solution
// that isn't as expensive to copy as a Box, but keeps us thread-safe
// (so World can still be Send)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub struct LibraryName(Arc<[LibraryNameItem]>);
impl LibraryName {
    #[inline]
    pub fn is_valid(&self) -> bool {
        !self.0.is_empty()
    }
}
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

#[macro_export]
macro_rules! library_name {
    ($intern:expr => $( $s:tt )+) => {
        [
            $(
                library_name!(@parsing $intern => $s)
            ),+
        ]
    };
    (@parsing $intern:expr => $s:ident) => {
        $crate::compiler::LibraryNameItem::Identifier($intern.get_or_intern_static(stringify!($s)))
    };
    (@parsing $_:expr => $s:literal) => {
        $crate::compiler::LibraryNameItem::Integer($s)
    }
}

/// Interface for Scheme libraries defined in Rust
pub trait Module {
    /// all symbols defined by this module (used when *everything* is imported)
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> HashSet<lasso::Spur>;
    fn syntax(&self, interner: &mut lasso::Rodeo, symbol: lasso::Spur) -> Option<ArcSyntax> {
        let _ = (interner, symbol);
        None
    }
    fn value<'gc>(&self, mc: &Mutation<'gc>, symbol: &str) -> Option<ValuePtr<'gc>> {
        let _ = (mc, symbol);
        None
    }
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

/// The result of a `define-library`
#[derive(Debug)]
struct SchemeLibrary<'gc> {
    exported_items: HashMap<lasso::Spur, ValuePtr<'gc>>,
}

#[allow(unsafe_code)]
unsafe impl<'gc> Collect<'gc> for SchemeLibrary<'gc> {
    fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
        for value in self.exported_items.values() {
            value.trace(cc);
        }
    }
}

/// A container for Scheme-defined modules (using `define-library` at the top level)
#[derive(Collect, Debug, Default)]
#[collect(no_drop)]
struct LocalWorld<'gc> {
    modules: HashMap<LibraryName, SchemeLibrary<'gc>>,
}

type SyntaxDef = HashMap<lasso::Spur, ArcSyntax>;
type VariableDef = fxhash::FxHashSet<lasso::Spur>;
#[derive(Debug, Clone, Default)]
pub struct Scope {
    args: Rc<[lasso::Spur]>,
    rest: Option<lasso::Spur>,

    upvalues: Rc<RefCell<fxhash::FxHashMap<Option<usize>, usize>>>,
    variables_defined: Rc<RefCell<fxhash::FxHashSet<lasso::Spur>>>,
}

#[derive(thiserror::Error, Debug)]
#[error("name not defined in scope")]
pub struct Undefined;

impl Scope {
    // A scope should be able to set an argument as an upvalue which changes the code emitted when it is defined
    // so that the code emitted when referencing it can also change to enable this

    /// Define an argument as an upvalue
    /// - `Some(index)`: argument at index
    /// - `None`: rest argument
    pub(crate) fn set_upvalue(&mut self, index: Option<usize>, upvalue_index: usize) {
        self.upvalues.borrow_mut().insert(index, upvalue_index);
    }

    /// Get the upvalue index of an argument if available
    pub fn is_upvalue(&self, symbol: lasso::Spur) -> Result<Option<usize>, Undefined> {
        if self.rest.is_some_and(|r| r == symbol) {
            Ok(self.upvalues.borrow().get(&None).copied())
        } else if let Some(pos) = self.args.iter().position(|s| s == &symbol) {
            Ok(self.upvalues.borrow().get(&Some(pos)).copied())
        } else {
            Err(Undefined)
        }
    }
}

slotmap::new_key_type! {
pub struct TransformerKey;
pub struct ProgramKey;
}
/// Compiler Stash
#[derive(Debug, Default)]
pub struct Stash<'gc> {
    // TODO Include a knob system to automatically drop unused transfomers and programs
    transformers: slotmap::SlotMap<TransformerKey, TransformerPtr<'gc>>,
    programs: slotmap::SlotMap<ProgramKey, ProgramPtr<'gc>>,
}
// TODO Allow storing and accessing these
#[allow(unsafe_code)]
unsafe impl<'gc> Collect<'gc> for Stash<'gc> {
    fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
        macro_rules! trace_slotmap {
            ($field:ident) => {
                for value in self.$field.values() {
                    value.trace(cc);
                }
            };
        }

        trace_slotmap!(transformers);
        trace_slotmap!(programs);
    }
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
    // macros that have been defined
    #[collect(require_static)]
    syntax_items: SyntaxDef,
    // variables that are in scope
    #[collect(require_static)]
    global_variables_defined: VariableDef,
    // arguments that are in scope
    #[collect(require_static)]
    scopes: Vec<Scope>,

    // checkpoints store macro and variable definitions
    #[collect(require_static)]
    checkpoints: Vec<(SyntaxDef, VariableDef, Vec<Scope>)>,
    // import environments in-scope
    environments: Vec<StackEnvironmentPtr<'gc>>,
    // which environment to use
    env_ptr: usize,

    // stash that can be used by macros to store things
    pub stash: Stash<'gc>,
}
#[derive(Debug, Clone, Copy)]
pub struct Checkpoint(usize);
#[derive(Debug, Clone, Copy)]
pub struct EnvironmentSpec(NonZero<usize>);
#[derive(Debug, Clone, Copy)]
pub enum Arg {
    Index {
        /// Scope is the 0 inverse-indexed scope (where 0 is current scope, 1 is parent scope, etc)
        scope: usize,
        index: usize,
    },
    Rest {
        /// Scope is the 0 inverse-indexed scope (where 0 is current scope, 1 is parent scope, etc)
        scope: usize,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum CompileError {
    #[error("a label was encountered in code")]
    Labeled(Option<SourceData>),
    #[error("a label ref was encountered in code")]
    LabelRef(Option<SourceData>),
    #[error("an empty list was encountered in code")]
    EmptyList(Option<SourceData>),
    #[error("a dotted list was encountered in code")]
    DottedList(Option<SourceData>),
    #[error("a macro encountered an error: {0}")]
    Macro(Arc<anyhow::Error>, Option<SourceData>),
    #[error(transparent)]
    ImportSet(#[from] ImportSetError),
    #[error(transparent)]
    Import(#[from] ImportError),
}

#[derive(Debug, Clone)]
pub enum ImportSet {
    /// Import all exports of a modules
    Name(LibraryName),
    /// Import only certain symbols from a module
    Only {
        set: Arc<ImportSet>,
        symbols: Arc<[lasso::Spur]>,
    },
    Except {
        set: Arc<ImportSet>,
        symbols: Arc<[lasso::Spur]>,
    },
    // TODO other import specs: prefix, rename
}

#[derive(thiserror::Error, Debug)]
pub enum ImportSetError {
    #[error("not an import set")]
    NotImportSet(Option<SourceData>),
    #[error("not a valid library name")]
    InvalidLibraryName(Option<SourceData>),
    #[error("not a symbol")]
    NotASymbol(Option<SourceData>),
}
impl ImportSet {
    fn as_library_name_item(ptr: ProgramPtr<'_>) -> Option<LibraryNameItem> {
        match ptr.data {
            ProgramData::Integer(i) if i >= 0 => Some(LibraryNameItem::Integer(i as u64)),
            ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(s)),
            _ => None,
        }
    }

    fn as_library_name(ptr: ProgramPtr<'_>, interner: &mut lasso::Rodeo) -> Option<LibraryName> {
        // We allow creating the empty library name as a "secret from code" module (as we compile
        // imports from code, we reject code imports from the '() module, which the Rust side
        // is completely fine with)

        match &ptr.data {
            ProgramData::List { head, body } => {
                let mut parts = vec![];
                match head {
                    ListHead::Program(p) => {
                        parts.push(Self::as_library_name_item(*p)?);
                    }
                    ListHead::Import => {
                        parts.push(LibraryNameItem::Identifier(
                            interner.get_or_intern_static("import"),
                        ));
                    }
                    ListHead::DefineLibrary => {
                        parts.push(LibraryNameItem::Identifier(
                            interner.get_or_intern_static("define-library"),
                        ));
                    }
                };
                for p in body {
                    parts.push(Self::as_library_name_item(*p)?);
                }
                Some(LibraryName(Arc::from(parts.as_slice())))
            }
            _ => None,
        }
    }

    pub fn convert(
        ptr: ProgramPtr<'_>,
        interner: &mut lasso::Rodeo,
    ) -> Result<Self, ImportSetError> {
        let only = interner.get_or_intern_static("only");
        let except = interner.get_or_intern_static("except");
        let prefix = interner.get_or_intern_static("prefix");
        let rename = interner.get_or_intern_static("rename");

        match &ptr.data {
            ProgramData::List { head, body } => match head {
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == only)
                        && body.len() >= 2 =>
                {
                    // read first body param as an import set, and the rest *must* be symbols
                    let source = ImportSet::convert(body[0], interner)?;
                    let symbols = body
                        .iter()
                        .skip(1)
                        .map(|p| match &p.data {
                            ProgramData::Symbol(s) => Ok(*s),
                            _ => Err(ImportSetError::NotASymbol(p.source)),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(ImportSet::Only {
                        set: Arc::new(source),
                        symbols: Arc::from(symbols.as_slice()),
                    })
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == only) => {
                    Err(ImportSetError::NotImportSet(ptr.source))
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == except)
                        && body.len() > 2 =>
                {
                    // read first body param as an import set, and the rest *must* be symbols
                    let source = ImportSet::convert(body[0], interner)?;
                    let symbols = body
                        .iter()
                        .skip(1)
                        .map(|p| match &p.data {
                            ProgramData::Symbol(s) => Ok(*s),
                            _ => Err(ImportSetError::NotASymbol(p.source)),
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(ImportSet::Except {
                        set: Arc::new(source),
                        symbols: Arc::from(symbols.as_slice()),
                    })
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == except) => {
                    Err(ImportSetError::NotImportSet(ptr.source))
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == prefix) =>
                {
                    // read first body param as an import set, and the second as a prefix to prepend
                    // can only specify those 2
                    todo!()
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == rename) =>
                {
                    // read first body param as an import set, and the rest *must* be pairs of sym1 and sym2
                    todo!()
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(_) | ProgramData::Integer(_))
                        && body.iter().all(|p| {
                            matches!(&p.data, ProgramData::Symbol(_) | ProgramData::Integer(_))
                        }) =>
                {
                    Ok(Self::Name(
                        Self::as_library_name(ptr, interner)
                            .ok_or(ImportSetError::InvalidLibraryName(ptr.source))?,
                    ))
                }
                ListHead::Import | ListHead::DefineLibrary => Ok(Self::Name(
                    Self::as_library_name(ptr, interner)
                        .ok_or(ImportSetError::InvalidLibraryName(ptr.source))?,
                )),
                _ => Err(ImportSetError::NotImportSet(ptr.source)),
            },
            _ => Err(ImportSetError::NotImportSet(ptr.source)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LibraryDeclaration<'gc> {
    Import(Rc<[ImportSet]>),
    IncludeLibraryDeclarations(Rc<[Rc<str>]>),
    Include {
        filenames: Rc<[Rc<str>]>,
        case_insensitive: bool,
    },
    Begin(Rc<[ProgramPtr<'gc>]>),
}

impl<'gc> LibraryDeclaration<'gc> {
    pub fn convert(interner: &mut lasso::Rodeo, ptr: ProgramPtr<'gc>) -> Option<Self> {
        // let import = interner.get_or_intern_static("import");
        // let include_library_definitions =
        //     interner.get_or_intern_static("include-library-definitions");
        // let include = interner.get_or_intern_static("include");
        // let include_ci = interner.get_or_intern_static("include-ci");
        // let begin = interner.get_or_intern_static("begin");
        let _ = (interner, ptr);
        None
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ImportError {
    #[error("invalid library import")]
    InvalidImport,
    #[error("library not found")]
    LibraryNotFound(LibraryName),
    // TODO store module name in error
    #[error("name not found in module: {name}")]
    NameNotFound { name: Box<str> },
    #[error("failed to define symbol")]
    FailedToDefine(lasso::Spur),
}

#[derive(thiserror::Error, Debug)]
pub enum DefineLibraryError {
    #[error("empty library name")]
    EmptyLibraryName,
    #[error("attempted to define reserved library name")]
    ReservedName(LibraryName),
    #[error("attempted to define an existing library")]
    AlreadyExists(LibraryName),
}

impl<'gc> Compiler<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            local_world: LocalWorld::default(),
            syntax_items: Default::default(),
            global_variables_defined: Default::default(),
            scopes: Default::default(),
            checkpoints: Default::default(),
            // The very first environment pointer is always the default environment
            environments: vec![Gc::new(mc, RefLock::new(Environment::new(mc, None)))],
            env_ptr: 0,
            stash: Stash::default(),
        }
    }

    /// Compile an list of programs into a [`Chunk`]
    ///
    /// # Parameters
    /// - `mc`
    /// - `interner`
    /// - `world`
    /// - `includer`: an [`Includer`] for external files
    /// - `program`: pointers to the members of a program
    pub fn compile(
        &mut self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        world: &World,
        includer: &impl Includer,
        programs: impl IntoIterator<Item = ProgramPtr<'gc>>,
    ) -> Result<ChunkPtr<'gc>, CompileError> {
        let mut programs = programs.into_iter().peekable();

        // TODO Read programs in order, while the program is List with head Import or DefineLibrary
        // interp as library system, otherwise, compile and turn off lib system (use compile_code)

        // Module system loop
        loop {
            match programs.peek() {
                Some(p)
                    if matches!(
                        p.data,
                        ProgramData::List {
                            head: ListHead::Import,
                            ..
                        }
                    ) =>
                {
                    // import code, so read carefully
                    let program = programs.next().unwrap();
                    let ProgramData::List { body, .. } = &program.data else {
                        unreachable!();
                    };

                    let sets: Result<Vec<_>, _> = body
                        .iter()
                        .map(|p| ImportSet::convert(*p, interner))
                        .collect();

                    for set in sets? {
                        self.import(mc, interner, world, &set, true)?;
                    }
                }
                Some(p)
                    if matches!(
                        p.data,
                        ProgramData::List {
                            head: ListHead::DefineLibrary,
                            ..
                        }
                    ) =>
                {
                    // define-library code, so read carefully
                    let _program = programs.next().unwrap();
                    // TODO the bodies of define-library become library declarations
                }
                _ => break,
            }
        }

        // compile code loop
        let mut constants = Vec::new();
        let mut lambdas = Vec::new();
        let mut promises = Vec::new();
        let mut upvalues = 0;
        let mut context = SyntaxContext {
            mc,
            interner,
            world,
            constants: &mut constants,
            lambdas: &mut lambdas,
            promises: &mut promises,
            upvalues: &mut upvalues,
        };
        let mut code = vec![];
        let mut labels = FxHashMap::default();
        for program in programs {
            if let Some(source) = program.source {
                labels.insert(code.len(), source);
            }
            code.extend(self.compile_code(&mut context, program)?.into_bytecode());
        }

        // TODO Generate labels

        // when we create our chunk, our import env is *always* the initial default environment
        Ok(Chunk::new(
            mc,
            code,
            constants,
            lambdas,
            promises,
            upvalues,
            self.default_environment_ptr(),
            labels,
        ))
    }

    /// Compile code in the current context
    ///
    /// Is meant for the implementations of macros ([`Syntax`])
    pub fn compile_code(
        &mut self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        program: ProgramPtr<'gc>,
    ) -> Result<SyntaxReturn<'gc>, CompileError> {
        macro_rules! simple_constant {
            ($data:expr => $name:ident) => {{
                let index = ctx.push_constant(Constant::$name($data));
                Ok(SyntaxReturn::Code(Box::from([Bytecode::PushConst {
                    index,
                }])))
            }};
        }
        match &program.data {
            ProgramData::Integer(i) => {
                simple_constant!(*i => Number)
            }
            ProgramData::Inexact(f) => {
                simple_constant!(*f => Inexact)
            }
            ProgramData::String(spur) => {
                let index =
                    ctx.push_constant(Constant::String(Arc::from(ctx.interner.resolve(spur))));
                Ok(SyntaxReturn::Code(Box::from([Bytecode::PushConst {
                    index,
                }])))
            }
            ProgramData::Symbol(spur) => {
                if let Some(arg) = self.is_argument(*spur) {
                    match arg {
                        Arg::Index { scope, index } if scope != 0 => {
                            // Find the argument scope, and define upvalues
                            let Some(argument_scope) = self.scopes.iter_mut().rev().nth(scope)
                            else {
                                unreachable!("[ICE] upvalue scope error");
                            };
                            let upvalue_index =
                                if let Ok(Some(upv)) = argument_scope.is_upvalue(*spur) {
                                    upv
                                } else {
                                    // Create an upvalue pointing at the requisite scope...
                                    let upvalue_index = ctx.add_upvalue();
                                    argument_scope.set_upvalue(Some(index), upvalue_index);
                                    upvalue_index
                                };

                            return Ok(SyntaxReturn::Code(Box::from([Bytecode::FetchUpvalue {
                                index: upvalue_index,
                            }])));
                        }
                        Arg::Rest { scope } if scope != 0 => {
                            // Find the argument scope, and define upvalues
                            let Some(argument_scope) = self.scopes.iter_mut().rev().nth(scope)
                            else {
                                unreachable!("[ICE] upvalue scope error");
                            };
                            let upvalue_index =
                                if let Ok(Some(upv)) = argument_scope.is_upvalue(*spur) {
                                    upv
                                } else {
                                    // Create an upvalue pointing at the requisite scope...
                                    let upvalue_index = ctx.add_upvalue();
                                    argument_scope.set_upvalue(None, upvalue_index);
                                    upvalue_index
                                };

                            return Ok(SyntaxReturn::Code(Box::from([Bytecode::FetchUpvalue {
                                index: upvalue_index,
                            }])));
                        }
                        // Otherwise, handle like a "normal" reference
                        _ => {}
                    };
                }

                Ok(SyntaxReturn::Code(Box::from([Bytecode::Reference {
                    symbol: *spur,
                }])))
            }
            ProgramData::Bool(b) => Ok(SyntaxReturn::Code(Box::from([Bytecode::PushBool {
                bool: *b,
            }]))),
            ProgramData::Char(c) => {
                simple_constant!(*c => Char)
            }
            ProgramData::Labeled { .. } => Err(CompileError::Labeled(program.source)),
            ProgramData::LabelRef(_) => Err(CompileError::LabelRef(program.source)),
            ProgramData::EmptyList => Err(CompileError::EmptyList(program.source)),
            ProgramData::List { head, body } => {
                // At this point, import and define-library don't have a special meaning anymore, so just interpret them as
                // their corresponding symbols
                let head_symbol = head.into_symbol(ctx.interner);
                if let Some(mcr) = head_symbol.and_then(|sym| self.get_macro(sym)) {
                    mcr.evaluate(ctx, self, self._current_env(), body)
                        .map_err(|e| CompileError::Macro(Arc::new(e), program.source))
                } else {
                    let mut code = vec![];
                    let args = body.len();
                    for res in body.iter().map(|program| self.compile_code(ctx, *program)) {
                        code.extend(res?.into_bytecode());
                    }

                    match head {
                        ListHead::Program(program) => {
                            code.extend(self.compile_code(ctx, *program)?.into_bytecode());
                        }
                        // head_symbol is Some(spur) where spur is the symbol we want
                        ListHead::DefineLibrary | ListHead::Import => {
                            code.push(Bytecode::Reference {
                                symbol: head_symbol.unwrap(),
                            });
                        }
                    };

                    code.push(Bytecode::Call { args });
                    Ok(SyntaxReturn::Code(code.into()))
                }
            }
            ProgramData::DottedList { .. } => Err(CompileError::DottedList(program.source)),
        }
    }

    /// Interpret an import set in the current environment
    ///
    /// # Parameters
    /// - `mc`
    /// - `interner`
    /// - `world`
    /// - `import_set`
    /// - `from_code`: denotes an import set as being from code rather than from Rust
    pub fn import(
        &mut self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        world: &World,
        import_set: &ImportSet,
        from_code: bool,
    ) -> Result<(), ImportError> {
        // - import: reads import sets, then searches local world (once implemented), then world, for the requisite module
        //   and importing the names as defined by spec
        // TODO Lookup in local libraries first
        // then if failed, look up in World

        fn get_library_name(imp: &ImportSet) -> Option<LibraryName> {
            match imp {
                ImportSet::Name(ln) => Some(ln.clone()),
                ImportSet::Only { set, .. } => get_library_name(set.as_ref()),
                ImportSet::Except { set, .. } => get_library_name(set.as_ref()),
            }
        }

        macro_rules! import_lib {
            (full $library_name:expr) => {
                    if let Some(modl) = world.library($library_name) {
                        for symbol in modl.all_symbols(interner) {
                            // try  to import as a syntax, then as a value, and fail the module if
                            // a name doesn't exist
                            if let Some(syntax) = modl.syntax(interner, symbol) {
                                // Define a macro in scope
                                self.define_macro(symbol, syntax);
                            } else if let Some(val) = modl.value(mc, interner.resolve(&symbol)) {
                                // freeze a module imported value (TODO check how this actually impacts things)
                                self._current_env()
                                    .borrow_mut(mc)
                                    .define(mc, symbol, val, true).map_err(|_| ImportError::FailedToDefine(symbol))?;
                            } else {
                                Err(ImportError::NameNotFound{ name: Box::from(interner.resolve(&symbol))})?
                            }
                        }
                        Ok(())
                    } else {
                        Err(ImportError::LibraryNotFound($library_name.clone()))
                    }
            };
            (only $set:expr, $symbols:expr) => {
                {
                    let Some(library_name) = get_library_name($set) else {
                        unreachable!("[ICE] import set did not specify a library name");
                    };
                    if let Some(modl) = world.library(&library_name) {
                        for symbol in $symbols.iter().copied() {
                            // dbg!(&symbol);
                            // try to import as a syntax, then as a value, and fail the module if
                            // a name doesn't exist
                            if let Some(syntax) = modl.syntax(interner, symbol) {
                                // Define a macro in scope
                                self.define_macro(symbol, syntax);
                            } else if let Some(val) = modl.value(mc, interner.resolve(&symbol)) {
                                // freeze a module imported value (TODO check how this actually impacts things)
                                self._current_env()
                                    .borrow_mut(mc)
                                    .define(mc, symbol, val, true).map_err(|_| ImportError::FailedToDefine(symbol))?;
                            } else {
                                Err(ImportError::NameNotFound{ name: Box::from(interner.resolve(&symbol))})?
                            }
                        }
                        Ok(())
                    } else {
                        Err(ImportError::LibraryNotFound(library_name.clone()))
                    }
                }
            };
        }

        if from_code {
            match import_set {
                ImportSet::Name(library_name) if library_name.is_valid() => {
                    import_lib!(full library_name)
                }
                ImportSet::Only { set, symbols } => {
                    import_lib!(only set, symbols)
                }
                ImportSet::Except { set, symbols } => todo!(),
                _ => Err(ImportError::InvalidImport),
            }
        } else {
            match import_set {
                ImportSet::Name(library_name) => {
                    import_lib!(full library_name)
                }
                ImportSet::Only { set, symbols } => {
                    import_lib!(only set, symbols)
                }
                ImportSet::Except { set, symbols } => todo!(),
            }
        }
    }

    /// Interpret a library definition into a given LocalWorld
    ///
    /// # Parameters
    /// - `mc`
    /// - `name`: library name to register
    /// - `worlds`: [`LocalWorld`] and [`World`] to use
    /// - `interner`:
    /// - `is_native`: will reserved names be allowed through?
    /// - `library_decls`: library declarations
    fn define_library(
        mc: &Mutation<'gc>,
        name: &LibraryName,
        worlds: (&mut LocalWorld<'gc>, &World),
        interner: &mut lasso::Rodeo,
        includer: &impl Includer,
        from_code: bool,
        library_decls: impl IntoIterator<Item = LibraryDeclaration<'gc>>,
    ) -> Result<(), DefineLibraryError> {
        // The '() module is private from Scheme code
        if !name.is_valid() {
            return Err(DefineLibraryError::EmptyLibraryName);
        }

        // We reserve all modules name with the first component 'scheme, 'srfi, and 'magus
        let reserved_starts = ["scheme", "srfi", "magus"].map(|s| interner.get_or_intern_static(s));
        if from_code
            && matches!(name.0[0], LibraryNameItem::Identifier(ref id) if reserved_starts.contains(id))
        {
            return Err(DefineLibraryError::ReservedName(name.clone()));
        }

        // If a name can be found, that is *also* an error
        let (local_world, world) = worlds;
        if local_world.modules.contains_key(name) || world.modules.contains_key(name) {
            return Err(DefineLibraryError::AlreadyExists(name.clone()));
        }

        // TODO something we will *definitely* support is SRFI 1: List library
        // cuz that's where `filter` is, but that should just show up as a Module that one can include
        // in a world

        // - define-library: goes through the definition, interpreting the heads as keywords
        //   then creates a module in the local world for that name (errors before interp if the name is already
        //   taken by either a previous define-library or the world)
        todo!()
    }

    /// Checks if an expression is considered a definition by Scheme
    pub fn is_definition(&self, program: ProgramPtr<'gc>) -> bool {
        let definition_symbols = self
            .syntax_items
            .iter()
            .filter_map(|(k, syn)| syn.is_definition(program).then_some(*k))
            .collect::<fxhash::FxHashSet<_>>();
        let container_symbols = self
            .syntax_items
            .iter()
            .filter_map(|(k, syn)| syn.is_container(program).then_some(*k))
            .collect::<fxhash::FxHashSet<_>>();

        match &program.data {
            ProgramData::List { head, body } => {
                matches!(head, ListHead::Program(p) if match p.data {
                    ProgramData::Symbol(s) if definition_symbols.contains(&s) => true,
                    ProgramData::Symbol(s)
                        if container_symbols.contains(&s)
                            && body.iter().all(|bp| self.is_definition(*bp)) =>
                    {
                        true
                    }
                    _ => false,
                })
            }
            _ => false,
        }
    }

    /// Used internally to get the current import environment
    fn _current_env(&self) -> StackEnvironmentPtr<'gc> {
        self.environments[self.env_ptr]
    }

    pub fn new_environment(
        &mut self,
        mc: &Mutation<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
    ) -> EnvironmentSpec {
        let new_env = Gc::new(mc, RefLock::new(Environment::new(mc, Some(import_env))));
        let Some(nzp) = NonZero::new(self.environments.len()) else {
            unreachable!()
        };
        self.environments.push(new_env);
        EnvironmentSpec(nzp)
    }

    /// Helper function for pushing arguments to certain names (while also handling upvalues)
    pub fn lambda_prelude(&self) -> impl IntoIterator<Item = Bytecode> {
        // This should be called when all upvalues are known
        if let Some(argument_scope) = self.scopes.last() {
            dbg!(argument_scope);

            argument_scope
                .args
                .iter()
                .copied()
                .enumerate()
                .flat_map(|(index, symbol)| {
                    if let Some(upv) = argument_scope.upvalues.borrow().get(&Some(index)) {
                        vec![
                            Bytecode::FetchArg { index },
                            Bytecode::SetUpvalue { index: *upv },
                            Bytecode::Define { symbol },
                        ]
                    } else {
                        vec![Bytecode::FetchArg { index }, Bytecode::Define { symbol }]
                    }
                })
                .chain(
                    argument_scope
                        .rest
                        .map(|symbol| {
                            if let Some(upv) = argument_scope.upvalues.borrow().get(&None) {
                                vec![
                                    Bytecode::FetchRest,
                                    Bytecode::SetUpvalue { index: *upv },
                                    Bytecode::Define { symbol },
                                ]
                            } else {
                                vec![Bytecode::FetchRest, Bytecode::Define { symbol }]
                            }
                        })
                        .unwrap_or_default(),
                )
                .collect::<Vec<_>>()
        } else {
            Vec::default()
        }
    }

    /// Definition overrides for upvalues are handled here
    pub fn lambda_postlude(&self) -> impl IntoIterator<Item = Bytecode> {
        if let Some(argument_scope) = self.scopes.last() {
            // If a variable name is the same as an upvalue's name, we Reference the variable,
            // then SetUpvalue to that reference
            argument_scope
                .variables_defined
                .borrow()
                .iter()
                .flat_map(|vn| {
                    if let Some(up_index) = argument_scope
                        .args
                        .iter()
                        .position(|s| s == vn)
                        .and_then(|pos| argument_scope.upvalues.borrow().get(&Some(pos)).copied())
                    {
                        vec![
                            Bytecode::Reference { symbol: *vn },
                            Bytecode::SetUpvalue { index: up_index },
                            Bytecode::Pop,
                        ]
                    } else if let Some(up_index) = argument_scope
                        .rest
                        .filter(|s| s == vn)
                        .and_then(|_| argument_scope.upvalues.borrow().get(&None).copied())
                    {
                        vec![
                            Bytecode::Reference { symbol: *vn },
                            Bytecode::SetUpvalue { index: up_index },
                            Bytecode::Pop,
                        ]
                    } else {
                        // just a normal variable
                        vec![]
                    }
                })
                .collect()
        } else {
            Vec::default()
        }
    }

    /// Helper for a hygenic context
    pub fn hygenic<T>(
        &mut self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        f: impl FnOnce(&mut SyntaxContext<'_, 'gc>, &mut Compiler<'gc>, StackEnvironmentPtr<'gc>) -> T,
    ) -> T {
        let checkpoint = self.checkpoint();
        let new_env = self.new_environment(ctx, import_env);
        let old_env = self.current_environment();
        self.environment(Some(new_env));
        let ret = (f)(ctx, self, self._current_env());
        self.environment(old_env);
        self.restore_checkpoint(checkpoint);
        ret
    }

    /// If `None` sets to default env
    pub fn environment(&mut self, spec: Option<EnvironmentSpec>) {
        if let Some(EnvironmentSpec(n)) = spec {
            self.env_ptr = n.get();
        } else {
            self.env_ptr = 0;
        }
    }

    /// Get the specifier for the current environment
    pub fn current_environment(&self) -> Option<EnvironmentSpec> {
        NonZero::new(self.env_ptr).map(EnvironmentSpec)
    }

    /// Get default environment ptr
    pub fn default_environment_ptr(&self) -> StackEnvironmentPtr<'gc> {
        self.environments[0]
    }

    /// Get the environment ptr for a specifier
    pub fn environment_ptr(&self, spec: EnvironmentSpec) -> StackEnvironmentPtr<'gc> {
        self.environments[spec.0.get()]
    }

    /// Create a checkpoint for the current compiler state
    ///
    /// This allows a macro to restore the syntax items, variables, and argument symbols defined at this point in
    /// time.
    pub fn checkpoint(&mut self) -> Checkpoint {
        let checkpoint = Checkpoint(self.checkpoints.len());
        self.checkpoints.push((
            self.syntax_items.clone(),
            self.global_variables_defined.clone(),
            self.scopes.clone(),
        ));
        checkpoint
    }

    /// Restore the syntax items, variables, and argument symbols defined at the point the checkpoint was created.
    pub fn restore_checkpoint(&mut self, checkpoint: Checkpoint) {
        let Some((syntax_items, variables_defined, scopes)) = self.checkpoints.get(checkpoint.0)
        else {
            unreachable!()
        };

        self.syntax_items = syntax_items.clone();
        self.global_variables_defined = variables_defined.clone();
        self.scopes = scopes.clone();
    }

    pub fn is_argument(&self, symbol: lasso::Spur) -> Option<Arg> {
        if let Some(scope) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(idx, args)| {
                matches!(args.rest, Some(rest) if symbol == rest).then_some(idx)
            })
        {
            return Some(Arg::Rest { scope });
        }

        self.scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(scope, args)| {
                args.args
                    .iter()
                    .position(|s| *s == symbol)
                    .map(|index| Arg::Index { scope, index })
            })
    }

    // if this fails, the variable is either undefined by the script,
    // or is a variable or arg
    //
    // since the environment is not determined until the script is run
    pub fn get_macro(&self, symbol: lasso::Spur) -> Option<ArcSyntax> {
        if self.global_variables_defined.contains(&symbol)
            || self.scopes.iter().any(|args| {
                args.args.contains(&symbol)
                    || args.rest.is_some_and(|r| r == symbol)
                    || args.variables_defined.borrow().contains(&symbol)
            })
        {
            return None;
        }

        self.syntax_items.get(&symbol).cloned()
    }

    pub fn define_macro(&mut self, symbol: lasso::Spur, syntax: ArcSyntax) {
        self.global_variables_defined.remove(&symbol);
        if let Some(arg_scope) = self.scopes.last_mut() {
            arg_scope.args = arg_scope
                .args
                .iter()
                .copied()
                .filter(|s| *s != symbol)
                .collect();
            if arg_scope.rest.is_some_and(|r| r == symbol) {
                arg_scope.rest.take();
            }
            arg_scope.variables_defined.borrow_mut().remove(&symbol);
        }
        self.syntax_items.insert(symbol, syntax);
    }

    pub fn define_variable(&mut self, symbol: lasso::Spur) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables_defined.borrow_mut().insert(symbol);
        } else {
            self.global_variables_defined.insert(symbol);
        }
    }

    pub fn define_arguments(
        &mut self,
        args: impl IntoIterator<Item = lasso::Spur>,
        rest: Option<lasso::Spur>,
    ) {
        // Create and push a new argument scope
        self.scopes.push(Scope {
            args: args.into_iter().collect(),
            rest,
            ..Default::default()
        });
    }

    /// Get the [`ArgumentScope`] of a given scope where 0 is local, 1 is parent, etc..
    pub fn argument_scope(&self, scope: usize) -> Option<&Scope> {
        self.scopes.iter().rev().nth(scope)
    }

    /// Get the [`ArgumentScope`] of a given scope where 0 is local, 1 is parent, etc..
    pub fn argument_scope_mut(&mut self, scope: usize) -> Option<&mut Scope> {
        self.scopes.iter_mut().rev().nth(scope)
    }
}
