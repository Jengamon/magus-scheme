use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    num::NonZero,
    rc::Rc,
    str::FromStr as _,
    sync::Arc,
};

use fxhash::{FxHashMap, FxHashSet};
use gc_arena::{Collect, Gc, Mutation, RefLock, Static};
use num::{BigInt, ToPrimitive};
use program_parsers::StringProgramError;

use crate::{
    Fuel, Value, ValuePtr,
    bytecode::{Bytecode, Chunk, ChunkPtr, Constant, ImportFallbackMap, SourceData},
    environment::{Environment, StackEnvironment, StackEnvironmentPtr},
    interpreter::{Includer, ValuePointers, thread::ThreadPtr},
    runtime::lambda::{CompiledLambda, CompiledLambdaPtr, Lambda, NativeLambdaPtr},
    value::{Number, PromisePtr},
};

/// List of Scheme feature identifiers that we support
pub const FEATURES: &[&str] = &[
    "magus",
    "r7rs",
    "exact-closed",
    "ieee-float",
    "full-unicode",
    "ratios",
];
/// Calculate the feature identifier for \<name-version>
pub fn name_version_feature() -> Box<str> {
    format!("magus-{}", env!("CARGO_PKG_VERSION")).into_boxed_str()
}
/// Calculate target dependant feature identifiers (target, os, arch)
pub fn target_features() -> impl IntoIterator<Item = Box<str>> {
    let target = target_triple::TARGET;
    if let Ok(triple) = target_lexicon::Triple::from_str(target) {
        let arch = triple.architecture.into_str();
        if triple.operating_system == target_lexicon::OperatingSystem::Unknown {
            // only mark arch and target if os is unknown
            [&arch, target].into_iter().map(Box::from).collect()
        } else {
            let os = triple.operating_system.into_str();
            [&arch, &os, target].into_iter().map(Box::from).collect()
        }
    } else {
        vec![Box::from(target)]
    }
}

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

impl<'gc> ListHead<'gc> {
    pub fn into_program(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        source: Option<SourceData>,
    ) -> ProgramPtr<'gc> {
        let import = interner.get_or_intern_static("import");
        let define_library = interner.get_or_intern_static("define-library");
        match self {
            ListHead::Program(p) => p,
            ListHead::Import => Gc::new(
                mc,
                Program {
                    data: ProgramData::Symbol(import),
                    source,
                },
            ),
            ListHead::DefineLibrary => Gc::new(
                mc,
                Program {
                    data: ProgramData::Symbol(define_library),
                    source,
                },
            ),
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
    Number(Number),
    // Integer(i64),
    // TODO Support exact rationals (b/c string->number supports them, and
    // not accepting these directly is *odd*)
    // (sign, numer, denom)
    // Rational(bool, u64, u64),
    Inexact(f64),
    // TODO complex numbers
    String(#[collect(require_static)] lasso::Spur),
    Symbol(#[collect(require_static)] lasso::Spur),
    Bool(bool),
    Char(char),
    Bytevector(Rc<[u8]>),
    Labeled {
        label: usize,
        item: ProgramPtr<'gc>,
    },
    LabelRef(usize),
    Vector(Vec<ProgramPtr<'gc>>),
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

// TODO Provide nice ways of "mutation" that create new programs
// (or just provide a visitor API that instead can return a value)

impl<'gc> Program<'gc> {
    pub fn new(data: ProgramData<'gc>, source: Option<SourceData>) -> Self {
        Self { data, source }
    }
}

pub trait ParseProgram {
    type Error;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error>;
}

pub struct SyntaxContext<'a, 'b, 'gc> {
    pub mc: &'a Mutation<'gc>,
    pub ecc: &'b mut ExternalCompilerContext<'a>,
    pub library_def: LibraryDefinitionContext<'a, 'gc>,
    constants: &'b mut Vec<Constant>,
    lambdas: &'b mut Vec<Lambda<'gc>>,
    promises: &'b mut Vec<PromisePtr<'gc>>,
    upvalues: &'b mut usize,
}

impl<'gc> SyntaxContext<'_, '_, 'gc> {
    /// Add a constant to the current compile context
    pub fn add_constant(&mut self, c: Constant) -> usize {
        if let Some(p) = self.constants.iter().position(|constant| constant == &c) {
            p
        } else {
            let idx = self.constants.len();
            self.constants.push(c);
            idx
        }
    }

    /// Add a compiled lambda to the current compile context to refer to it from bytecode
    pub fn add_lambda(&mut self, ptr: CompiledLambdaPtr<'gc>) -> usize {
        if let Some(p) = self
            .lambdas
            .iter()
            .position(|lptr| matches!(lptr, Lambda::Compiled(lptr) if Gc::ptr_eq(*lptr, ptr)))
        {
            p
        } else {
            let idx = self.lambdas.len();
            self.lambdas.push(Lambda::Compiled(ptr));
            idx
        }
    }

    /// Add a native lambda to the current compile context to refer to it from bytecode
    pub fn add_native_lambda(&mut self, ptr: NativeLambdaPtr<'gc>) -> usize {
        if let Some(p) = self
            .lambdas
            .iter()
            .position(|lptr| matches!(lptr, Lambda::Native(lptr) if Gc::ptr_eq(*lptr, ptr)))
        {
            p
        } else {
            let idx = self.lambdas.len();
            self.lambdas.push(Lambda::Native(ptr));
            idx
        }
    }

    /// Add a promise to the current compile context to refer to it from bytecode
    pub fn add_promise(&mut self, prom: PromisePtr<'gc>) -> usize {
        if let Some(p) = self
            .promises
            .iter()
            .position(|pptr| Gc::ptr_eq(*pptr, prom))
        {
            p
        } else {
            let idx = self.promises.len();
            self.promises.push(prom);
            idx
        }
    }

    /// Internal method to create an upvalue reference in the current compile context
    pub(crate) fn add_upvalue(&mut self) -> usize {
        // Upvalues should be comparable to see if they are referencing the same out-of-scope value
        let idx = *self.upvalues;
        *self.upvalues += 1;
        idx
    }

    /// Get all constants in the current compile context
    pub fn constants(&self) -> impl IntoIterator<Item = Constant> {
        self.constants.clone()
    }

    /// Get all lambdas in the current compile context
    pub fn lambdas(&self) -> impl IntoIterator<Item = Lambda<'gc>> {
        self.lambdas.clone()
    }

    /// Get all promises in the current compile context
    // #[deprecated = "check if using native lambdas can solve w/o adding bytecode support"]
    pub fn promises(&self) -> impl IntoIterator<Item = PromisePtr<'gc>> {
        self.promises.clone()
    }

    /// Get the number of upvalues in the current compile context
    pub fn upvalues(&self) -> usize {
        *self.upvalues
    }
}

impl<'gc> std::ops::Deref for SyntaxContext<'_, '_, 'gc> {
    type Target = Mutation<'gc>;
    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

pub trait Transformer<'gc>: std::fmt::Debug {
    /// Evaluate a macro, until it finishes
    ///
    /// # Parameters
    /// - `mc`: [`gc_arena`] mutation context
    /// - `args`: external representation corresponding to passed in syntax
    fn evaluate(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>>;

    /// Registers this syntax item as a definition
    fn is_definition(&self, _ptr: ProgramPtr<'gc>, _compiler: &Compiler<'gc>) -> bool {
        false
    }

    /// Registers this syntax item as a container if all the returned program pointers are definitions
    ///
    /// Affects how definitions are registered. A container syntax is considered a
    /// definition if all of it's components are definitions (or containers of only definitions)
    fn is_container(
        &self,
        _ptr: ProgramPtr<'gc>,
        _compiler: &Compiler<'gc>,
    ) -> Vec<ProgramPtr<'gc>> {
        Vec::new()
    }
}
pub type TransformerPtr<'gc> = Gc<'gc, dyn Transformer<'gc>>;

/// The result of a syntax evaluation
pub enum SyntaxReturn<'gc> {
    /// This is code to add to the compiling chunk
    Code(Box<[Bytecode]>),
    /// This is a transformer that can be used by a syntax item
    /// to transform code
    Transformer(TransformerPtr<'gc>),
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
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>>;

    /// Registers this syntax item as a definition
    fn is_definition<'gc>(&self, _ptr: ProgramPtr<'gc>, _compiler: &Compiler<'gc>) -> bool {
        false
    }

    /// Registers this syntax item as a container
    ///
    /// Affects how definitions are registered. A container syntax is considered a
    /// definition if all of it's components are definitions (or containers of only definitions)
    ///
    /// If this returns an empty list, the syntax is *not* considered a container (and thus not a definition)
    fn is_container<'gc>(
        &self,
        _ptr: ProgramPtr<'gc>,
        _compiler: &Compiler<'gc>,
    ) -> Vec<ProgramPtr<'gc>> {
        Vec::new()
    }

    /// Marks this syntax as deriving from a local Transformer
    fn is_transformer(&self) -> Option<TransformerKey> {
        None
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

    pub fn to_string(&self, interner: &lasso::Rodeo) -> Box<str> {
        Box::from(
            self.0
                .iter()
                .map(|ni| match ni {
                    LibraryNameItem::Identifier(i) => interner.resolve(i).to_string(),
                    LibraryNameItem::Integer(i) => i.to_string(),
                })
                .collect::<Vec<_>>()
                .join(" ")
                .as_str(),
        )
    }

    pub fn convert(ptr: ProgramPtr<'_>, interner: &mut lasso::Rodeo) -> Option<Self> {
        let import = interner.get_or_intern_static("import");
        let define_library = interner.get_or_intern_static("define-library");
        match &ptr.data {
            ProgramData::List { head, body }
                if matches!(head, ListHead::Import) && body.iter().all(|p| {
                    matches!(&p.data, ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO)
                        || matches!(p.data, ProgramData::Symbol(_))
                }) =>
            {
                let body_items = body.iter().map(|p| match &p.data {
                    ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(*s)),
                    ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO => {
                        i.to_u64().map(LibraryNameItem::Integer)
                    }
                    _ => unreachable!(),
                });

                Some(LibraryName(
                    std::iter::once(Some(LibraryNameItem::Identifier(import)))
                        .chain(body_items)
                        .collect::<Option<_>>()?,
                ))
            }
            ProgramData::List { head, body }
                if matches!(head, ListHead::DefineLibrary) && body.iter().all(|p| {
                    matches!(&p.data, ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO)
                        || matches!(p.data, ProgramData::Symbol(_))
                }) =>
            {
                let body_items = body.iter().map(|p| match &p.data {
                    ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(*s)),
                    ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO => {
                        i.to_u64().map(LibraryNameItem::Integer)
                    }
                    _ => unreachable!(),
                });

                Some(LibraryName(
                    std::iter::once(Some(LibraryNameItem::Identifier(define_library)))
                        .chain(body_items)
                        .collect::<Option<_>>()?,
                ))
            }
            ProgramData::List { head, body }
                if matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(_)) || matches!(&p.data, ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO))
                    && body.iter().all(|p| {
                        matches!(&p.data, ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO)
                            || matches!(p.data, ProgramData::Symbol(_))
                    }) =>
            {
                let body_items = body.iter().map(|p| match &p.data {
                    ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(*s)),
                    ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO => i.to_u64().map(LibraryNameItem::Integer),
                    _ => unreachable!(),
                });

                let ListHead::Program(list_head) = head else {
                    unreachable!()
                };
                let first_item = match &list_head.data {
                    ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(*s)),
                    ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO => i.to_u64().map(LibraryNameItem::Integer),
                    _ => unreachable!(),
                };

                Some(LibraryName(
                    std::iter::once(first_item).chain(body_items).collect::<Option<_>>()?,
                ))
            }
            _ => None,
        }
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
                $crate::library_name!(@parsing $intern => $s)
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
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(
        &mut self,
        name: LibraryName,
        module: impl Module + Send + Sync + 'static,
    ) -> Result<(), LibraryNameExists> {
        self.insert_arc(name, Arc::new(module))
    }

    pub fn insert_arc(
        &mut self,
        name: LibraryName,
        module: Arc<dyn Module + Send + Sync + 'static>,
    ) -> Result<(), LibraryNameExists> {
        if self.modules.contains_key(&name) {
            return Err(LibraryNameExists);
        }

        self.modules.insert(name, module);
        Ok(())
    }

    pub fn has_library(&self, name: &LibraryName) -> bool {
        self.modules.contains_key(name)
    }

    pub fn library(&self, name: &LibraryName) -> Option<&(dyn Module + Send + Sync)> {
        self.modules.get(name).map(|m| m.as_ref())
    }

    pub fn library_arc(&self, name: &LibraryName) -> Option<&Arc<dyn Module + Send + Sync>> {
        self.modules.get(name)
    }

    pub fn forget(
        &mut self,
        name: &LibraryName,
    ) -> Option<Arc<dyn Module + Send + Sync + 'static>> {
        self.modules.remove(name)
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
enum ExportItem<'gc> {
    Value(ValuePtr<'gc>),
    Macro(#[collect(require_static)] ArcSyntax),
    Transformer(TransformerPtr<'gc>),
}

/// The result of a `define-library`
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
struct SchemeLibrary<'gc> {
    exported_items: HashMap<Static<lasso::Spur>, ExportItem<'gc>>,
}

// #[allow(unsafe_code)]
// unsafe impl<'gc> Collect<'gc> for SchemeLibrary<'gc> {
//     fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
//         for value in self.exported_items.values() {
//             value.trace(cc);
//         }
//     }
// }

/// A container for Scheme-defined modules (using `define-library` at the top level)
#[derive(Collect, Debug, Default, Clone)]
#[collect(no_drop)]
pub struct LocalWorld<'gc> {
    modules: HashMap<LibraryName, SchemeLibrary<'gc>>,
}

type SyntaxDef = HashMap<lasso::Spur, ArcSyntax>;
type VariableDef = fxhash::FxHashSet<lasso::Spur>;
#[derive(Debug, Clone, Default)]
pub struct Scope {
    args: Rc<[lasso::Spur]>,
    rest: Option<lasso::Spur>,
    requested_names: FxHashSet<lasso::Spur>,

    upvalues: Rc<RefCell<fxhash::FxHashMap<Option<usize>, usize>>>,
    variables_defined: Rc<RefCell<fxhash::FxHashMap<lasso::Spur, Option<usize>>>>,
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

    pub fn request_name(&mut self, name: lasso::Spur) {
        self.requested_names.insert(name);
    }
}

/// A local representation of a [`Transformer`] used to store in an [`ArcSyntax`]
#[derive(Debug)]
struct PrivateTransformer {
    key: TransformerKey,
    _knob: Arc<()>,
}

// We just delegate to the Transformer object stored in the compiler. As this struct is private, the *only*
// way it gets created is from some Syntax or the compiler installing it.
impl Syntax for PrivateTransformer {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        compiler
            .stash
            .transformers
            .get(self.key)
            .copied()
            .ok_or(anyhow::anyhow!(
                "transformer {:?} not found in compiler",
                self.key
            ))
            .and_then(|trans| trans.evaluate(ctx, compiler, import_env, args))
    }

    fn is_definition<'gc>(&self, ptr: ProgramPtr<'gc>, compiler: &Compiler<'gc>) -> bool {
        compiler
            .stash
            .transformers
            .get(self.key)
            .map(|trans| trans.is_definition(ptr, compiler))
            .unwrap_or_default()
    }

    fn is_container<'gc>(
        &self,
        ptr: ProgramPtr<'gc>,
        compiler: &Compiler<'gc>,
    ) -> Vec<ProgramPtr<'gc>> {
        compiler
            .stash
            .transformers
            .get(self.key)
            .map(|trans| trans.is_container(ptr, compiler))
            .unwrap_or_default()
    }

    fn is_transformer(&self) -> Option<TransformerKey> {
        Some(self.key)
    }
}

slotmap::new_key_type! {
pub struct TransformerKey;
// pub struct ProgramKey;
}
/// Compiler Stash
#[derive(Debug, Default)]
pub struct Stash<'gc> {
    transformer_knobs: slotmap::SecondaryMap<TransformerKey, Arc<()>>,
    // TODO Include a knob system to automatically drop unused transfomers and programs
    transformers: slotmap::SlotMap<TransformerKey, TransformerPtr<'gc>>,
    // programs: slotmap::SlotMap<ProgramKey, ProgramPtr<'gc>>,
}

impl Stash<'_> {
    fn cleanup(&mut self) {
        // Drop transformers where their knobs only have 1 referent: us.
        let transformers_to_drop = self
            .transformer_knobs
            .iter()
            .filter_map(|(key, knob)| (Arc::strong_count(knob) == 1).then_some(key))
            .collect::<fxhash::FxHashSet<_>>();
        for key in transformers_to_drop {
            self.transformers.remove(key);
            self.transformer_knobs.remove(key);
        }
    }
}

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
        // trace_slotmap!(programs);
    }
}

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
enum NativeItem<'gc> {
    Syntax(#[collect(require_static)] ArcSyntax),
    Value(ValuePtr<'gc>),
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

    /// defined labels
    labeled_labels: FxHashSet<usize>,
    /// requested labels
    label_ref_labels: FxHashSet<usize>,
    /// label data
    label_values: FxHashMap<usize, ProgramPtr<'gc>>,

    // cache for native modules
    native_cache: FxHashMap<LibraryName, FxHashMap<Static<lasso::Spur>, NativeItem<'gc>>>,

    // stash that can be used by macros to store things
    stash: Stash<'gc>,

    /// Recursion counter
    rec_counter: Gc<'gc, RefLock<usize>>,
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
    #[error("undefined labels: {labels:?}")]
    UndefinedLabels {
        location: Option<SourceData>,
        labels: FxHashSet<usize>,
    },
    #[error("duplicate label: {label}")]
    DuplicateLabel {
        location: Option<SourceData>,
        label: usize,
    },
    #[error("an empty list was encountered in code")]
    EmptyList(Option<SourceData>),
    #[error("a dotted list was encountered in code")]
    DottedList(Option<SourceData>),
    #[error("a macro encountered an error: {0}")]
    Macro(Arc<anyhow::Error>, Option<SourceData>),
    #[error("no library name in library declaration")]
    NoLibraryName(Option<SourceData>),
    #[error(transparent)]
    ImportSet(#[from] ImportSetError),
    #[error(transparent)]
    Import(#[from] ImportError),
    #[error(transparent)]
    LibraryDeclaration(#[from] LibraryDeclarationError),
    #[error(transparent)]
    Library(#[from] DefineLibraryError),
    #[error("import resolves to overlapping names")]
    DoubleImport(Box<[lasso::Spur]>),
    #[error("too many recursions at macro-expansion time")]
    TooRecursive,
    #[error("ratio over 0 in source")]
    RatioOverZero(Option<SourceData>),
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
    Prefix {
        set: Arc<ImportSet>,
        prefix: Arc<str>,
    },
    Rename {
        set: Arc<ImportSet>,
        rename_pairs: Arc<[(lasso::Spur, lasso::Spur)]>,
    },
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
        match &ptr.data {
            ProgramData::Number(Number::Integer(i)) if i >= &BigInt::ZERO => {
                i.to_u64().map(LibraryNameItem::Integer)
            }
            ProgramData::Symbol(s) => Some(LibraryNameItem::Identifier(*s)),
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
                        && !body.is_empty() =>
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
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == prefix)
                        && body.len() == 2
                        && matches!(body[1].data, ProgramData::Symbol(_)) =>
                {
                    // read first body param as an import set, and the second as a prefix to prepend
                    // can only specify those 2
                    let source = ImportSet::convert(body[0], interner)?;
                    let ProgramData::Symbol(prefix_sym) = body[1].data else {
                        unreachable!()
                    };
                    let prefix = Arc::<str>::from(interner.resolve(&prefix_sym));

                    Ok(ImportSet::Prefix {
                        set: Arc::new(source),
                        prefix,
                    })
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == prefix) => {
                    Err(ImportSetError::NotImportSet(ptr.source))
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == rename)
                        && !body.iter().skip(1).any(|p| {
                            !matches!(&p.data, ProgramData::List {
                                body, ..
                            } if body.len() == 1 )
                        }) =>
                {
                    // so we *don't* actually handle the happy path conditions in the if above, because it's too annoying, so we'll
                    // just error out here
                    //
                    // read first body param as an import set, and the rest *must* be pairs of sym1 and sym2
                    let source = ImportSet::convert(body[0], interner)?;
                    let rename_pairs = body
                        .iter()
                        .skip(1)
                        .map(|p| {
                            let ProgramData::List { head, body } = &p.data else {
                                // we buy this at least

                                unreachable!()
                            };
                            // we know body[0] is the only element of body, but we don't know what it is (as well as the head)
                            // so generate those spurs
                            let head_spur = match head {
                                ListHead::Import => interner.get_or_intern_static("import"),
                                ListHead::DefineLibrary => {
                                    interner.get_or_intern_static("define-library")
                                }
                                ListHead::Program(p) => match p.data {
                                    ProgramData::Symbol(s) => s,
                                    _ => return Err(ImportSetError::NotImportSet(ptr.source)),
                                },
                            };
                            let body_spur = match body[0].data {
                                ProgramData::Symbol(s) => s,
                                _ => return Err(ImportSetError::NotImportSet(ptr.source)),
                            };

                            Ok((head_spur, body_spur))
                        })
                        .collect::<Result<Arc<_>, _>>()?;

                    Ok(ImportSet::Rename {
                        set: Arc::new(source),
                        rename_pairs,
                    })
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == rename) =>
                {
                    // read first body param as an import set, and the rest *must* be pairs of sym1 and sym2
                    Err(ImportSetError::NotImportSet(ptr.source))
                }
                ListHead::Program(p)
                    if matches!(
                        &p.data,
                        ProgramData::Symbol(_) | ProgramData::Number(Number::Integer(_))
                    ) && body.iter().all(|p| {
                        matches!(
                            &p.data,
                            ProgramData::Symbol(_) | ProgramData::Number(Number::Integer(_))
                        )
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

#[derive(thiserror::Error, Debug)]
pub enum ImportError {
    #[error("invalid library import")]
    InvalidImport,
    #[error("library not found: ({0})")]
    LibraryNotFound(Box<str>),
    // TODO store module name in error
    #[error("name not found in module: {name}")]
    NameNotFound { name: Box<str> },
    #[error("names not found in module: {}", names.clone().join(", "))]
    NamesNotFound { names: Vec<Box<str>> },
    #[error("failed to define symbol")]
    FailedToDefine(lasso::Spur),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExportSet {
    Name(lasso::Spur),
    Rename { from: lasso::Spur, to: lasso::Spur },
}
impl ExportSet {
    pub fn convert(ptr: ProgramPtr<'_>, interner: &mut lasso::Rodeo) -> Option<ExportSet> {
        let rename = interner.get_or_intern_static("rename");
        match &ptr.data {
            ProgramData::Symbol(s) => Some(ExportSet::Name(*s)),
            ProgramData::List { head, body }
                if body.len() == 2
                    && body
                        .iter()
                        .all(|p| matches!(p.data, ProgramData::Symbol(_)))
                    && matches!(head, ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == rename)) =>
            {
                // rename
                let ProgramData::Symbol(from) = body[0].data else {
                    unreachable!()
                };
                let ProgramData::Symbol(to) = body[1].data else {
                    unreachable!()
                };
                Some(ExportSet::Rename { from, to })
            }
            _ => None,
        }
    }
}

/// A `cond-expand` feature requirement
#[derive(Debug, Clone)]
pub enum FeatureRequirement {
    // Some feature identifier (has to be a valid one, either from the default features list, or the input `additional_features`)
    Feature(lasso::Spur),
    Library(LibraryName),
    And(Rc<[Box<Self>]>),
    Or(Rc<[Box<Self>]>),
    Not(Box<Self>),
}

#[derive(thiserror::Error, Debug)]
#[error("not a feature requirement")]
pub struct NotFeatureRequirement(Option<SourceData>);

impl FeatureRequirement {
    pub fn is_satisfied(
        &self,
        compiler: &Compiler<'_>,
        world: &World,
        features: &[lasso::Spur],
    ) -> bool {
        match self {
            Self::Feature(f) => features.contains(f),
            Self::Library(ln) => compiler.has_library(ln, world),
            Self::And(and) => and
                .iter()
                .all(|f| f.is_satisfied(compiler, world, features)),
            Self::Or(or) => or.iter().any(|f| f.is_satisfied(compiler, world, features)),
            Self::Not(feat) => !feat.is_satisfied(compiler, world, features),
        }
    }

    pub fn convert(
        ptr: ProgramPtr<'_>,
        interner: &mut lasso::Rodeo,
    ) -> Result<Self, NotFeatureRequirement> {
        let library = interner.get_or_intern_static("library");
        let and = interner.get_or_intern_static("and");
        let or = interner.get_or_intern_static("or");
        let not = interner.get_or_intern_static("not");

        match &ptr.data {
            ProgramData::Symbol(s) => Ok(Self::Feature(*s)),
            ProgramData::List {
                head: ListHead::Program(head),
                body,
            } => match &head.data {
                ProgramData::Symbol(s) if *s == library && body.len() == 1 => {
                    if let Some(library_name) = LibraryName::convert(body[0], interner) {
                        Ok(Self::Library(library_name))
                    } else {
                        Err(NotFeatureRequirement(ptr.source))
                    }
                }
                ProgramData::Symbol(s) if *s == not && body.len() == 1 => {
                    let sub = Self::convert(body[0], interner)?;
                    Ok(Self::Not(Box::new(sub)))
                }
                ProgramData::Symbol(s) if *s == and => {
                    let sub = body
                        .iter()
                        .map(|p| Self::convert(*p, interner).map(Box::new))
                        .collect::<Result<_, _>>()?;
                    Ok(Self::And(sub))
                }
                ProgramData::Symbol(s) if *s == or => {
                    let sub = body
                        .iter()
                        .map(|p| Self::convert(*p, interner).map(Box::new))
                        .collect::<Result<_, _>>()?;
                    Ok(Self::Or(sub))
                }
                _ => Err(NotFeatureRequirement(ptr.source)),
            },
            _ => Err(NotFeatureRequirement(ptr.source)),
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
    Export(Rc<[ExportSet]>),
    CondExpand {
        branches: Rc<[(FeatureRequirement, Box<[Self]>)]>,
        else_branch: Option<Box<[Self]>>,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum LibraryDeclarationError {
    #[error("not a library declaration")]
    NotALibraryDeclaration(Option<SourceData>),
    #[error("not an export set")]
    NotAnExportSet(Option<SourceData>),
    #[error(transparent)]
    ImportSetError(#[from] ImportSetError),
    #[error(transparent)]
    FeatureRequirement(#[from] NotFeatureRequirement),
}

impl<'gc> LibraryDeclaration<'gc> {
    pub fn convert(
        ptr: ProgramPtr<'gc>,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
    ) -> Result<Self, LibraryDeclarationError> {
        let export = interner.get_or_intern_static("export");
        let include_library_declarations =
            interner.get_or_intern_static("include-library-declarations");
        let include = interner.get_or_intern_static("include");
        let include_ci = interner.get_or_intern_static("include-ci");
        let begin = interner.get_or_intern_static("begin");
        let cond_expand = interner.get_or_intern_static("cond-expand");
        let else_sym = interner.get_or_intern_static("else");

        match &ptr.data {
            ProgramData::List { head, body } => match head {
                ListHead::Import => {
                    let imports = body
                        .iter()
                        .map(|p| ImportSet::convert(*p, interner))
                        .collect::<Result<_, _>>()?;
                    Ok(LibraryDeclaration::Import(imports))
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == export) =>
                {
                    // export
                    // read elements as ExportSets
                    let exports = body
                        .iter()
                        .map(|p| {
                            ExportSet::convert(*p, interner)
                                .ok_or(LibraryDeclarationError::NotAnExportSet(p.source))
                        })
                        .collect::<Result<_, _>>()?;
                    Ok(LibraryDeclaration::Export(exports))
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == begin) => {
                    // begin
                    Ok(LibraryDeclaration::Begin(Rc::from(body.as_slice())))
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == include_library_declarations)
                        && body
                            .iter()
                            .all(|b| matches!(b.data, ProgramData::String(_))) =>
                {
                    let filenames = body
                        .iter()
                        .filter_map(|b| match &b.data {
                            ProgramData::String(s) => Some(Rc::from(interner.resolve(s))),
                            _ => unreachable!(),
                        })
                        .collect::<Rc<[_]>>();

                    Ok(LibraryDeclaration::IncludeLibraryDeclarations(filenames))
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == include)
                        && body
                            .iter()
                            .all(|b| matches!(b.data, ProgramData::String(_))) =>
                {
                    let filenames = body
                        .iter()
                        .filter_map(|b| match &b.data {
                            ProgramData::String(s) => Some(Rc::from(interner.resolve(s))),
                            _ => unreachable!(),
                        })
                        .collect::<Rc<[_]>>();

                    Ok(LibraryDeclaration::Include {
                        filenames,
                        case_insensitive: false,
                    })
                }
                ListHead::Program(p)
                    if matches!(&p.data, ProgramData::Symbol(s) if *s == include_ci)
                        && body
                            .iter()
                            .all(|b| matches!(b.data, ProgramData::String(_))) =>
                {
                    let filenames = body
                        .iter()
                        .filter_map(|b| match &b.data {
                            ProgramData::String(s) => Some(Rc::from(interner.resolve(s))),
                            _ => unreachable!(),
                        })
                        .collect::<Rc<[_]>>();

                    Ok(LibraryDeclaration::Include {
                        filenames,
                        case_insensitive: true,
                    })
                }
                ListHead::Program(p) if matches!(&p.data, ProgramData::Symbol(s) if *s == cond_expand) =>
                {
                    // A cond-expand consists of at least 1 clause of form (FeatureRequirement <lib decls>...)
                    // followed by up to one (else <lib decls>...)
                    let mut branches = vec![];
                    let mut else_branch = vec![];
                    let mut last_non_else_index = None;
                    for (idx, p) in body.iter().enumerate() {
                        if let ProgramData::List { head, body } = &p.data {
                            if let Some(s) = head.into_symbol(interner) {
                                if s == else_sym {
                                    // This is the else decl, add to else stuff, then break (so that this *has* to be the last one)
                                    let decls = body
                                        .iter()
                                        .map(|b| Self::convert(*b, mc, interner))
                                        .collect::<Result<Vec<_>, _>>()?;
                                    else_branch.extend(decls);
                                    break;
                                }
                            }
                            // The head this the requirement, the body the declarationss
                            last_non_else_index = Some(idx);
                            let head = FeatureRequirement::convert(
                                head.into_program(mc, interner, ptr.source),
                                interner,
                            )?;
                            let decls = body
                                .iter()
                                .map(|b| Self::convert(*b, mc, interner))
                                .collect::<Result<_, _>>()?;
                            branches.push((head, decls));
                        } else {
                            // not a valid cond-expand decl
                            return Err(LibraryDeclarationError::NotALibraryDeclaration(
                                ptr.source,
                            ));
                        }
                    }

                    if last_non_else_index.is_none_or(|lb| {
                        ![body.len(), body.len().saturating_sub(1)].contains(&(lb + 1))
                    }) {
                        // else branch is not the last branch
                        return Err(LibraryDeclarationError::NotALibraryDeclaration(ptr.source));
                    }

                    Ok(Self::CondExpand {
                        branches: branches.into(),
                        else_branch: if !else_branch.is_empty() {
                            Some(Box::from_iter(else_branch))
                        } else {
                            None
                        },
                    })
                }
                _ => Err(LibraryDeclarationError::NotALibraryDeclaration(ptr.source)),
            },
            _ => Err(LibraryDeclarationError::NotALibraryDeclaration(ptr.source)),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum DefineLibraryError {
    #[error("empty library name")]
    EmptyLibraryName,
    #[error("attempted to define reserved library name")]
    ReservedName(LibraryName),
    #[error("attempted to define an existing library")]
    AlreadyExists(LibraryName),
    #[error("ran out of fuel while interpreting library ({0})")]
    OutOfFuel(Box<str>),
    #[error("no name in library: {0}")]
    UndefinedExport(Box<str>),
    #[error("library interpretation error: {0}")]
    InterpError(Box<str>),
    #[error("library compilation error: {0}")]
    Compile(#[from] Box<CompileError>),
    #[error("library import error: {0}")]
    Import(#[from] ImportError),
    #[error("library include error: {0}")]
    Include(anyhow::Error),
    #[error("library include parse error: {0}")]
    IncludeParse(#[from] StringProgramError),
    #[error("library declaration error: {0}")]
    LibraryDeclaration(#[from] LibraryDeclarationError),
    #[error("import resolves to overlapping names")]
    DoubleImport(Box<[lasso::Spur]>),
}

/// Context struct for things external to the compiler
pub struct ExternalCompilerContext<'a> {
    pub world: &'a World,
    pub interner: &'a mut lasso::Rodeo,
    pub includer: &'a dyn Includer,
}

/// Context struct for library definition parameters
#[derive(Clone, Copy)]
pub struct LibraryDefinitionContext<'a, 'gc> {
    /// Maximum amount of fuel used *per* library definition. `None` means to run to completion (unlimited).
    pub max_fuel: Option<i32>,
    /// Pointers to null (semantic), true and false (convenience)
    pub value_pointers: ValuePointers<'gc>,
    /// Any additional features to be supported by `(cond-expand)`
    pub additional_features: Option<&'a [Arc<str>]>,

    // TODO ... what if we require a Thread<'gc> here????
    // and is has to be the thread that the code for this compiler will be
    // run on (so that upvalues are preserved between compiler imports
    // and main code????)
    pub thread: ThreadPtr<'gc>,
}

impl<'gc> Compiler<'gc> {
    /// Maximum number of recursive calls before macro expansion fails
    const MAX_RECURSION: usize = 200;

    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            local_world: LocalWorld::default(),
            syntax_items: Default::default(),
            global_variables_defined: Default::default(),
            scopes: Default::default(),
            checkpoints: Default::default(),
            // The very first environment pointer is always the default environment
            environments: vec![Gc::new(mc, RefLock::new(Environment::new(mc, None)))],
            labeled_labels: FxHashSet::default(),
            label_ref_labels: FxHashSet::default(),
            label_values: FxHashMap::default(),
            env_ptr: 0,
            stash: Stash::default(),
            native_cache: FxHashMap::default(),
            rec_counter: Gc::new(mc, RefLock::new(0)),
        }
    }

    /// Access label data (`labeled`, `requested`)
    pub fn label_data(&self) -> (&FxHashSet<usize>, &FxHashSet<usize>) {
        (&self.labeled_labels, &self.label_ref_labels)
    }

    /// Add labels to `labeled` set
    pub fn add_labeled(&mut self, labeled: impl IntoIterator<Item = usize>) {
        self.labeled_labels.extend(labeled);
    }

    /// Add labels to `requested` set
    pub fn add_label_refs(&mut self, label_refs: impl IntoIterator<Item = usize>) {
        self.label_ref_labels.extend(label_refs);
    }

    /// Add label data
    pub fn label_value(&mut self, label: usize) -> Option<ProgramPtr<'gc>> {
        self.label_values.get(&label).copied()
    }

    /// Convenience function for cleaning up unused transformers
    pub fn cleanup(&mut self) {
        self.stash.cleanup();
    }

    /// Convenience function for generating the list of base features
    pub fn base_features(interner: &mut lasso::Rodeo) -> Vec<lasso::Spur> {
        let mut features: Vec<_> = FEATURES
            .iter()
            .map(|f| interner.get_or_intern_static(f))
            .collect();
        features.push(interner.get_or_intern(name_version_feature()));
        features.extend(
            target_features()
                .into_iter()
                .map(|i| interner.get_or_intern(i)),
        );
        features
    }

    fn add_label_values_from_program(&mut self, ptr: ProgramPtr<'gc>) -> Result<(), CompileError> {
        match &ptr.data {
            ProgramData::Labeled { label, item } => {
                if self.label_values.contains_key(label) {
                    return Err(CompileError::DuplicateLabel {
                        location: ptr.source,
                        label: *label,
                    });
                }
                self.label_values.insert(*label, *item);
                self.add_label_values_from_program(*item)?;
            }
            ProgramData::List { head, body } => {
                if let ListHead::Program(p) = head {
                    self.add_label_values_from_program(*p)?;
                }
                for program in body {
                    self.add_label_values_from_program(*program)?;
                }
            }
            ProgramData::DottedList { pre_dot, dot } => {
                for program in pre_dot {
                    self.add_label_values_from_program(*program)?;
                }
                self.add_label_values_from_program(*dot)?;
            }
            ProgramData::Vector(vec) => {
                for program in vec {
                    self.add_label_values_from_program(*program)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Compile an list of programs into a [`Chunk`] (not allowing any imports)
    ///
    /// # Parameters
    /// - `mc`
    /// - `ecc`: [`ExternalCompilerContext`]
    /// - `value_pointers`: the pointers to the values used to represent `'()`, `#t`, and `#f`
    /// - `max_fuel`: amount of fuel given to *each* `define-library` statement for any necessary code. `None` means to run to completion
    /// - `programs`: pointers to the members of a program
    pub fn compile_no_import<'a>(
        &mut self,
        mc: &'a Mutation<'gc>,
        ecc: &mut ExternalCompilerContext<'a>,
        library_def: LibraryDefinitionContext<'a, 'gc>,
        programs: impl IntoIterator<Item = ProgramPtr<'gc>>,
    ) -> Result<ChunkPtr<'gc>, CompileError> {
        // compile code loop
        let mut constants = Vec::new();
        let mut lambdas = Vec::new();
        let mut promises = Vec::new();
        let mut upvalues = 0;
        let mut context = SyntaxContext::<'a, '_, 'gc> {
            mc,
            ecc,
            library_def,
            constants: &mut constants,
            lambdas: &mut lambdas,
            promises: &mut promises,
            upvalues: &mut upvalues,
        };
        let mut code = vec![];
        let mut labels = FxHashMap::default();
        for program in programs {
            self.label_ref_labels.clear();
            self.labeled_labels.clear();
            self.label_values.clear();
            self.add_label_values_from_program(program)?;

            if let Some(source) = program.source {
                labels.insert(code.len(), source);
            }
            code.extend(self.compile_code(&mut context, program)?.into_bytecode());
            // Clear datum labels (between top-level datum!)
            let undefined_labels = self
                .label_ref_labels
                .difference(&self.labeled_labels)
                .copied()
                .collect::<FxHashSet<_>>();
            if !undefined_labels.is_empty() {
                // Error where there are undefined labels
                return Err(CompileError::UndefinedLabels {
                    location: program.source,
                    labels: undefined_labels,
                });
            }
        }

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

    /// Compile an list of programs into a [`Chunk`]
    ///
    /// # Parameters
    /// - `mc`
    /// - `ecc`: [`ExternalCompilerContext`]
    /// - `value_pointers`: the pointers to the values used to represent `'()`, `#t`, and `#f`
    /// - `max_fuel`: amount of fuel given to *each* `define-library` statement for any necessary code. `None` means to run to completion
    /// - `programs`: pointers to the members of a program
    pub fn compile<'a>(
        &mut self,
        mc: &'a Mutation<'gc>,
        ecc: &mut ExternalCompilerContext<'a>,
        library_def: &LibraryDefinitionContext<'a, 'gc>,
        programs: impl IntoIterator<Item = ProgramPtr<'gc>>,
    ) -> Result<ChunkPtr<'gc>, CompileError> {
        self.stash.cleanup();

        let mut programs = programs.into_iter().peekable();
        let mut imported = FxHashSet::default();

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
                        .map(|p| ImportSet::convert(*p, ecc.interner))
                        .collect();

                    for set in sets? {
                        let imports = self.import(mc, ecc.interner, ecc.world, &set, true)?;
                        if !imported.is_disjoint(&imports) {
                            return Err(CompileError::DoubleImport(Box::from_iter(
                                imported.intersection(&imports).copied(),
                            )));
                        }
                        imported.extend(imports);
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
                    let program = programs.next().unwrap();
                    let ProgramData::List { body, .. } = &program.data else {
                        unreachable!();
                    };

                    let name = body
                        .iter()
                        .next()
                        .and_then(|n| LibraryName::convert(*n, ecc.interner))
                        .ok_or(CompileError::NoLibraryName(program.source))?;

                    let library_decls = body
                        .iter()
                        .skip(1)
                        .map(|p| LibraryDeclaration::convert(*p, mc, ecc.interner))
                        .collect::<Result<Vec<_>, _>>()?;

                    // manually match and ignore OoF errors
                    match self.define_library(mc, &name, ecc, true, *library_def, library_decls) {
                        Ok(_) => {}
                        Err(e) => return Err(e.into()),
                    }
                }
                _ => break,
            }
        }

        self.compile_no_import(mc, ecc, *library_def, programs)
    }

    /// Compile code in the current context
    ///
    /// Is meant for the implementations of macros ([`Syntax`])
    pub fn compile_code(
        &mut self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        program: ProgramPtr<'gc>,
    ) -> Result<SyntaxReturn<'gc>, CompileError> {
        macro_rules! simple_constant {
            ($data:expr => $name:ident) => {{
                let index = ctx.add_constant(Constant::$name($data));
                Ok(SyntaxReturn::Code(Box::from([Bytecode::PushConst {
                    index,
                }])))
            }};
        }
        match &program.data {
            ProgramData::Number(n) => {
                simple_constant!(n.clone() => Number)
            }
            // ProgramData::Integer(i) => {
            //     // simple_constant!(*i => Number)
            //     todo!()
            // }
            // ProgramData::Rational(sign, numer, denom) if *denom != 0 => {
            //     // let index = ctx.add_constant(Constant::Rational(*sign, *numer, *denom));
            //     // Ok(SyntaxReturn::Code(Box::from([Bytecode::PushConst {
            //     //     index,
            //     // }])))
            //     todo!()
            // }
            // ProgramData::Rational(_, _, _) => Err(CompileError::RatioOverZero(program.source)),
            ProgramData::Inexact(f) => {
                simple_constant!(*f => Inexact)
            }
            ProgramData::Bytevector(bv) => {
                simple_constant!(Arc::from(bv.as_ref()) => Bytevector)
            }
            ProgramData::String(spur) => {
                let index =
                    ctx.add_constant(Constant::String(Arc::from(ctx.ecc.interner.resolve(spur))));
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

                match self.is_argument(*spur) {
                    Some(Arg::Index { index, scope: 0 })
                        if !self
                            .argument_scope(0)
                            .unwrap()
                            .variables_defined
                            .borrow()
                            .contains_key(spur) =>
                    {
                        Ok(SyntaxReturn::Code(Box::from([Bytecode::FetchArg {
                            index,
                        }])))
                    }
                    Some(Arg::Rest { scope: 0 })
                        if !self
                            .argument_scope(0)
                            .unwrap()
                            .variables_defined
                            .borrow()
                            .contains_key(spur) =>
                    {
                        Ok(SyntaxReturn::Code(Box::from([Bytecode::FetchRest])))
                    }
                    _ => {
                        Ok(SyntaxReturn::Code(Box::from([Bytecode::Reference {
                            symbol: *spur,
                            // enable searching through fallback iif we can find the name at compile time
                            // to prevent unimported name conflicts finding the wrong value
                            enable_fallback: self.is_bound_variable(spur),
                        }])))
                    }
                }
            }
            ProgramData::Bool(b) => Ok(SyntaxReturn::Code(Box::from([Bytecode::PushBool {
                bool: *b,
            }]))),
            ProgramData::Char(c) => {
                simple_constant!(*c => Char)
            }
            ProgramData::Vector(v) => {
                let mut code = vec![];
                let length = v.len();
                for res in v.iter().map(|program| self.compile_code(ctx, *program)) {
                    code.extend(res?.into_bytecode());
                }
                code.push(Bytecode::MakeVector { length });
                Ok(SyntaxReturn::Code(code.into()))
            }
            // TODO these are valid in code, so handle them.
            // They are self-evaluating (code eval is same as quote eval... ish)
            ProgramData::Labeled { label, item } => {
                self.labeled_labels.insert(*label);
                Ok(SyntaxReturn::Code(
                    self.compile_code(ctx, *item)?
                        .into_bytecode()
                        .into_iter()
                        .chain([
                            Bytecode::FillHole { id: *label },
                            Bytecode::MakeHole { id: *label },
                        ])
                        .collect(),
                ))
            }
            ProgramData::LabelRef(label) => {
                self.label_ref_labels.insert(*label);
                Ok(SyntaxReturn::Code(Box::from([Bytecode::MakeHole {
                    id: *label,
                }])))
            }
            ProgramData::EmptyList => Err(CompileError::EmptyList(program.source)),
            ProgramData::List { head, body } => {
                // At this point, import and define-library don't have a special meaning anymore, so just interpret them as
                // their corresponding symbols
                let head_symbol = head.into_symbol(ctx.ecc.interner);
                if let Some(mcr) = head_symbol.and_then(|sym| self.get_macro(sym)) {
                    if *self.rec_counter.borrow() > Self::MAX_RECURSION {
                        return Err(CompileError::TooRecursive);
                    }
                    *self.rec_counter.borrow_mut(ctx) += 1;
                    let ret = mcr
                        .evaluate(ctx, self, self._current_env(), body)
                        .map_err(|e| CompileError::Macro(Arc::new(e), program.source));
                    *self.rec_counter.borrow_mut(ctx) -= 1;
                    ret
                } else {
                    let mut code = vec![];
                    let args = body.len();
                    for res in body.iter().map(|program| self.compile_code(ctx, *program)) {
                        code.extend(res?.into_bytecode());
                    }

                    let head_code = match head {
                        ListHead::Program(program) => self
                            .compile_code(ctx, *program)?
                            .into_bytecode()
                            .into_iter()
                            .collect(),
                        // head_symbol is Some(spur) where spur is the symbol we want
                        ListHead::DefineLibrary | ListHead::Import => {
                            let head_symbol = head_symbol.unwrap();
                            vec![Bytecode::Reference {
                                symbol: head_symbol,
                                enable_fallback: self.is_bound_variable(&head_symbol),
                            }]
                        }
                    };

                    for ref_upvalue in head_code.iter().filter_map(|c| {
                        if let Bytecode::FetchUpvalue { index } = c {
                            Some(*index)
                        } else {
                            None
                        }
                    }) {
                        // Force upvalue if we need to call it
                        if let Some(upvalue_def_code) = self.force_upvalue(ref_upvalue) {
                            code.extend(upvalue_def_code);
                        }
                    }
                    code.extend(head_code);

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
    ) -> Result<FxHashSet<lasso::Spur>, ImportError> {
        // - import: reads import sets, then searches local world (once implemented), then world, for the requisite module
        //   and importing the names as defined by spec

        fn get_library_name(imp: &ImportSet) -> Option<LibraryName> {
            match imp {
                ImportSet::Name(ln) => Some(ln.clone()),
                ImportSet::Only { set, .. } => get_library_name(set.as_ref()),
                ImportSet::Except { set, .. } => get_library_name(set.as_ref()),
                ImportSet::Prefix { set, .. } => get_library_name(set.as_ref()),
                ImportSet::Rename { set, .. } => get_library_name(set.as_ref()),
            }
        }

        // TODO We want an inverse solver?
        // Get a library from a library name
        // Then get all the terms that are defined in that library, and bubble up.

        let Some(library_name) = get_library_name(import_set) else {
            unreachable!("[ICE] import set did not specify a library name");
        };

        if from_code && !library_name.is_valid() {
            return Err(ImportError::InvalidImport);
        }

        // Unrecurive import set
        #[derive(Debug, Clone)]
        enum ImportOperations {
            Only {
                include: FxHashSet<lasso::Spur>,
            },
            Except {
                exclude: FxHashSet<lasso::Spur>,
            },
            Prefix {
                prefix: Arc<str>,
            },
            Rename {
                rename: FxHashMap<lasso::Spur, lasso::Spur>,
            },
        }

        fn unrecursivize(set: &ImportSet) -> impl IntoIterator<Item = ImportOperations> {
            match set {
                ImportSet::Name(_) => vec![],
                ImportSet::Only { set, symbols } => std::iter::once(ImportOperations::Only {
                    include: symbols.iter().copied().collect(),
                })
                .chain(unrecursivize(set))
                .collect(),
                ImportSet::Except { set, symbols } => std::iter::once(ImportOperations::Except {
                    exclude: symbols.iter().copied().collect(),
                })
                .chain(unrecursivize(set))
                .collect(),
                ImportSet::Prefix { set, prefix } => std::iter::once(ImportOperations::Prefix {
                    prefix: Arc::clone(prefix),
                })
                .chain(unrecursivize(set))
                .collect(),
                ImportSet::Rename { set, rename_pairs } => {
                    std::iter::once(ImportOperations::Rename {
                        rename: rename_pairs.iter().copied().collect(),
                    })
                    .chain(unrecursivize(set))
                    .collect()
                }
            }
        }

        // Get an inside-out list of the operations to get from the set of all symbols in the library to
        // a mapping of specific symbols to import -> what name to import them under
        let operations = {
            let mut ops: Vec<_> = unrecursivize(import_set).into_iter().collect();
            ops.reverse();
            ops
        };

        /// Apply operations in the given order, failing on an invalid operation, with the result being in the set and map
        /// given
        fn resolve_operations(
            ops: impl IntoIterator<Item = ImportOperations>,
            symbols: &mut FxHashSet<lasso::Spur>,
            mapping: &mut FxHashMap<lasso::Spur, lasso::Spur>,
            interner: &mut lasso::Rodeo,
            found: &FxHashSet<lasso::Spur>,
            fail_on_missing: bool,
        ) -> Result<(), ImportError> {
            let subset_symbols: FxHashSet<_> = symbols.union(found).copied().collect();
            for op in ops {
                match op {
                    ImportOperations::Only { include } => {
                        // Reverse map the include set into import terms
                        let include: FxHashSet<_> = include
                            .iter()
                            .map(|exp| {
                                mapping
                                    .iter()
                                    .find_map(|(k, v)| if v == exp { Some(*k) } else { None })
                                    .unwrap_or(*exp)
                            })
                            .collect();
                        if !include.is_subset(&subset_symbols) {
                            let include_not_symbols = include.difference(&subset_symbols);
                            if fail_on_missing {
                                return Err(ImportError::NamesNotFound {
                                    names: include_not_symbols
                                        .map(|i| Box::from(interner.resolve(i)))
                                        .collect(),
                                });
                            }
                        }
                        *symbols = include;
                    }
                    ImportOperations::Except { exclude } => {
                        // Reverse map the include set into import terms
                        let exclude: FxHashSet<_> = exclude
                            .iter()
                            .map(|exp| {
                                mapping
                                    .iter()
                                    .find_map(|(k, v)| if v == exp { Some(*k) } else { None })
                                    .unwrap_or(*exp)
                            })
                            .collect();
                        if !exclude.is_subset(&subset_symbols) {
                            let exclude_not_symbols = exclude.difference(&subset_symbols);
                            if fail_on_missing {
                                return Err(ImportError::NamesNotFound {
                                    names: exclude_not_symbols
                                        .map(|i| Box::from(interner.resolve(i)))
                                        .collect(),
                                });
                            }
                        }
                        *symbols = symbols.difference(&exclude).copied().collect();
                    }
                    ImportOperations::Rename { rename } => {
                        // Given rename < RF -> RT
                        // If RF reversed mapped is not in symbols, that is an error.
                        // Otherwise, change the mapping entry of RF to RT
                        let (keyed, not_found): (FxHashSet<_>, _) = rename
                            .keys()
                            .map(|exp| {
                                mapping
                                    .iter()
                                    .find_map(|(k, v)| if v == exp { Some(*k) } else { None })
                                    .ok_or(*exp)
                            })
                            .partition(Result::is_ok);
                        if !not_found.is_empty() && fail_on_missing {
                            return Err(ImportError::NamesNotFound {
                                names: not_found
                                    .into_iter()
                                    .map(|r| Box::from(interner.resolve(&r.unwrap_err())))
                                    .collect(),
                            });
                        }
                        let keyed: FxHashSet<_> = keyed.into_iter().map(|r| r.unwrap()).collect();
                        if !keyed.is_subset(&subset_symbols) {
                            let keyed_not_symbols = keyed.difference(&subset_symbols);
                            if fail_on_missing {
                                return Err(ImportError::NamesNotFound {
                                    names: keyed_not_symbols
                                        .map(|i| Box::from(interner.resolve(i)))
                                        .collect(),
                                });
                            }
                        }
                        let mapped = rename
                            .iter()
                            .map(|(exp, v)| {
                                let map = mapping
                                    .iter()
                                    .find_map(|(k, v)| if v == exp { Some(*k) } else { None })
                                    .unwrap_or(*exp);
                                (map, *v)
                            })
                            .collect::<Vec<_>>();
                        mapping.extend(mapped);
                    }
                    ImportOperations::Prefix { prefix } => {
                        //
                        for (_, v) in mapping.iter_mut() {
                            let source = interner.resolve(v);
                            let prefixed = interner.get_or_intern(format!("{prefix}{source}"));
                            *v = prefixed;
                        }
                    }
                }
            }
            Ok(())
        }

        fn self_ref_lambda_fix<'gc>(
            mc: &Mutation<'gc>,
            symbol: lasso::Spur,
            value: ValuePtr<'gc>,
        ) -> ValuePtr<'gc> {
            if let Value::Lambda(Lambda::Compiled(c)) = *value.borrow() {
                let chunk = c.chunk;
                if chunk
                    .code
                    .iter()
                    .any(|c| matches!(c, Bytecode::Reference { symbol: sym, .. } if sym == &symbol))
                {
                    // self-referential fix
                    let self_ref = Value::Undefined.into_ptr(mc);
                    let fallback = if let Some(f) = chunk.fallback {
                        let mut new_fallback = f.as_ref().clone();
                        new_fallback.insert(Static(symbol), (self_ref, 0));
                        new_fallback
                    } else {
                        let mut new_fallback = ImportFallbackMap::default();
                        new_fallback.insert(Static(symbol), (self_ref, 0));
                        new_fallback
                    };
                    let new_chunk = Chunk::with_fallback(
                        mc,
                        chunk.code.iter().copied(),
                        chunk.constants.iter().cloned(),
                        chunk.lambdas.iter().cloned(),
                        chunk.promises.iter().cloned(),
                        chunk.upvalues,
                        chunk.import_env,
                        chunk.labels.as_ref().clone(),
                        Some(Gc::new(mc, fallback)),
                    );
                    let new_lambda = Value::Lambda({
                        let l =
                            Lambda::Compiled(Gc::new(mc, CompiledLambda::new(c.arity, new_chunk)));
                        if let Some(uid) = c.upvalue_id {
                            l.label(mc, uid)
                        } else {
                            l
                        }
                    })
                    .into_ptr(mc);
                    // self-reference magic
                    *self_ref.borrow_mut(mc) = *new_lambda.borrow();
                    // Switch the value on the down-low
                    new_lambda
                } else {
                    // no self-reference done, so don't do any work
                    value
                }
            } else {
                value
            }
        }

        let mut found = FxHashSet::default();
        let mut names = FxHashSet::default();
        let mut found_library = false;

        // Try to find the library to start the import process
        if let Some(modl) = self.local_world.modules.get(&library_name).cloned() {
            found_library = true;
            let mut import_symbols: FxHashSet<_> =
                modl.exported_items.keys().map(|s| **s).collect();
            let mut import_mapping: FxHashMap<_, _> =
                import_symbols.iter().map(|s| (*s, *s)).collect();
            resolve_operations(
                operations.clone(),
                &mut import_symbols,
                &mut import_mapping,
                interner,
                &FxHashSet::default(),
                !world.has_library(&library_name),
            )?;
            // Extend "found" with any name found in the local (Scheme) module (used for existence checks)
            found.extend(modl.exported_items.keys().map(|k| k.0));
            // for each of the remaining symbols, search for them in the library, erroring if the symbol isn't found, then
            // get the mapped version of the symbol, and store it at that name
            for symbol in &import_symbols {
                let mapped_symbol = import_mapping.get(symbol).copied().unwrap_or(*symbol);
                if let Some(v) = modl.exported_items.get(&Static(*symbol)).cloned() {
                    match v {
                        ExportItem::Value(mut value) => {
                            if mapped_symbol != *symbol {
                                value = self_ref_lambda_fix(mc, *symbol, value);
                            }
                            self._current_env()
                                .borrow_mut(mc)
                                .define(mc, mapped_symbol, value, true)
                                .map_err(|_| ImportError::FailedToDefine(mapped_symbol))?;
                            self.define_variable(mapped_symbol);
                        }
                        ExportItem::Macro(syntax) => {
                            self.define_macro(mapped_symbol, syntax);
                        }
                        ExportItem::Transformer(tptr) => {
                            let mcr = self.install_transformer(tptr);
                            self.define_macro(mapped_symbol, mcr);
                        }
                    }
                    names.insert(mapped_symbol);
                } else if !world.has_library(&library_name) {
                    // We fail name resolution if there is no native module corresponding to this
                    // one, and the name was not found
                    Err(ImportError::NameNotFound {
                        name: Box::from(interner.resolve(&mapped_symbol)),
                    })?
                }
            }
        }

        // Get native names
        if let Some(modl) = world.library(&library_name) {
            found_library = true;
            let mut import_symbols: FxHashSet<_> = modl.all_symbols(interner).into_iter().collect();
            let mut import_mapping: FxHashMap<_, _> =
                import_symbols.iter().map(|s| (*s, *s)).collect();
            resolve_operations(
                operations,
                &mut import_symbols,
                &mut import_mapping,
                interner,
                &found,
                true,
            )?;

            // Import from the native library names that *haven't* been imported
            for symbol in import_symbols.difference(&found).copied() {
                let mapped_symbol = import_mapping.get(&symbol).copied().unwrap_or(symbol);
                // Check the cache first for already loaded symbols
                if let Some(item) = self
                    .native_cache
                    .entry(library_name.clone())
                    .or_default()
                    .get(&Static(symbol))
                    .cloned()
                {
                    match item {
                        NativeItem::Syntax(syntax) => {
                            self.define_macro(mapped_symbol, Arc::clone(&syntax));
                        }
                        NativeItem::Value(value) => {
                            self._current_env()
                                .borrow_mut(mc)
                                .define(mc, mapped_symbol, value, true)
                                .map_err(|_| ImportError::FailedToDefine(mapped_symbol))?;
                            self.define_variable(mapped_symbol);
                        }
                    }
                } else if let Some(syntax) = modl.syntax(interner, symbol) {
                    // Define a macro in scope
                    self.define_macro(mapped_symbol, Arc::clone(&syntax));
                    // cache result
                    self.native_cache
                        .entry(library_name.clone())
                        .or_default()
                        .insert(Static(symbol), NativeItem::Syntax(syntax));
                } else if let Some(mut value) = modl.value(mc, interner.resolve(&symbol)) {
                    // freeze a module imported value (TODO check how this actually impacts things)
                    if mapped_symbol != symbol {
                        value = self_ref_lambda_fix(mc, symbol, value);
                    }
                    self._current_env()
                        .borrow_mut(mc)
                        .define(mc, mapped_symbol, value, true)
                        .map_err(|_| ImportError::FailedToDefine(mapped_symbol))?;
                    // cache result
                    self.native_cache
                        .entry(library_name.clone())
                        .or_default()
                        .insert(Static(symbol), NativeItem::Value(value));
                    self.define_variable(mapped_symbol);
                } else {
                    Err(ImportError::NameNotFound {
                        name: Box::from(interner.resolve(&mapped_symbol)),
                    })?
                }
                names.insert(mapped_symbol);
            }
        }

        if !found_library {
            Err(ImportError::LibraryNotFound(
                library_name.to_string(interner),
            ))
        } else {
            Ok(names)
        }
    }

    /// Does this compiler have access to a library with the given name
    pub fn has_library(&self, name: &LibraryName, world: &World) -> bool {
        self.has_local_library(name) || world.has_library(name)
    }

    /// Does this compiler have access to a library defined in its local world with the given name
    pub fn has_local_library(&self, name: &LibraryName) -> bool {
        self.local_world.modules.contains_key(name)
    }

    /// Generate full features list
    pub fn features<S: AsRef<str>>(
        additional_features: impl IntoIterator<Item = S>,
        interner: &mut lasso::Rodeo,
    ) -> Vec<lasso::Spur> {
        let mut features = Self::base_features(interner);
        features.extend(
            additional_features
                .into_iter()
                .map(|addf| interner.get_or_intern(addf)),
        );
        features
    }

    /// Interpret a library definition into a given LocalWorld
    ///
    /// # Parameters
    /// - `mc`
    /// - `name`: library name to register
    /// - `ecc`: [`ExternalCompilerContext`]
    /// - `from_code`: will reserved names (null and names beginning with `scheme`, `srfi`, `magus`) be allowed through?
    /// - `library_def`: [`LibraryDefinitionContext`]
    /// - `library_decls`: library declarations
    pub fn define_library<'a>(
        &mut self,
        mc: &'a Mutation<'gc>,
        name: &LibraryName,
        ecc: &mut ExternalCompilerContext<'a>,
        from_code: bool,
        library_def: LibraryDefinitionContext<'a, 'gc>,
        library_decls: impl IntoIterator<Item = LibraryDeclaration<'gc>>,
    ) -> Result<(), DefineLibraryError> {
        // The '() module is private from Scheme code
        if from_code && !name.is_valid() {
            return Err(DefineLibraryError::EmptyLibraryName);
        }

        // We reserve all modules name with the first component 'scheme, 'srfi, and 'magus
        let reserved_starts =
            ["scheme", "srfi", "magus"].map(|s| ecc.interner.get_or_intern_static(s));
        if from_code
            && matches!(name.0[0], LibraryNameItem::Identifier(ref id) if reserved_starts.contains(id))
        {
            return Err(DefineLibraryError::ReservedName(name.clone()));
        }

        // If a name can be found, that is *also* an error
        if self.local_world.modules.contains_key(name) {
            return Err(DefineLibraryError::AlreadyExists(name.clone()));
        }
        // We don't look at the world b/c we allow native and Scheme libraries with the same name to exist.
        // The Scheme library is searched first, then the native library. This allows for "Scheme postludes" to
        // implement functionality and niceties using Scheme on top of natively implement functionality.

        // The compiler we will use to compile the library (so that the only interface between these compilers is
        // the library export interface.)
        let mut lib_compiler = Compiler::new(mc);
        // Copy over our local world (for those local defs)
        lib_compiler.local_world = self.local_world.clone();

        // When compiling we use the *exact* same ecc, so that the values are compatible with *this* interpreter (as
        // we would be using the same interner)

        // For execution, we either use one big blob of fuel, or (when None) refill in units of 1,000 fuel
        let mut fuel = Fuel::with(library_def.max_fuel.unwrap_or(1_000));
        let global_env = Gc::new(
            mc,
            RefLock::new(StackEnvironment::new(
                mc,
                Some(lib_compiler.default_environment_ptr()),
            )),
        );

        let mut imported = FxHashSet::default();
        let mut export_sets = HashSet::new();
        let mut decls = VecDeque::from_iter(library_decls);

        #[expect(clippy::too_many_arguments)]
        fn execute_code<'a, 'gc>(
            mc: &'a Mutation<'gc>,
            code: &[ProgramPtr<'gc>],
            name: &LibraryName,
            ecc: &mut ExternalCompilerContext<'a>,
            library_def: LibraryDefinitionContext<'a, 'gc>,
            lib_compiler: &mut Compiler<'gc>,
            global_env: StackEnvironmentPtr<'gc>,
            fuel: &mut Fuel,
        ) -> Result<(), DefineLibraryError> {
            // this is the "fun" one. we use the repl substitution trick to make `global_env` our global environment
            // when using thread. But first, we gotta compile in our compiler.
            let chunk = lib_compiler
                .compile_no_import(mc, ecc, library_def, code.iter().copied())
                .map_err(Box::new)?;

            let thread = library_def.thread;
            thread.borrow_mut(mc).include(mc, chunk, false);
            let vp = library_def.value_pointers;
            let ctx = crate::interpreter::Context {
                mc,
                thread,
                null_value: vp.null_value,
                true_value: vp.true_value,
                false_value: vp.false_value,
            };
            // repl trick to share envs (to be made "official" with a nice interface)
            *thread.borrow_mut(mc).env().unwrap().borrow_mut(mc) = *global_env.borrow();
            // reparent the global_env to the chunk parent
            global_env.borrow_mut(mc).reparent(Some(chunk.import_env));
            while fuel.remaining() > 0 && !thread.borrow().is_finished() {
                thread
                    .borrow_mut(mc)
                    .step(ctx, ecc.interner, ecc.world, ecc.includer, fuel);
                if library_def.max_fuel.is_none() {
                    fuel.refill(1_000, 1_000);
                }
            }

            if !thread.borrow().is_finished() {
                // we ran outta fuel
                Err(DefineLibraryError::OutOfFuel(name.to_string(ecc.interner)))
            } else {
                // .. borrowck sometime we fightin
                let mut thread_mut = thread.borrow_mut(mc);
                let ret = if let Some(Err(e)) = thread_mut.result() {
                    // Rendered to string b/c DefineLibraryError is currently not using the 'gc lifetime
                    let err = DefineLibraryError::InterpError(Box::from(
                        e.display(ecc.interner, []).to_string().as_str(),
                    ));
                    Err(err)
                } else {
                    // reset thread before continuing
                    thread_mut.reset();
                    Ok(())
                };
                if ret.is_err() {
                    // reset thread on interp error
                    thread_mut.reset_error();
                }
                ret
            }
        }

        // Generate features list
        let features = Self::features(library_def.additional_features.unwrap_or(&[]), ecc.interner);

        let mut library_code = vec![];

        while let Some(decl) = decls.pop_front() {
            match decl {
                LibraryDeclaration::Import(imports) => {
                    for import in imports.iter() {
                        let imports =
                            lib_compiler.import(mc, ecc.interner, ecc.world, import, true)?;
                        if !imported.is_disjoint(&imports) {
                            return Err(DefineLibraryError::DoubleImport(Box::from_iter(
                                imported.intersection(&imports).copied(),
                            )));
                        }
                        imported.extend(imports);
                    }
                }
                LibraryDeclaration::IncludeLibraryDeclarations(filenames) => {
                    for filename in filenames.iter() {
                        // AFAICT this works like `include` so case-sensitivity is reset to "sensitive".
                        let code = (
                            &filename,
                            ecc.includer
                                .include(filename)
                                .map_err(DefineLibraryError::Include)?,
                        )
                            .parse_program(mc, ecc.interner, false)?;
                        for decl in code
                            .into_iter()
                            .rev()
                            .map(|p| LibraryDeclaration::convert(p, mc, ecc.interner))
                            .collect::<Result<Vec<_>, _>>()?
                        {
                            decls.push_front(decl);
                        }
                    }
                }
                LibraryDeclaration::Include {
                    filenames,
                    case_insensitive,
                } => {
                    for filename in filenames.iter() {
                        let code = (
                            &filename,
                            ecc.includer
                                .include(filename)
                                .map_err(DefineLibraryError::Include)?,
                        )
                            .parse_program(
                                mc,
                                ecc.interner,
                                case_insensitive,
                            )?;
                        library_code.extend(code);
                    }
                }
                LibraryDeclaration::Begin(code) => {
                    library_code.extend(code.iter());
                }
                LibraryDeclaration::Export(names) => {
                    export_sets.extend(names.iter().copied());
                }
                LibraryDeclaration::CondExpand {
                    branches,
                    else_branch,
                } => {
                    let mut branch_satisfied = false;
                    for (req, cond_decls) in branches.iter() {
                        if req.is_satisfied(self, ecc.world, &features) {
                            branch_satisfied = true;
                            for decl in cond_decls.iter() {
                                decls.push_front(decl.clone());
                            }
                            // Ignore the remaining clauses
                            break;
                        }
                    }
                    if !branch_satisfied {
                        // expand else branch
                        if let Some(else_branch) = else_branch {
                            for decl in else_branch.iter() {
                                decls.push_front(decl.clone());
                            }
                        }
                    }
                }
            }
        }

        // Execute all code defined in libraries (done all at once here, so that cond-expand is handled nicely)
        execute_code(
            mc,
            &library_code,
            name,
            ecc,
            library_def,
            &mut lib_compiler,
            global_env,
            &mut fuel,
        )?;

        // Handle all exports at the end! (b/c export decls can come *before* the items they define)
        let mut exports = fxhash::FxHashMap::default();
        let mut memo_nameddeps =
            FxHashMap::<lasso::Spur, (bool, FxHashSet<lasso::Spur>)>::default();
        let mut find_item = |name: &lasso::Spur| -> Option<ExportItem<'gc>> {
            // search first for macros, then as a value in the global scope
            if let Some(syntax) = lib_compiler.syntax_items.get(name) {
                if let Some(key) = syntax.is_transformer() {
                    lib_compiler
                        .stash
                        .transformers
                        .get(key)
                        .copied()
                        .map(ExportItem::Transformer)
                } else {
                    Some(ExportItem::Macro(Arc::clone(syntax)))
                }
            } else if let Ok(binding) = global_env.borrow().get(*name) {
                // TODO A lambda can depend on references, so go through the code of compiled lambdas and
                // add as a "dependency" any reference it or its lambdas depend on? (as long as it is not defined in scope!)
                binding.read(|v| {
                    if let Value::Lambda(Lambda::Compiled(c)) = *v.borrow() {
                        fn get_global_refs(
                            name: lasso::Spur,
                            code: &[Bytecode],
                        ) -> fxhash::FxHashSet<lasso::Spur> {
                            // defined not as an argument
                            code.iter()
                                .scan(fxhash::FxHashSet::from_iter([name]), |s, c| {
                                    if let Bytecode::Define { symbol } = c {
                                        s.insert(*symbol);
                                        Some(None)
                                    } else if let Bytecode::Reference { symbol, .. } = c {
                                        if !s.contains(symbol) {
                                            Some(Some(*symbol))
                                        } else {
                                            Some(None)
                                        }
                                    } else {
                                        Some(None)
                                    }
                                })
                                .flatten()
                                .collect()
                        }

                        fn get_deps(
                            name: lasso::Spur,
                            c: &CompiledLambda<'_>,
                        ) -> FxHashSet<lasso::Spur> {
                            let mut dependants = get_global_refs(name, &c.chunk.code);
                            let referenced_lambdas = c
                                .chunk
                                .code
                                .iter()
                                .filter_map(|c| {
                                    if let Bytecode::PushLambda { index } = c {
                                        Some(*index)
                                    } else {
                                        None
                                    }
                                })
                                .collect::<FxHashSet<_>>();
                            // Check depending values and add any names that *those* depend on (if they are also compiled lambdas)
                            for lmbr in referenced_lambdas {
                                let Lambda::Compiled(l) = &c.chunk.lambdas[lmbr] else {
                                    continue;
                                };
                                dependants.extend(get_global_refs(name, &l.chunk.code));
                            }
                            dependants
                        }

                        let dependants = if let Some((full, deps)) = memo_nameddeps.get(name) {
                            let mut dependants = deps.clone();
                            if *full {
                                dependants
                            } else {
                                for dname in dependants.clone().into_iter() {
                                    if let Ok(Value::Lambda(Lambda::Compiled(c))) = global_env
                                        .borrow()
                                        .get(dname)
                                        .map(|bnd| *(*bnd.get().borrow()).borrow())
                                    {
                                        let subdeps = get_deps(dname, &c);
                                        dependants.extend(subdeps);
                                    }
                                }
                                memo_nameddeps.insert(*name, (true, dependants.clone()));
                                dependants
                            }
                        } else {
                            let mut dependants = get_deps(*name, &c);
                            for name in dependants.clone().into_iter() {
                                if let Ok(Value::Lambda(Lambda::Compiled(c))) = global_env
                                    .borrow()
                                    .get(name)
                                    .map(|bnd| *(*bnd.get().borrow()).borrow())
                                {
                                    let subdeps = get_deps(name, &c);
                                    memo_nameddeps.insert(name, (false, subdeps.clone()));
                                    dependants.extend(subdeps);
                                }
                            }
                            memo_nameddeps.insert(*name, (true, dependants.clone()));
                            dependants
                        };

                        // eprintln!(
                        //     "{} -> {:?}",
                        //     ecc.interner.resolve(name),
                        //     dependants
                        //         .iter()
                        //         .map(|s| ecc.interner.resolve(s))
                        //         .collect::<Vec<_>>()
                        // );

                        let mut map = ImportFallbackMap::default();
                        for dependant in dependants {
                            if let Ok(val) = global_env.borrow().get(dependant) {
                                let depth = global_env.borrow().depth(dependant).unwrap();
                                map.insert(
                                    Static(dependant),
                                    (*val.get().borrow(), depth + self.environments.len()),
                                );
                            }
                        }

                        let fallback = if !map.is_empty() {
                            Some(Gc::new(mc, map))
                        } else {
                            None
                        };

                        // Recreate the chunk with compiled lambdas referencing this fallback
                        // TODO Might be *too* permissive, but w/e for now
                        // and this might be *uber* buggy, but this *kinda* works, so fix things from
                        // this starting point
                        let chunk = c.chunk;
                        let new_chunk = Chunk::with_fallback(
                            mc,
                            chunk.code.iter().copied(),
                            chunk.constants.iter().cloned(),
                            chunk.lambdas.iter().cloned(),
                            chunk.promises.iter().cloned(),
                            chunk.upvalues,
                            chunk.import_env,
                            chunk.labels.as_ref().clone(),
                            fallback,
                        );

                        let mut lambda = Gc::new(mc, CompiledLambda::new(c.arity, new_chunk));

                        if let Some(label) = c.upvalue_id {
                            lambda = lambda.label(mc, label);
                        }

                        Some(ExportItem::Value(
                            Value::Lambda(Lambda::Compiled(lambda)).into_ptr(mc),
                        ))
                    } else {
                        Some(ExportItem::Value(*v))
                    }
                })
            } else {
                None
            }
        };
        for name in export_sets {
            match name {
                ExportSet::Name(spur) => {
                    let item = find_item(&spur).ok_or(DefineLibraryError::UndefinedExport(
                        Box::from(ecc.interner.resolve(&spur)),
                    ))?;
                    exports.insert(spur, item);
                }
                ExportSet::Rename { from, to } => {
                    let item = find_item(&from).ok_or(DefineLibraryError::UndefinedExport(
                        Box::from(ecc.interner.resolve(&from)),
                    ))?;
                    exports.insert(to, item);
                }
            }
        }

        // If we get through all the declarations without running out of fuel, *then* we define the library
        let new_library = SchemeLibrary {
            exported_items: exports.into_iter().map(|(k, v)| (Static(k), v)).collect(),
        };

        self.local_world.modules.insert(name.clone(), new_library);
        Ok(())
    }

    /// Checks if an expression is considered a definition by Scheme
    pub fn is_definition(&self, program: ProgramPtr<'gc>) -> bool {
        let definition_symbols = self
            .syntax_items
            .iter()
            .filter_map(|(k, syn)| syn.is_definition(program, self).then_some(*k))
            .collect::<fxhash::FxHashSet<_>>();
        let container_symbols = self
            .syntax_items
            .iter()
            .filter_map(|(k, syn)| {
                let to_check = syn.is_container(program, self);
                (!to_check.is_empty()).then_some((*k, to_check))
            })
            .collect::<fxhash::FxHashMap<_, _>>();

        match &program.data {
            ProgramData::List { head, .. } => {
                matches!(head, ListHead::Program(p) if match p.data {
                    ProgramData::Symbol(s) if definition_symbols.contains(&s) => true,
                    ProgramData::Symbol(s)
                        if container_symbols.contains_key(&s)
                            && container_symbols.get(&s).unwrap().iter().all(|bp| self.is_definition(*bp)) =>
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

    /// Create a new environment (with the given pointer as its parent)
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

    /// Find the definition of an upvalue at a point, then force it's definition
    fn force_upvalue(&self, upvalue_index: usize) -> Option<Vec<Bytecode>> {
        for argument_scope in self.scopes.iter().rev() {
            for (name, v) in argument_scope.variables_defined.borrow_mut().iter_mut() {
                if &Some(upvalue_index) == v {
                    *v = None;
                    return Some(vec![
                        Bytecode::Reference {
                            symbol: *name,
                            enable_fallback: true,
                        },
                        Bytecode::SetUpvalue {
                            index: upvalue_index,
                        },
                        Bytecode::Pop,
                    ]);
                }
            }
        }
        None
    }

    /// Helper function for pushing arguments to certain names (while also handling upvalues)
    pub fn lambda_prelude(&self) -> impl IntoIterator<Item = Bytecode> {
        // This should be called when all upvalues are known
        if let Some(argument_scope) = self.scopes.last() {
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
                    } else if argument_scope.requested_names.contains(&symbol) {
                        vec![Bytecode::FetchArg { index }, Bytecode::Define { symbol }]
                    } else {
                        // vec![Bytecode::FetchArg { index }, Bytecode::Define { symbol }]
                        vec![]
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
                            } else if argument_scope.requested_names.contains(&symbol) {
                                vec![Bytecode::FetchRest, Bytecode::Define { symbol }]
                            } else {
                                // vec![Bytecode::FetchRest, Bytecode::Define { symbol }]
                                vec![]
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
                .flat_map(|(vn, upv)| {
                    if let Some(upv) = upv {
                        vec![
                            Bytecode::Reference {
                                symbol: *vn,
                                enable_fallback: true,
                            },
                            Bytecode::SetUpvalue { index: *upv },
                            Bytecode::Pop,
                        ]
                    } else if let Some(up_index) = argument_scope
                        .args
                        .iter()
                        .position(|s| s == vn)
                        .and_then(|pos| argument_scope.upvalues.borrow().get(&Some(pos)).copied())
                    {
                        vec![
                            Bytecode::Reference {
                                symbol: *vn,
                                enable_fallback: true,
                            },
                            Bytecode::SetUpvalue { index: up_index },
                            Bytecode::Pop,
                        ]
                    } else if let Some(up_index) = argument_scope
                        .rest
                        .filter(|s| s == vn)
                        .and_then(|_| argument_scope.upvalues.borrow().get(&None).copied())
                    {
                        vec![
                            Bytecode::Reference {
                                symbol: *vn,
                                enable_fallback: true,
                            },
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
    pub fn hygenic<'a, T>(
        &'a mut self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        f: impl FnOnce(
            &mut SyntaxContext<'_, '_, 'gc>,
            &mut Compiler<'gc>,
            StackEnvironmentPtr<'gc>,
        ) -> T,
    ) -> T {
        let new_env = self.new_environment(ctx, import_env);
        self.hygenic_with_env(ctx, Some(new_env), f)
    }

    /// Helper for a hygenic context in a given environment
    pub fn hygenic_with_env<T>(
        &mut self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        env_spec: Option<EnvironmentSpec>,
        f: impl FnOnce(
            &mut SyntaxContext<'_, '_, 'gc>,
            &mut Compiler<'gc>,
            StackEnvironmentPtr<'gc>,
        ) -> T,
    ) -> T {
        let checkpoint = self.checkpoint();
        let old_env = self.current_environment();
        self.environment(env_spec);
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
        self.scopes
            .iter()
            .rev()
            .enumerate()
            .find_map(|(scope, args)| {
                if matches!(args.rest, Some(rest) if symbol == rest) {
                    Some(Arg::Rest { scope })
                } else {
                    args.args
                        .iter()
                        .position(|s| *s == symbol)
                        .map(|index| Arg::Index { scope, index })
                }
            })
    }

    /// Checks if a name is bound to *any* variable name
    pub fn is_bound_variable(&self, symbol: &lasso::Spur) -> bool {
        self.global_variables_defined.contains(symbol)
            || self
                .scopes
                .last()
                .is_some_and(|sc| sc.variables_defined.borrow().contains_key(symbol))
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
                    || args.variables_defined.borrow().contains_key(&symbol)
            })
        {
            return None;
        }

        self.syntax_items.get(&symbol).cloned()
    }

    /// Create an ArcSyntax from a [`TransformerPtr`]
    pub fn install_transformer(&mut self, transformer: TransformerPtr<'gc>) -> ArcSyntax {
        let new_knob = Arc::new(());
        let key = self.stash.transformers.insert(transformer);
        self.stash.transformer_knobs.insert(key, new_knob.clone());
        let syntax = PrivateTransformer {
            key,
            _knob: new_knob,
        };
        Arc::new(syntax)
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
            // insert None if not defined
            scope
                .variables_defined
                .borrow_mut()
                .entry(symbol)
                .or_default();
        } else {
            self.global_variables_defined.insert(symbol);
        }
    }

    pub fn define_parameters(
        &mut self,
        interner: &mut lasso::Rodeo,
        args: impl IntoIterator<Item = lasso::Spur>,
        rest: Option<lasso::Spur>,
    ) -> Result<(), anyhow::Error> {
        let args: Rc<[lasso::Spur]> = args.into_iter().collect();
        let unique: FxHashSet<_> = args.iter().copied().collect();
        if unique.len() != args.len() {
            let repeated_params = unique
                .iter()
                .map(|s| (*s, args.iter().filter(|ps| ps == &s).count()))
                .filter(|(_, c)| *c > 1)
                .map(|(sym, _)| interner.resolve(&sym))
                .collect::<Vec<_>>();
            return Err(anyhow::anyhow!("parameters repeated: {repeated_params:?}"));
        }
        // Create and push a new argument scope
        self.scopes.push(Scope {
            args,
            rest,
            ..Default::default()
        });
        Ok(())
    }

    /// Get the [`Scope`] of a given scope where 0 is local, 1 is parent, etc..
    pub fn argument_scope(&self, scope: usize) -> Option<&Scope> {
        self.scopes.iter().rev().nth(scope)
    }

    /// Get the [`Scope`] of a given scope where 0 is local, 1 is parent, etc..
    pub fn argument_scope_mut(&mut self, scope: usize) -> Option<&mut Scope> {
        self.scopes.iter_mut().rev().nth(scope)
    }
}
