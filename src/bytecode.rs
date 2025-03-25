//! Execution model for Scheme code

use core::fmt;
use std::{collections::HashMap, rc::Rc, sync::Arc};

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc, Mutation, Static};

use crate::{
    ValuePtr, environment::StackEnvironmentPtr, runtime::lambda::Lambda, value::PromisePtr,
};

/*
compiled form is at its root primitive forms:
<literal>
<variable ref>
<procedure (function/macro) call>
<macro call>

--- below are "definitive forms" where the implementations
--- are *definitely* provided on the interpreter side natively (in Rust code)
define
lambda
if
set!

then provide Rust-side impls for the rest of the standard library (and/or mix it with
Scheme-impls)
*/
// NOTE For any code that references the environment, it also *must* have an
// `import_env` member that specifies one of the import envs to fallback to
// in case it isn't found in the current environment (None means to not fallback,
// this is generally the case with Scheme code read from a file, before any imports....
// you can't really do much in that kind of environment)
#[derive(Debug, Clone, Copy)]
pub enum Bytecode {
    /// Push the null cons to the stack
    PushNull,
    /// Push a void value to the stack
    PushVoid,
    /// Push to stack a constant value at a given index of the constant table
    PushConst { index: usize },
    /// Push a boolean value to stack
    PushBool { bool: bool },
    /// Push a compiled lambda to the stack
    PushLambda { index: usize },
    /// Push a promise to the stack
    PushPromise { index: usize },
    /// Sets an upvalue referencing the value at the top of stack (does not pop)
    SetUpvalue { index: usize },
    /// Fetch an upvalue (an argument of a parent scope)
    FetchUpvalue { index: usize },
    /// Fetch args from the current scope
    FetchArg { index: usize },
    /// Fetch the rest arg from the current scope
    FetchRest,
    /// Pop the top 2 arguments from the stack and make a cons cell out of them
    /// (fails if either of the 2 arguments are undefined)
    MakePair,
    /// Make a vector (popping from stack), using the amount specified as the number of
    /// items
    MakeVector { length: usize },
    /// Look up the symbol in the stack environment, and push the result to
    /// stack (if not found or not a symbol, errors)
    Reference { symbol: lasso::Spur },
    /// Pop the top value (must be a callable)
    /// Call the given lambda, making it a tail call if possible (there are
    /// no more instructions in the current context to execute)
    Call { args: usize },
    /// Pop a list and a value, and append the value to the list, pushing the list back to
    /// stack
    Splice,

    // Holes are the way to make self-referential datatypes
    /// Creates a hole for self-reference
    MakeHole { id: usize },
    /// Pop the top of the stack as the value of a hole, and clear the hole.
    FillHole { id: usize },

    // NOTE These are the "definitive forms" that are
    // theoretically all that's needed to implement the
    // Scheme standard library (other than native procedures)
    // (We only need `define`, `set!`, and `if` as explicit instructions,
    // b/c `lambda`, is more "make a compiled lambda, add it to the chunk,
    // the used the `PushLambda` instructions to push it to stack.")
    /// Pop the value on the stack, and define a given symbol using that value.
    Define { symbol: lasso::Spur },
    /// Pop the value on the stack and set! a given symbol using that value
    /// (error if the symbol is not already defined in the environment)
    SetBang { symbol: lasso::Spur },
    /// Set the value of an upvalue if it exists
    SetBangUpvalue { index: usize },
    /// Branching instruction
    ///
    /// Jump forward by a certain number of instructions if the value popped from the top of the stack is false (any other
    /// value is considered true)
    If { jump: usize },
    /// Jump forward a certain number of instructions
    ///
    /// Used for `if` on the true branch
    Jump { jump: usize },
    /// Force the top of the stack if it is a promise.
    /// Otherwise, does nothing.
    Force,

    /// Duplicate the reference to the value at the top of the stack
    Duplicate,
    /// Pop the value at the top of the stack
    // This should be a rare instruction to emit, mostly used to get upvalues working
    // nicely with definitions
    Pop,
    // /// Explicitly end an execution frame
    // Return,
}

impl Bytecode {
    /// Calculate the VM fuel cost of an instruction
    pub fn cost(&self) -> i32 {
        match self {
            Self::PushNull => 1,
            Self::PushVoid => 1,
            Self::PushConst { .. } => 1,
            Self::PushBool { .. } => 1,
            Self::PushLambda { .. } => 1,
            Self::PushPromise { .. } => 1,
            Self::SetUpvalue { .. } => 1,
            Self::FetchUpvalue { .. } => 1,
            Self::FetchArg { .. } => 1,
            Self::FetchRest { .. } => 1,
            Self::MakePair => 1,
            Self::MakeHole { .. } => 1,
            Self::FillHole { .. } => 1,
            Self::MakeVector { .. } => 1,
            Self::Reference { .. } => 1,
            Self::Splice => 1,
            Self::Call { .. } => 4,
            Self::Force => 4,
            Self::Define { .. } => 2,
            Self::SetBang { .. } => 2,
            Self::SetBangUpvalue { .. } => 2,
            Self::If { .. } => 2,
            Self::Jump { .. } => 2,
            Self::Duplicate => 1,
            Self::Pop => 1,
            // Self::Return => 4,
        }
    }
}

// Nice (?) mnemonic display for bytecode
impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bytecode::PushNull => write!(f, "NULL"),
            Bytecode::PushVoid => write!(f, "VOID"),
            Bytecode::PushConst { index } => write!(f, "CNST {index}"),
            Bytecode::PushBool { bool } => {
                write!(f, "BOOL {}", if *bool { "#t" } else { "#f" })
            }
            Bytecode::PushLambda { index } => write!(f, "LMBD {index}"),
            Bytecode::PushPromise { index } => write!(f, "PROM {index}"),
            Bytecode::SetUpvalue { index } => write!(f, "UPVL {index}"),
            Bytecode::FetchUpvalue { index } => write!(f, "FUPV {index}"),
            Bytecode::FetchArg { index } => write!(f, "FARG {index}"),
            Bytecode::FetchRest => write!(f, "REST"),
            Bytecode::MakePair => write!(f, "PAIR"),
            Bytecode::MakeVector { length } => write!(f, "VECT {length}"),
            Bytecode::Reference { symbol } => write!(f, "REFR {}", symbol.into_inner()),
            Bytecode::Call { args } => write!(f, "CALL {args}"),
            Bytecode::Splice => write!(f, "SPLI"),
            Bytecode::MakeHole { id } => write!(f, "HOLE {id}"),
            Bytecode::FillHole { id } => write!(f, "FILL {id}"),
            Bytecode::Define { symbol } => write!(f, "DEFN {}", symbol.into_inner()),
            Bytecode::SetBang { symbol } => write!(f, "SET! {}", symbol.into_inner()),
            Bytecode::SetBangUpvalue { index } => write!(f, "SETU {}", index),
            Bytecode::If { jump } => write!(f, "JMIF {jump}"),
            Bytecode::Jump { jump } => write!(f, "JUMP {jump}"),
            Bytecode::Force => write!(f, "FORS"),
            Bytecode::Duplicate => write!(f, "DUPL"),
            Bytecode::Pop => write!(f, "SPOP"),
        }
    }
}

/// Constant values, that can be pushed to stack as-is
///
/// Mostly used to store "primitive" values, and correspond to external
/// representation types (except for list)
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Symbol(lasso::Spur),
    Char(char),
    Number(i64),
    // TODO Support exact rational numbers
    // (We can use BigRational directly here b/c Copy is not required as it is in Value)
    // (well it's more likely (due to how our frontend works) to support Rational64 instead)
    Inexact(f64),
    String(Arc<str>),
    Bytevector(Arc<[u8]>),
}

#[derive(Debug, Clone, Copy)]
pub struct SourceData {
    pub source_id: lasso::Spur,
    pub range: (usize, usize),
}

impl SourceData {
    #[inline]
    pub fn source_id(self) -> lasso::Spur {
        self.source_id
    }

    #[inline]
    pub fn range(self) -> (usize, usize) {
        self.range
    }
}

/// A chunk of bytecode
#[derive(Collect, Debug)]
#[collect(no_drop)]
// NOTE Chunks are not thread-safe and are immutable, so to make them *really* cheap to clone,
// we can use Rc
pub struct Chunk<'gc> {
    /// number of upvalues this chunk (and subchunks) can use
    pub upvalues: usize,
    /// symbols this chunk references
    #[collect(require_static)]
    pub constants: Rc<[Constant]>,
    /// lambdas this chunk defines
    pub lambdas: Rc<[Lambda<'gc>]>,
    /// promises this chunk defines
    pub promises: Rc<[PromisePtr<'gc>]>,
    /// environment this chunk references
    // We only need 1 because of the fact that a Scheme program is all the imports *then*
    // commands and definitions
    pub import_env: StackEnvironmentPtr<'gc>,
    #[collect(require_static)]
    pub code: Rc<[Bytecode]>,
    /// Hash map of code locations to SourceData
    #[collect(require_static)]
    pub labels: Rc<FxHashMap<usize, SourceData>>,
    /// When importing a lambda, it might refer to things in its defining library scope
    /// that aren't imported into the program scope. For these references, libraries
    /// can store the names here, so that if all else fails, the values can still be referenced.
    pub(crate) fallback: ImportFallback<'gc>,
}
pub type ChunkPtr<'gc> = Gc<'gc, Chunk<'gc>>;
pub type ImportFallbackMap<'gc> = HashMap<Static<lasso::Spur>, ValuePtr<'gc>>;
pub type ImportFallback<'gc> = Option<Gc<'gc, ImportFallbackMap<'gc>>>;

impl<'gc> Chunk<'gc> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        mc: &Mutation<'gc>,
        code: impl IntoIterator<Item = Bytecode>,
        constants: impl IntoIterator<Item = Constant>,
        lambdas: impl IntoIterator<Item = Lambda<'gc>>,
        promises: impl IntoIterator<Item = PromisePtr<'gc>>,
        upvalues: usize,
        import_stack_env: StackEnvironmentPtr<'gc>,
        labels: FxHashMap<usize, SourceData>,
    ) -> ChunkPtr<'gc> {
        let chunk = Self {
            code: code.into_iter().collect(),
            constants: constants.into_iter().collect(),
            lambdas: lambdas.into_iter().collect(),
            promises: promises.into_iter().collect(),
            upvalues,
            import_env: import_stack_env,
            labels: Rc::new(labels),
            fallback: None,
        };

        Gc::new(mc, chunk)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn with_fallback(
        mc: &Mutation<'gc>,
        code: impl IntoIterator<Item = Bytecode>,
        constants: impl IntoIterator<Item = Constant>,
        lambdas: impl IntoIterator<Item = Lambda<'gc>>,
        promises: impl IntoIterator<Item = PromisePtr<'gc>>,
        upvalues: usize,
        import_stack_env: StackEnvironmentPtr<'gc>,
        labels: FxHashMap<usize, SourceData>,
        fallback: ImportFallback<'gc>,
    ) -> ChunkPtr<'gc> {
        let chunk = Self {
            code: code.into_iter().collect(),
            constants: constants.into_iter().collect(),
            lambdas: lambdas.into_iter().collect(),
            promises: promises.into_iter().collect(),
            upvalues,
            import_env: import_stack_env,
            labels: Rc::new(labels),
            fallback,
        };

        Gc::new(mc, chunk)
    }

    /// Find the corresponding [`SourceData`] for a given index into bytecode
    pub fn find_label(&self, pc: usize) -> Option<SourceData> {
        self.labels
            .keys()
            .filter(|k| **k <= pc)
            .max()
            .and_then(|idx| self.labels.get(idx).copied())
    }
}
