//! Execution model for Scheme code

use std::{rc::Rc, sync::Arc};

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc, Mutation};

use crate::{
    environment::StackEnvironmentPtr,
    runtime::lambda::{Arity, CompiledLambdaPtr},
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
    /// Multiple returns are turned into a "values" object, which represent multiple items that were returned by a
    /// procedure. To work with the items individually, they must be unpacked (and we reuse arity
    /// to represent how many values were expected to be unpacked onto the stack, so we can revert and error
    /// if an unexpected amount occurs)
    Unpack { amount: Arity },

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
    /// Branching instruction
    ///
    /// Jump forward by a certain number of instructions if the value popped from the top of the stack is false (any other
    /// value is considered true)
    If { jump: usize },
    /// Jump forward a certain number of instructions
    ///
    /// Used for `if` on the true branch
    Jump { jump: usize },

    /// Duplicate the reference to the value at the top of the stack
    Duplicate,
    // /// Pop the value at the top of the stack
    // Pop,
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
            Self::FetchArg { .. } => 1,
            Self::FetchRest { .. } => 1,
            Self::MakePair => 1,
            Self::MakeHole { .. } => 1,
            Self::FillHole { .. } => 1,
            Self::MakeVector { .. } => 1,
            Self::Reference { .. } => 1,
            Self::Unpack { .. } => 1,
            Self::Call { .. } => 4,
            Self::Define { .. } => 2,
            Self::SetBang { .. } => 2,
            Self::If { .. } => 2,
            Self::Jump { .. } => 2,
            Self::Duplicate => 1,
            // Self::Pop => 1,
            // Self::Return => 4,
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
    /// symbols this chunk references
    #[collect(require_static)]
    pub constants: Rc<[Constant]>,
    /// lambdas this chunk defines
    pub lambdas: Rc<[CompiledLambdaPtr<'gc>]>,
    /// environment this chunk references
    // We only need 1 because of the fact that a Scheme program is all the imports *then*
    // commands and definitions
    pub import_env: StackEnvironmentPtr<'gc>,
    #[collect(require_static)]
    pub code: Rc<[Bytecode]>,
    /// Hash map of code locations to SourceData
    #[collect(require_static)]
    pub labels: Rc<FxHashMap<usize, SourceData>>,
}
pub type ChunkPtr<'gc> = Gc<'gc, Chunk<'gc>>;

impl<'gc> Chunk<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        code: impl IntoIterator<Item = Bytecode>,
        constants: impl IntoIterator<Item = Constant>,
        lambdas: impl IntoIterator<Item = CompiledLambdaPtr<'gc>>,
        import_stack_env: StackEnvironmentPtr<'gc>,
        labels: FxHashMap<usize, SourceData>,
    ) -> ChunkPtr<'gc> {
        let chunk = Self {
            code: code.into_iter().collect(),
            constants: constants.into_iter().collect(),
            lambdas: lambdas.into_iter().collect(),
            import_env: import_stack_env,
            labels: Rc::new(labels),
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
