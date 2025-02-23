//! Execution model for Scheme code

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc};

use crate::environment::StackEnvironmentPtr;

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
    /// Push to stack a constant value at a given index of the constant table
    PushConst { index: usize },
    /// Push a boolean value to stack
    PushBool { bool: bool },
    /// Push a compiled lambda to the stack
    PushLambda { index: usize },
    /// Make a list (popping from stack), using the amount specified as the number of
    /// items
    MakeList { length: usize },
    // /// Pop a value (must be a nonnegative integer or 0)
    // /// then make a list (popping from stack) using the value as the number of items
    // MakeListIndirect,
    /// Make a vector (popping from stack), using the amount specified as the number of
    /// items
    MakeVector { length: usize },
    // /// Pop a value (must be a nonnegative integer or 0)
    // /// then make a vector (popping from stack) using the value as the number of items
    // MakeVectorIndirect,
    /// Look up the given symbol in the stack environment, and push the result to
    /// stack (if not found, errors)
    Reference {
        symbol: lasso::Spur,
        import_env: Option<usize>,
    },
    // /// Pop the top value (must be a symbol)
    // /// Look up the value in the stack environment, and push the result to
    // /// stack (if not found, errors)
    // ReferenceIndirect { import_env: Option<usize> },
    /// Invoke the given macro from the macro environment
    /// using `args` values from the stack where the topmost is the first value
    Syntax {
        symbol: lasso::Spur,
        args: usize,
        import_env: Option<usize>,
    },
    // /// Pop the top value (must be a symbol)
    // /// Look up the value in the macro environment, then evaluate the macro
    // /// using `args` values from the stack where the topmost is the first value
    // SyntaxIndirect {
    //     args: usize,
    //     import_env: Option<usize>,
    // },
    /// Pop the top value (must be a callable)
    /// Call the given lambda, making it a tail call if possible (there are
    /// no more instructions in the current context to execute)
    Call,

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
    /// Jump forward by a certain number of instructions
    /// if the value popped from the top of the stack is falsey
    /// (which is only #f and '() \[null])
    If { jump: usize },

    /// Duplicate the reference to the value at the top of the stack
    Duplicate,
    /// Pop the value at the top of the stack
    Pop,
    /// Explicitly end an execution frame
    Return,
}

impl Bytecode {
    /// Calculate the VM fuel cost of an instruction
    pub fn cost(&self) -> i32 {
        match self {
            Self::PushConst { .. } => 1,
            Self::PushBool { .. } => 1,
            Self::PushLambda { .. } => 1,
            Self::MakeList { .. } => 1,
            // Self::MakeListIndirect => 1,
            Self::MakeVector { .. } => 1,
            // Self::MakeVectorIndirect => 1,
            Self::Reference { .. } => 1,
            // Self::ReferenceIndirect { .. } => 1,
            Self::Syntax { .. } => 2,
            // Self::SyntaxIndirect { .. } => 2,
            Self::Call => 4,
            Self::Define { .. } => 2,
            Self::SetBang { .. } => 2,
            Self::If { .. } => 2,
            Self::Duplicate => 1,
            Self::Pop => 1,
            Self::Return => 4,
        }
    }
}

/// Constant values, that can be pushed to stack as-is
///
/// Mostly used to store "primitive" values, and correspond to external
/// representation types (except for list)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Symbol(lasso::Spur),
    Char(char),
    Number(i64),
    String(String),
    Bytevector(Vec<u8>),
}

#[derive(Debug, Clone, Copy)]
pub enum SourceData {
    /// a value directly written in source
    Direct {
        source_id: lasso::Spur,
        range: (usize, usize),
    },
    /// a value generated from source at a location
    /// (whether lambda or macro)
    Computed {
        source_id: lasso::Spur,
        range: (usize, usize),
    },
}

impl SourceData {
    #[inline]
    pub fn source_id(self) -> lasso::Spur {
        match self {
            Self::Direct { source_id, .. } => source_id,
            Self::Computed { source_id, .. } => source_id,
        }
    }

    #[inline]
    pub fn range(self) -> (usize, usize) {
        match self {
            Self::Direct { range, .. } => range,
            Self::Computed { range, .. } => range,
        }
    }

    /// Create a computed copy of this SourceData
    fn derive(self) -> Self {
        match self {
            Self::Direct { source_id, range } => Self::Computed { source_id, range },
            computed => computed,
        }
    }
}

/// A chunk of bytecode
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub struct Chunk<'gc> {
    /// symbols this chunk references
    #[collect(require_static)]
    pub(crate) constants: Box<[Constant]>,
    /// environment this chunk references
    // We only need 1 because of the fact that a Scheme program is all the imports *then*
    // commands and definitions
    pub(crate) import_stack_env: StackEnvironmentPtr<'gc>,
    // TODO import macro env
    // TODO compiled lambdas
    #[collect(require_static)]
    pub(crate) code: Box<[Bytecode]>,
    /// Hash map of code locations to SourceData
    #[collect(require_static)]
    labels: FxHashMap<usize, SourceData>,
    /// Does this chunk extend into another chunk? (used if referencing more than C constants)
    #[collect(require_static)]
    pub(crate) next_chunk: Option<ChunkPtr<'gc>>,
}
pub type ChunkPtr<'gc> = Gc<'gc, Chunk<'gc>>;

impl<'gc> Chunk<'gc> {
    fn new(
        code: impl IntoIterator<Item = Bytecode>,
        constants: impl IntoIterator<Item = Constant>,
        import_stack_env: StackEnvironmentPtr<'gc>,
        // TODO macro_env
        // TODO compiled lambdas
        labels: FxHashMap<usize, SourceData>,
    ) -> Self {
        Self {
            code: code.into_iter().collect(),
            constants: constants.into_iter().collect(),
            import_stack_env,
            labels,
            next_chunk: None,
        }
    }

    /// Find the corresponding [`SourceData`] for a given index into bytecode
    fn find_label(&self, pc: usize) -> Option<SourceData> {
        self.labels
            .keys()
            .filter(|k| **k <= pc)
            .max()
            .and_then(|idx| self.labels.get(idx).copied())
    }
}
