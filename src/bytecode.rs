//! Execution model for Scheme code

use core::fmt;
use gc_arena::{Collect, Gc};
use rowan::TextRange;

/*
compiled form is at its root primitive forms:
<literal>
<variable ref>
<procedure (function/macro) call>
define
lambda
if
set!
(when/if supported) include, include-ci

then provide Rust-side impls for the rest of the standard library (and/or mix it with
Scheme-impls)
*/
#[derive(Debug, Clone, Copy)]
pub enum Bytecode {
    /// Push to stack a constant value at a given index of the constant table
    PushConst { index: usize },
    /// Make a list (popping from stack), using the amount specified as the number of
    /// items
    MakeList { length: usize },
    /// Pop a value (must be a nonnegative integer or 0)
    /// then make a list (popping from stack) using the value as the number of items
    MakeListIndirect,
    /// Pop the top value (must be a symbol)
    /// Look up the value in the stack environment, and push the result to
    /// stack (if not found, errors)
    Reference,
    /// Pop the top value (must be a symbol)
    /// Look up the value in the macro environment, then evaluate the macro
    /// using `args` values from the stack where the topmost is the first value
    Syntax { args: usize },
    /// Pop the top value (must be a callable)
    /// Call the given lambda, making it a tail call if possible (there are
    /// no more instructions in the current context to execute)
    Call,
}

impl Bytecode {
    /// Calculate the VM fuel cost of an instruction
    pub fn cost(&self) -> i32 {
        match self {
            Self::PushConst { .. } => 1,
            Self::MakeList { .. } => 1,
            Self::MakeListIndirect => 1,
            Self::Reference => 1,
            Self::Syntax { .. } => 2,
            Self::Call => 4,
        }
    }
}

/// Constant values, that can be pushed to stack as-is
///
/// Mostly used to store "primitive" values, and correspond to external
/// representation types (except for list)
#[derive(Debug, Clone)]
pub enum Constant {
    Symbol(lasso::Spur),
    Integer(isize),
    Unsigned(usize),
    String(String),
    Bytevector(Vec<u8>),
}

#[derive(Debug, Clone, Copy)]
pub enum SourceData {
    /// a value directly written in source
    Direct {
        source_id: lasso::Spur,
        range: TextRange,
    },
    /// a value generated from source at a location
    /// (whether lambda or macro)
    Computed {
        source_id: lasso::Spur,
        range: TextRange,
    },
}

impl SourceData {
    #[inline]
    pub fn source_id(&self) -> lasso::Spur {
        match self {
            Self::Direct { source_id, .. } => *source_id,
            Self::Computed { source_id, .. } => *source_id,
        }
    }

    #[inline]
    pub fn range(&self) -> TextRange {
        match self {
            Self::Direct { range, .. } => *range,
            Self::Computed { range, .. } => *range,
        }
    }
}

/// A chunk of bytecode, with necessary constants
/// and references pre-evaluated
#[derive(Collect)]
#[collect(no_drop)]
// V and S are arbitrary limits that we've "hardcoded" in.
// If the resulting value table would require more that V entries, or
// the resulting symbol table would require more than N entries, we fail to JIT
pub struct Chunk<const C: usize> {
    /// symbols this chunk references
    #[collect(require_static)]
    pub(crate) constants: [Constant; C],
    /// Number of symbols used by this chunk
    #[collect(require_static)]
    pub(crate) constants_allocated: usize,
    #[collect(require_static)]
    pub(crate) code: Box<[Bytecode]>,
    #[collect(require_static)]
    pub(crate) source_data: SourceData,
    /// Does this chunk extend into another chunk? (used if referencing more than C constants)
    #[collect(require_static)]
    pub(crate) next_chunk: Option<Box<Chunk<C>>>,
}
pub type ChunkPtr<'gc, const C: usize> = Gc<'gc, Chunk<C>>;

fn iter_to_fixed<T, const C: usize>(items: impl IntoIterator<Item = T>) -> Option<([T; C], usize)> {
    let desired = items.into_iter().collect::<Vec<_>>();
    let allocated = desired.len();
    let fixed: [_; C] = desired.try_into().ok()?;

    Some((fixed, allocated))
}

impl<const C: usize> Chunk<C> {
    /// Returns None if too many constants or variables are to be allocated
    fn new(
        code: impl IntoIterator<Item = Bytecode>,
        constants: impl IntoIterator<Item = Constant>,
        source_data: SourceData,
    ) -> Option<Self> {
        let (constants, constants_allocated) = iter_to_fixed(constants)?;

        Some(Self {
            code: Box::from(code.into_iter().collect::<Vec<_>>()),
            constants,
            constants_allocated,
            source_data,
            next_chunk: None,
        })
    }
}

impl<const C: usize> fmt::Debug for Chunk<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // custom debug so that we can skip printing the unused values
        f.debug_struct("Chunk")
            .field("constants", &&self.constants[..self.constants_allocated])
            .field("next_chunk", &self.next_chunk)
            .field("code", &self.code)
            .finish()
    }
}

// TODO figure out what is actually needed to support code like this
