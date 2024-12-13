//! If we want to go even fasterer, a Treewalk can JIT code into this
//! representation.

use core::fmt;
use gc_arena::{Collect, Gc};

use crate::{environment::StackEnvironmentPtr, value::StackValue};

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
    // Bytecode is represented in CPS, so there is only 1 CALL command, but
    // it takes both a number of arguments off the stack *and* a register
    // (where the register is the continuation)
    Call {
        /// This is the register referring to the function to be called
        function: u8,
        /// number of arguments to pass into this
        arg_count: usize,
        /// Register 0 is always the continuation of the current chunk, so
        /// when this is 0, this is always a tail-call
        continuation: u8,
    },
    // This instructs the interpreter to execute a macro in the current environment
    // with the given symbol
    Syntax {
        /// index into symbols array
        symbol: usize,
        /// number of elements pushed to stack for eval
        arg_count: usize,
    },
}

/// A chunk of bytecode, with necessary constants
/// and references pre-evaluated
#[derive(Collect)]
#[collect(no_drop)]
// V and S are arbitrary limits that we've "hardcoded" in.
// If the resulting value table would require more that V entries, or
// the resulting symbol table would require more than N entries, we fail to JIT
pub struct Chunk<'gc, const V: usize, const S: usize, const E: usize> {
    /// registers used by this chunk
    ///
    /// The code is register-based, with up to 255 registers
    /// Register 0 is always allowed represents the continuation of
    /// the lambda chunk.
    ///
    /// The stack is filled with undefined values initially.
    registers_allocated: u8,
    /// values this chunk references
    values: [StackValue<'gc>; V],
    /// Number of values used by this chunk
    values_allocated: usize,
    /// symbols this chunk references
    #[collect(require_static)]
    symbols: [lasso::Spur; S],
    /// Number of symbols used by this chunk
    symbols_allocated: usize,
    /// environments this chunk references
    envs: [StackEnvironmentPtr<'gc>; E],
    /// Number of environments used by this chunk
    envs_allocated: usize,
    #[collect(require_static)]
    pub code: Box<[Bytecode]>,
}
pub type ChunkPtr<'gc, const V: usize, const N: usize, const E: usize> =
    Gc<'gc, Chunk<'gc, V, N, E>>;

impl<const V: usize, const N: usize, const E: usize> fmt::Debug for Chunk<'_, V, N, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // custom debug so that we can skip printing the unused values
        f.debug_struct("Chunk")
            .field("registers_allocated", &self.registers_allocated)
            .field("values", &&self.values[..self.values_allocated])
            .field("symbols", &&self.symbols[..self.symbols_allocated])
            .field("envs", &&self.symbols[..self.envs_allocated])
            .finish()
    }
}

// TODO figure out what is actually needed to support code like this
// NOTE Making chunks cannot be touched by users so that we can support arbitrary
// restrictions. We want to be able to assume that anything in the values array
// has been passed through the Treewalk's EnsureNull, so that any null cons *is* null (eq? '())
//
// Macros support this process where the *result* of the macro
// can be JIT'ed, which means that macros have to be pure: the same input data results in the
// same output data. Impure macros can be marked so, which will tell the Treewalk to never JIT them.
//
// This means that we can design the bytecode to assume that macros will never be called/needed, b/c for any macro that
// *would* be needed, we've JIT'ed the output, which is *equivalent* to executing the macro with the given input (which
// is the assertion a macro makes when it declares itself pure.)
// Generally any macro that mutates itself is impure, because self-mutation makes it easy to break the *same input, same output*
// pattern required for JIT
