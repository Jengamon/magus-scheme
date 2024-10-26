//! If we want to go even fasterer, a Treewalk can JIT code into this
//! representation.

use gc_arena::{Collect, Gc};

use crate::value::ValuePtr;

#[derive(Debug, Clone, Copy)]
pub enum Bytecode {}

/// A chunk of bytecode, with necessary constants
/// and references pre-evaluated
#[derive(Debug, Collect)]
#[collect(no_drop)]
#[expect(dead_code)]
// V and S are arbitrary limits that we've "hardcoded" in.
// If the resulting value table would require more that V entries, or
// the resulting symbol table would require more than N entries, we fail to JIT
pub struct Chunk<'gc, const V: usize, const S: usize> {
    /// values this chunk references
    values: [ValuePtr<'gc>; V],
    /// symbols this chunk references
    #[collect(require_static)]
    symbols: [Option<lasso::Spur>; S],
    #[collect(require_static)]
    pub code: Box<[Bytecode]>,
}
pub type ChunkPtr<'gc, const V: usize, const N: usize> = Gc<'gc, Chunk<'gc, V, N>>;

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
