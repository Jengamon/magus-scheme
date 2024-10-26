//! If we want to go even fasterer, a Treewalk can JIT code into this
//! representation.

use gc_arena::Collect;

use crate::value::ValuePtr;

#[derive(Debug, Clone, Copy)]
pub enum Bytecode {}

/// A chunk of bytecode, with necessary constants
/// and references pre-evaluated
#[derive(Debug, Collect)]
#[collect(no_drop)]
#[expect(dead_code)]
pub struct Chunk<'gc> {
    /// values this chunk references
    values: [ValuePtr<'gc>; 256],
    /// symbols this chunk references
    #[collect(require_static)]
    symbols: [Option<lasso::Spur>; 256],
    #[collect(require_static)]
    pub code: Box<[Bytecode]>,
}

// TODO figure out what is actually needed to support code like this
// NOTE Making chunks cannot be touched by users so that we can support arbitrary
// restrictions. We want to be able to assume that anything in the values array
// has been passed through the Treewalk's EnsureNull, so that any null cons *is* null (eq? '())
//
// Macros support this process where the *result* of the macro
// can be JIT'ed, which means that macros have to be pure: the same input data results in the
// same output data. Impure macros can be marked so, which will tell the Treewalk to never JIT them.
