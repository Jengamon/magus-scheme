// NOTE this is just to make the usage of unsafe code a *little* more annoying / granular
// Gotta sign it in duplicate.
#![deny(unsafe_code)]

// NOTE We do quite a bit of optimizing based off the assumption that *GC data does not move*
// ...that is true for the forseeable future, but that guarantee must hold true (or at least, it
// has to visibly be true for us)
// (If this changes, we would need some kind of gc tag that is the same for an arbitrary Gc allocation
// for its lifetime. rn its the address but it could be something else in the future)

pub mod bytecode;
pub mod compiler;
pub mod environment;
pub mod general_parser;
pub mod interpreter;
pub mod lexer;
mod num;
pub mod runtime;
pub mod stdlib;

// TODO when stablizing, remove these re-exports, so that our public API
// doesn't rely on the public API of all our dependencies
pub use anyhow;
pub use fxhash;
pub use gc_arena;
pub use lasso;
pub use rowan;

pub use compiler::{
    ExternalCompilerContext, LibraryDefinitionContext, LibraryName, LibraryNameItem, ParseProgram,
    Syntax, SyntaxContext, SyntaxReturn, World,
};
pub use general_parser::{CompoundTermKind, TokenKind, gast::*, general_parse};
pub use interpreter::{
    ChunkHandle, CompilerHandle, Includer, Interpreter, NullIncluder, Registerable, ThreadHandle,
    ValueHandle, ValuePointers, thread::ThreadPtr,
};
pub use num::{ExactReal, SchemeNumber};
pub use rowan::TextRange;
pub use runtime::{
    any::Any,
    fuel::Fuel,
    value::{self, Value, ValuePtr, ValueType},
};

macro_rules! handle_type {
    ($v:vis $hn:ident => $k:ty) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $v struct $hn {
            _knob: Arc<()>,
            key: $k,
        }
    };
}
pub(crate) use handle_type;
