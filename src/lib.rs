// NOTE this is just to make the usage of unsafe code a *little* more annoying / granular
// Gotta sign it in duplicate.
#![deny(unsafe_code)]

pub mod bytecode;
pub mod compiler;
pub mod environment;
pub mod general_parser;
pub mod interpreter;
pub mod lexer;
mod num;
pub mod runtime;
pub mod stdlib;

pub use anyhow;
pub use fxhash;
pub use lasso::Rodeo;
pub use rowan;

pub use general_parser::{CompoundTermKind, TokenKind, gast::*, general_parse};
pub use num::{ExactReal, SchemeNumber};
pub use rowan::TextRange;
pub use runtime::{
    any::Any,
    fuel::Fuel,
    value::{self, Value, ValuePtr, ValueType},
};
