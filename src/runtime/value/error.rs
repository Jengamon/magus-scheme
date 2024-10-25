use core::fmt;

use gc_arena::{Collect, Gc};
use rowan::TextRange;

use super::{ResolvedValue, ValuePtr};

/// Errors store this to record where they're from
#[derive(Debug)]
pub struct StackFrame {
    // what range of text were we processing
    pub range: TextRange,
    // what was the label of the scope we were in?
    pub scope_label: Option<Box<str>>,
}

/// Scheme is allowed to check what kind of error
/// This allows us to do that.
#[derive(Collect, Debug)]
#[collect(no_drop)]
pub enum SchemeErrorType<'gc> {
    /// A value was raised
    Raise(ResolvedValue<'gc, lasso::Spur>),
    /// Rust code produced an error
    Rust(#[collect(require_static)] anyhow::Error),
    /// Multiple errors have occured
    Compound(Vec<SchemeErrorPtr<'gc>>),
    /// A read error has occured
    Read,
}

impl<'gc> fmt::Display for SchemeErrorType<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Raise(v) => write!(f, "a value was raised: {}", v),
            _ => todo!(),
        }
    }
}

/// Scheme-side error
#[derive(Collect)]
#[collect(no_drop)]
pub struct SchemeError<'gc> {
    #[collect(require_static)]
    pub backtrace: Vec<StackFrame>,
    pub error_type: SchemeErrorType<'gc>,
}
pub type SchemeErrorPtr<'gc> = Gc<'gc, SchemeError<'gc>>;

impl<'gc> SchemeError<'gc> {
    pub fn display<'s>(self, source: &'s str) -> DisplaySchemeError<'s, 'gc> {
        DisplaySchemeError {
            source,
            error: self,
        }
    }
}

impl<'gc> fmt::Debug for SchemeError<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchemeError")
            .field("backtrace", &self.backtrace)
            .field("error_type", &self.error_type)
            .finish()
    }
}

pub struct DisplaySchemeError<'s, 'gc> {
    source: &'s str,
    error: SchemeError<'gc>,
}

impl<'s, 'gc> DisplaySchemeError<'s, 'gc> {
    pub fn into_inner(self) -> SchemeError<'gc> {
        self.error
    }
}

impl<'s, 'gc> fmt::Display for DisplaySchemeError<'s, 'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
