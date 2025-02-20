use core::fmt;
use std::rc::Rc;

use gc_arena::{Collect, Gc};

use crate::{interpreter::thread::LambdaException, value::ResolvedValue};

/// Errors store this to record where they're from
#[derive(Debug, Clone, Copy)]
pub struct StackFrame {
    // what range of text were we processing (or None if external)
    pub range: Option<(usize, usize)>,
    // what was the label of the scope we were in?
    pub scope_label: Option<lasso::Spur>,
    // What source file is this scope from?
    pub source_filename: Option<lasso::Spur>,
}

/// Scheme is allowed to check what kind of error
/// This allows us to do that.
#[derive(Collect, Debug, thiserror::Error, Clone)]
#[collect(no_drop)]
pub enum SchemeErrorType<'gc> {
    /// A value was raised by 'raise
    #[error("a value was raised: {0}")]
    Raise(ResolvedValue<'gc, lasso::RodeoResolver>),
    /// A value was raised by 'raise-continuable
    #[error("a value was raised: {0}")]
    RaiseContinuable(ResolvedValue<'gc, lasso::RodeoResolver>),
    /// Handler returned on a non-continuable error
    #[error("error handler failed: {0}")]
    HandlerFailed(Gc<'gc, SchemeErrorType<'gc>>),
    /// Rust code produced an error
    #[error("Rust code produced an error: {0}")]
    Rust(#[collect(require_static)] Rc<anyhow::Error>),
    /// Rust code produced an error (continuable)
    #[error("Rust code produced an error: {0}")]
    RustContinuable(#[collect(require_static)] Rc<anyhow::Error>),
    /// Attempted to read from environment a name that doesn't exist
    #[error("`{0}` does not exist in environment")]
    EnvLoad(Box<str>),
    /// Bytecode references an argument it didn't have
    #[error("referenced non-existant parameter {0}")]
    InvalidArg(usize),
    /// Bytecode uses rest parameter where no rest argument can exist
    #[error("referenced nonexistent rest parameter")]
    InvalidRest,
    /// Attempted to call a non-callable
    #[error("attempt to call non-callable")]
    NonCallable,
    /// Attempted to define in a frozen environment
    #[error("cannot define in a frozen environment")]
    FrozenDefine,
    /// Lambda exceptions (runtime errors)
    #[error("lambda exception: {0}")]
    LambdaException(
        #[from]
        #[collect(require_static)]
        LambdaException,
    ),
}

impl SchemeErrorType<'_> {
    /// Can Scheme catch and process the error?
    pub fn is_continuable(&self) -> bool {
        matches!(self, Self::RaiseContinuable(_) | Self::RustContinuable(_))
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
    pub fn display<'s, R: lasso::Resolver>(
        &'s self,
        resolver: &'s R,
    ) -> DisplaySchemeError<'s, 'gc, R> {
        DisplaySchemeError {
            resolver,
            error: self,
        }
    }
}

impl fmt::Debug for SchemeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchemeError")
            .field("backtrace", &self.backtrace)
            .field("error_type", &self.error_type)
            .finish()
    }
}

pub struct DisplaySchemeError<'s, 'gc, R: lasso::Resolver> {
    resolver: &'s R,
    error: &'s SchemeError<'gc>,
}

struct SourceDisplay<'a>(&'a str);
impl fmt::Display for SourceDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut prev_is_space = false;
        for c in self.0.chars() {
            let emit = match c {
                '\t' => None,
                c if c.is_whitespace() => Some(' '),

                c => Some(c),
            };

            match emit {
                Some(' ') if prev_is_space => {}
                Some(' ') => {
                    prev_is_space = true;
                    write!(f, " ")?;
                }
                Some(c) => {
                    prev_is_space = false;
                    write!(f, "{c}")?;
                }
                None => {
                    prev_is_space = false;
                }
            }
        }

        Ok(())
    }
}

impl<'gc, R: lasso::Resolver> fmt::Display for DisplaySchemeError<'_, 'gc, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error:")?;

        let mut display_fn =
            |err: &SchemeError<'gc>| -> fmt::Result { write!(f, " {}", err.error_type) };

        display_fn(self.error)?;

        // write the backtrace
        write!(f, "\n\nBacktrace:")?;
        for frame in self.error.backtrace.iter().rev() {
            let file_name = |source_id: Option<lasso::Spur>| {
                if let Some(sid) = source_id {
                    self.resolver.try_resolve(&sid).unwrap_or("<unnamed>")
                } else {
                    "<<external>>"
                }
            };

            let source = |source_id: Option<lasso::Spur>| {
                source_id.and_then(|sid| self.resolver.try_resolve(&sid))
            };
            if let Some(range) = frame.range {
                write!(
                    f,
                    "\n - {:?} {}[{}{:?}] {}",
                    range,
                    if let Some(source) = source(frame.source_filename) {
                        format!("\"{}\" ", SourceDisplay(&source[range.0..range.1]))
                    } else {
                        "".to_string()
                    },
                    file_name(frame.source_filename),
                    range,
                    frame
                        .scope_label
                        .as_ref()
                        .and_then(|sl| self.resolver.try_resolve(sl))
                        .unwrap_or("<<root>>")
                )?;
            } else {
                write!(
                    f,
                    "\n - <<external>> {}",
                    frame
                        .scope_label
                        .as_ref()
                        .and_then(|sl| self.resolver.try_resolve(sl))
                        .unwrap_or("<<root>>")
                )?;
            }
        }
        Ok(())
    }
}
