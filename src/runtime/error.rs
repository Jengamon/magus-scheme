use core::fmt;
use std::rc::Rc;

use gc_arena::{Collect, Gc};

use crate::{
    bytecode::Bytecode,
    interpreter::thread::{Execution, LambdaException},
    value::ResolvedValue,
};

/// Errors store this to record where they're from
#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct StackFrame<'gc> {
    // what range of text were we processing (or None if external)
    pub range: Option<(usize, usize)>,
    // what was executing on this frame?
    pub execution: Execution<'gc>,
    // What source file is this scope from?
    #[collect(require_static)]
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
    /// Attempted to define a non-existent name
    #[error("set! could not find name `{0}` in environment")]
    NoName(Box<str>),
    /// Lambda exceptions (runtime errors)
    #[error("lambda exception: {0}")]
    LambdaException(
        #[from]
        #[collect(require_static)]
        LambdaException,
    ),
    #[error("hole {0} was already defined")]
    AlreadyDefinedHole(usize),
    #[error("hole {0} was not defined")]
    UndefinedHole(usize),
    #[error("instruction expected more values: {0:?}")]
    NoValue(#[collect(require_static)] Bytecode),
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
    pub backtrace: Vec<StackFrame<'gc>>,
    pub error_type: SchemeErrorType<'gc>,
}
pub type SchemeErrorPtr<'gc> = Gc<'gc, SchemeError<'gc>>;

impl<'gc> SchemeError<'gc> {
    pub fn display<'s, R: lasso::Resolver>(
        &'s self,
        resolver: &'s R,
        sources: impl IntoIterator<Item = (lasso::Spur, &'s str)>,
    ) -> DisplaySchemeError<'s, 'gc, R> {
        DisplaySchemeError {
            resolver,
            error: self,
            sources: sources.into_iter().collect(),
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
    sources: fxhash::FxHashMap<lasso::Spur, &'s str>,
}

impl<'gc, R: lasso::Resolver> fmt::Display for DisplaySchemeError<'_, 'gc, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error:")?;

        let mut display_fn =
            |err: &SchemeError<'gc>| -> fmt::Result { write!(f, " {}", err.error_type) };

        display_fn(self.error)?;

        // write the backtrace
        write!(f, "\n\nBacktrace:")?;
        for frame in self.error.backtrace.iter() {
            let file_name = |source_id: Option<lasso::Spur>| {
                if let Some(sid) = source_id {
                    self.resolver.try_resolve(&sid).unwrap_or("<unnamed>")
                } else {
                    "<<external>>"
                }
            };

            let source = |source_id: Option<lasso::Spur>| {
                source_id.and_then(|sid| self.sources.get(&sid).copied())
            };
            if let Some(range) = frame.range {
                write!(
                    f,
                    "\n - {:?} {} {}{}",
                    range,
                    file_name(frame.source_filename),
                    match frame.execution {
                        Execution::Bytecode { chunk, pc, .. } =>
                            format!("<<code {chunk:p}@({pc})>>"),
                        Execution::Native { native, .. } => format!("<<native {native:p}>>"),
                    },
                    (source)(frame.source_filename)
                        .and_then(|s| {
                            if !((0..s.len()).contains(&range.0) && (0..s.len()).contains(&range.1))
                            {
                                None
                            } else {
                                Some(s)
                            }
                        })
                        .map(|s| format!(": {}", &s[range.0..range.1]))
                        .unwrap_or(String::new()),
                )?;
            } else {
                write!(
                    f,
                    "\n - <<synthesized>> {}",
                    match frame.execution {
                        Execution::Bytecode { chunk, pc, .. } =>
                            format!("<<code {chunk:p}@({pc})>>"),
                        Execution::Native { native, .. } => format!("<<native {native:p}>>"),
                    }
                )?;
            }
        }
        Ok(())
    }
}
