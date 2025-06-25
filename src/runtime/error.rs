use core::fmt;
use std::rc::Rc;

use gc_arena::{Collect, Gc};

use crate::{
    ValuePtr, ValueType,
    bytecode::Bytecode,
    interpreter::thread::{Execution, LambdaException},
    value::ResolvedValue,
};

use super::value::{ContinuationPtr, ModeDisplay};

/// Errors store this to record where they're from
#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct StackFrame<'gc> {
    // TODO Add a label / some way of identifying the name of the procedure under evaluation
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
    /// Tried to use ReturnHandler when function is not in error mode
    #[error("native function returned from non-existent handler")]
    InvalidReturnHandler,
    /// A value was raised by 'raise
    #[error("{0}")]
    Raise(ResolvedValue<'gc, lasso::RodeoResolver, ModeDisplay>),
    /// A value was raised by 'raise-continuable
    #[error("{0}")]
    RaiseContinuable(
        ResolvedValue<'gc, lasso::RodeoResolver, ModeDisplay>,
        ContinuationPtr<'gc>,
    ),
    /// Handler returned on a non-continuable error
    #[error("error handler failed: {0}")]
    HandlerFailed(Gc<'gc, SchemeErrorType<'gc>>),
    /// Rust code produced an error
    #[error("{0}")]
    Rust(#[collect(require_static)] Rc<anyhow::Error>),
    /// Rust code produced an error (continuable)
    #[error("{0}")]
    RustContinuable(
        #[collect(require_static)] Rc<anyhow::Error>,
        ContinuationPtr<'gc>,
    ),
    /// Attempted to read from environment a name that doesn't exist
    #[error("`{0}` does not exist in environment")]
    EnvLoad(Box<str>),
    /// Bytecode references an argument it didn't have
    #[error("referenced non-existant parameter {0}")]
    InvalidArg(usize),
    /// Bytecode uses rest parameter where no rest argument can exist
    #[error("referenced nonexistent rest parameter")]
    InvalidRest,
    #[error("attempted to redefine an imported symbol: {0}")]
    ImportedSymbol(Box<str>),
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
    /// Lambda exception during a parameter call (runtime errors)
    #[error("parameter exception: {0}")]
    ParameterException(#[collect(require_static)] LambdaException),
    /// Called a parameter with more than 0 arguments
    #[error("parameter calls do not accept arguments")]
    Parameter,
    #[error("instruction expected more values: {0:?}")]
    NoValue(#[collect(require_static)] Bytecode),
    #[error("{inst} expected a {expected:?}, but found a {kind:?}")]
    WrongValue {
        inst: &'static str,
        expected: ValueType,
        kind: ValueType,
    },
    #[error("{0} expected a list, got a non-list cons")]
    ExpectedList(&'static str),
    #[error("too much recursion, try using lazy evaluation")]
    TooMuchRecursion,
}

impl<'gc> SchemeErrorType<'gc> {
    /// Can Scheme catch and process the error?
    pub fn is_continuable(&self) -> bool {
        matches!(
            self,
            Self::RaiseContinuable(_, _) | Self::RustContinuable(_, _)
        )
    }

    /// Argument value
    pub fn value(&self) -> Option<ValuePtr<'gc>> {
        match self {
            Self::Raise(v) => Some(v.value_ptr()),
            Self::RaiseContinuable(v, _) => Some(v.value_ptr()),
            Self::HandlerFailed(h) => h.value(),
            _ => None,
        }
    }

    /// Continuation
    pub fn continuation(&self) -> Option<ContinuationPtr<'gc>> {
        match self {
            Self::RaiseContinuable(_, cont) => Some(*cont),
            Self::RustContinuable(_, cont) => Some(*cont),
            _ => None,
        }
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
pub type SourcesMap = fxhash::FxHashMap<lasso::Spur, Box<str>>;

impl<'gc> SchemeError<'gc> {
    pub fn display<'s, R: lasso::Resolver>(
        &'s self,
        resolver: &'s R,
        sources: &'s SourcesMap,
    ) -> DisplaySchemeError<'s, 'gc, R> {
        DisplaySchemeError {
            resolver,
            error: self,
            sources,
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
    sources: &'s SourcesMap,
}

#[cfg(target_arch = "wasm32")]
fn style_text(_idx: usize, t: String) -> String {
    t
}

#[cfg(not(target_arch = "wasm32"))]
fn style_text(idx: usize, s: String) -> String {
    use yansi::Paint;
    if idx == 0 {
        s.red().to_string()
    } else {
        s.blue().to_string()
    }
}

impl<'gc, R: lasso::Resolver> fmt::Display for DisplaySchemeError<'_, 'gc, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error: {}", self.error.error_type)?;

        if !self.error.backtrace.is_empty() {
            // write the backtrace
            write!(f, "\n\nBacktrace:")?;
            // Now we go through the frames and build up codesnake blocks to display.
            // codesnake *cannot* have overlapping spans so we go backwards through frames and do:
            // - within the same sourcefile: check if a span overlaps, if it does, and the new frame is larger, use the
            //   larger frame as the error span, if no overlap, create a secondary error

            // For now we will use the "dumb" method of each frame getting it's own block
            let mut indices =
                fxhash::FxHashMap::<lasso::Spur, Option<codesnake::LineIndex>>::default();
            for (idx, frame) in self.error.backtrace.iter().enumerate() {
                if let Some((source_id, range)) = frame.source_filename.zip(frame.range) {
                    let line_index = indices.entry(source_id).or_insert_with(|| {
                        self.sources
                            .get(&source_id)
                            .map(|src| codesnake::LineIndex::new(src))
                    });

                    if let Some(li) = line_index.as_ref() {
                        let block = codesnake::Block::new(
                            li,
                            [codesnake::Label::new(range.0..range.1)
                                .with_text(format!("frame {idx}"))
                                .with_style(move |s| style_text(idx, s))],
                        )
                        .expect("code ref out-of-range")
                        .map_code(|c| codesnake::CodeWidth::new(c, c.len()));
                        writeln!(
                            f,
                            "\n{}<<{}>>",
                            block.prologue(),
                            self.resolver.resolve(&source_id)
                        )?;
                        write!(f, "{block}")?;
                        write!(f, "{}", block.epilogue())?;
                    } else {
                        write!(
                            f,
                            "\n - <<{}:[{}:{}]>> {}",
                            self.resolver.resolve(&source_id),
                            range.0,
                            range.1,
                            match frame.execution {
                                Execution::Bytecode { chunk, pc, .. } => format!(
                                    "<<code{}>>",
                                    if f.alternate() {
                                        String::new()
                                    } else {
                                        format!(
                                            " 0x{:x}@({pc})",
                                            (&raw const *chunk.as_ref()).addr()
                                        )
                                    }
                                ),
                                Execution::Native { native, .. } => format!(
                                    "<<native{}>>",
                                    if f.alternate() {
                                        String::new()
                                    } else {
                                        format!(" 0x{:x}", (&raw const *native.borrow()).addr())
                                    }
                                ),
                            }
                        )?;
                    }
                } else {
                    write!(
                        f,
                        "\n - <<synthesized>> {}",
                        match frame.execution {
                            Execution::Bytecode { chunk, pc, .. } => format!(
                                "<<code{}>>",
                                if f.alternate() {
                                    String::new()
                                } else {
                                    format!(" 0x{:x}@({pc})", (&raw const *chunk.as_ref()).addr())
                                }
                            ),
                            Execution::Native { native, .. } => format!(
                                "<<native{} [{}]>>",
                                if f.alternate() {
                                    String::new()
                                } else {
                                    format!(" 0x{:x}", (&raw const *native.borrow()).addr())
                                },
                                native.borrow().name(),
                            ),
                        }
                    )?;
                }
            }
        }
        Ok(())
    }
}
