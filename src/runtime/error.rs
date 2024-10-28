use core::fmt;

use gc_arena::{Collect, Gc};
use rowan::TextRange;

use crate::{environment::RebindError, value::ResolvedValue};

/// Errors store this to record where they're from
#[derive(Debug, Clone)]
pub struct StackFrame {
    // what range of text were we processing (or None if external)
    pub range: Option<TextRange>,
    // what was the label of the scope we were in?
    pub scope_label: Option<Box<str>>,
    // What source id is this scope from?
    pub source_id: Option<usize>,
}

/// Scheme is allowed to check what kind of error
/// This allows us to do that.
#[derive(Collect, Debug, thiserror::Error)]
#[collect(no_drop)]
pub enum SchemeErrorType<'gc> {
    /// A value was raised
    #[error("a value was raised: {0}")]
    Raise(ResolvedValue<'gc, lasso::Spur>),
    /// Rust code produced an error
    #[error("Rust code produced an error: {0}")]
    Rust(#[collect(require_static)] anyhow::Error),
    /// Rust typecheck produced an error
    #[error("Rust typecheck produced an error: {0}")]
    Typecheck(#[collect(require_static)] anyhow::Error),
    /// Rust macro produced an error
    #[error("Rust macro produced an error: {0}")]
    Macro(#[collect(require_static)] anyhow::Error),
    /// Macro was in wrong form
    #[error("bad macro form: {0}")]
    MacroForm(#[collect(require_static)] anyhow::Error),
    /// Multiple errors have occured
    #[error("Multiple errors have occured")]
    Compound(Vec<SchemeErrorPtr<'gc>>),
    /// Attempted to execute an empty list
    #[error("attempted to execute an empty list")]
    Null,
    /// Attempted to read from environment a name that doesn't exist
    #[error("`{0}` does not exist in environment")]
    EnvLoad(Box<str>),
    /// Attempted to execute a list with a dot, or non-symbol, non-list head
    #[error("cannot execute list")]
    BadList,
    /// Define did not find a value to define
    #[error("define cannot define nothing")]
    NullDefine,
    /// Define attempted on frozen environment
    #[error("cannot define in frozen environment")]
    FrozenDefine,
    /// An error occured with `set!`
    #[error("set! error: {0}")]
    SetBang(#[collect(require_static)] RebindError),
    /// Attempted to evaluate a value with no external representation
    #[error("data has no external representation: {0}")]
    BadEval(ResolvedValue<'gc, lasso::Spur>),
    /// Lambda didn't return a value
    #[error("lambda has no return value")]
    LambdaNoReturn,
    /// Tried to execute a non-lambda
    #[error("{0} is not a lambda")]
    NonLambda(ResolvedValue<'gc, lasso::Spur>),
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
    pub fn display<'s>(&'s self, sources: &'s [&'s str]) -> DisplaySchemeError<'s, 'gc> {
        DisplaySchemeError {
            filename: None,
            sources,
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
    filename: Option<Box<str>>,
    sources: &'s [&'s str],
    error: &'s SchemeError<'gc>,
}

struct SourceDisplay<'a>(&'a str);
impl<'a> fmt::Display for SourceDisplay<'a> {
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

impl<'s, 'gc> fmt::Display for DisplaySchemeError<'s, 'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_count = match &self.error.error_type {
            SchemeErrorType::Compound(c) => c.len(),
            _ => 1,
        };

        write!(
            f,
            "{} error{} in {}:",
            error_count,
            if error_count != 1 { "s" } else { "" },
            self.filename
                .as_ref()
                .map(|b| b.as_ref())
                .unwrap_or("<unnamed.scm>")
        )?;

        let mut display_fn = |err: &SchemeError<'gc>| -> fmt::Result {
            write!(f, " {}", err.error_type)?;
            match &err.error_type {
                SchemeErrorType::Compound(errs) => {
                    for err in errs {
                        write!(f, "\n- {}", err.error_type)?;
                    }
                    Ok(())
                }
                _ => Ok(()),
            }
        };

        display_fn(self.error)?;

        // write the backtrace
        write!(f, "\n\nBacktrace:")?;
        for frame in self.error.backtrace.iter().rev() {
            if let Some((range, source_id)) = frame.range.zip(frame.source_id) {
                write!(
                    f,
                    "\n - {:?} \"{}\" {}",
                    range,
                    SourceDisplay(
                        &self.sources[source_id][range.start().into()..range.end().into()]
                    ),
                    frame
                        .scope_label
                        .as_ref()
                        .map(|b| b.as_ref())
                        .unwrap_or("<<root>>")
                )?;
            } else {
                write!(
                    f,
                    "\n - <<external>> {}",
                    frame
                        .scope_label
                        .as_ref()
                        .map(|b| b.as_ref())
                        .unwrap_or("<<root>>")
                )?;
            }
        }
        Ok(())
    }
}
