use core::fmt;

use gc_arena::{Collect, Gc};
use rowan::TextRange;

use crate::{environment::RebindError, value::ResolvedValue};

/// Errors store this to record where they're from
#[derive(Debug, Clone)]
pub struct StackFrame {
    // what range of text were we processing (or None if external)
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
    /// Rust typecheck produced an error
    Typecheck(#[collect(require_static)] anyhow::Error),
    /// Rust macro produced an error
    Macro(#[collect(require_static)] anyhow::Error),
    /// Multiple errors have occured
    Compound(Vec<SchemeErrorPtr<'gc>>),
    /// A read error has occured
    Read,
    /// Attempted to execute an empty list
    Null,
    /// Attempted to read from environment a name that doesn't exist
    EnvLoad(Box<str>),
    /// Attempted to execute a non-executable list (a list with a dot, or with no external representation)
    BadList,
    /// Define did not find a value to define
    NullDefine,
    /// Define attempted on frozen environment
    FrozenDefine,
    /// An error occured with `set!`
    SetBang(#[collect(require_static)] RebindError),
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
    pub fn display<'s>(&'s self, source: &'s str) -> DisplaySchemeError<'s, 'gc> {
        DisplaySchemeError {
            filename: None,
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
    filename: Option<Box<str>>,
    source: &'s str,
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

        let mut display_fn = |err: &SchemeError<'gc>| match &err.error_type {
            SchemeErrorType::EnvLoad(name) => write!(f, " failed to find {name} in environment"),
            err => write!(f, " {err:?}"),
        };

        display_fn(self.error)?;

        // write the backtrace
        write!(f, "\n\nBacktrace:")?;
        for frame in &self.error.backtrace {
            let range = frame.range;
            write!(
                f,
                "\n - {:?} \"{}\" {}",
                range,
                SourceDisplay(&self.source[range.start().into()..range.end().into()]),
                frame
                    .scope_label
                    .as_ref()
                    .map(|b| b.as_ref())
                    .unwrap_or("<<root>>")
            )?;
        }
        Ok(())
    }
}
