//! Representation of Scheme values
use core::fmt;

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    runtime::external::ExternalRepresentationVisitor, DatumVisitor, ExactReal, Procedure,
    SchemeNumber,
};

pub use port::{InputPort, OutputPort, PortType};

mod port;

pub type ValuePtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;
pub type Integer = i64;

// Type that stores all possible values!
#[derive(Collect, Clone, Copy, Default, Debug)]
#[collect(no_drop)]
pub enum Value<'gc> {
    // This is the value written as ()
    #[default]
    Null,
    // Attempting to access this value is an error
    // (but the binding exists for the purposes of set!)
    // TODO allow syntax-rules special form to define an auxillary macro `undefined`
    // that provides this value to macros
    Undefined,
    // the return value of set! and definitions (define, define-record-type, define-syntax)
    Void,
    // For now, we only support exact integers, so
    Number(Integer),
    // The Rc is so that we don't have to pay to clone the string
    // unless we try to modify it
    String(String),
    // the value of '<ident> (quote <ident>)
    Symbol(String),
    Bool(bool),
    Char(char),
    // Strings might not need to be in the GC, so
    // onlu allow interned strings for now
    // GcString(GcString<'gc>),
    // I/O with ports
    InputPort(Gc<'gc, InputPort>),
    OutputPort(Gc<'gc, OutputPort>),

    Cons(ConsCell<'gc>),
    // Represents something runnable
    Procedure(Gc<'gc, Procedure<'gc>>),
    // TODO records
    // I want to handle userdata the same way as we handle records
}

impl<'gc> Value<'gc> {
    pub fn as_number(&self) -> Option<Integer> {
        match self {
            Self::Number(int) => Some(*int),
            _ => None,
        }
    }

    pub fn as_input_port(&self) -> Option<&InputPort> {
        match self {
            Self::InputPort(inp) => Some(inp.as_ref()),
            _ => None,
        }
    }

    pub fn as_output_port(&self) -> Option<&OutputPort> {
        match self {
            Self::OutputPort(outp) => Some(outp.as_ref()),
            _ => None,
        }
    }
}

impl<'gc> fmt::Display for Value<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => write!(f, "()"),
            Value::Undefined => write!(f, "#<error>"),
            Value::Void => write!(f, "#<undef>"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => todo!(),
            Value::Symbol(s) => todo!(),
            Value::Bool(b) => todo!(),
            Value::Char(c) => todo!(),
            Value::InputPort(_) => todo!(),
            Value::OutputPort(_) => todo!(),
            Value::Cons(_) => todo!(),
            Value::Procedure(_) => todo!(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ValueConvertError {
    // currently only support exact integers
    #[error("unsupported number {0}")]
    UnsupportedNumber(SchemeNumber),
}

pub struct ValueConvert<'gc> {
    mutation: &'gc Mutation<'gc>,
    interner: &'gc mut lasso::Rodeo,
    value_stack: Vec<Result<ValuePtr<'gc>, ValueConvertError>>,
}

impl<'gc> ValueConvert<'gc> {
    pub fn new(mutation: &'gc Mutation<'gc>, interner: &'gc mut lasso::Rodeo) -> Self {
        Self {
            mutation,
            interner,
            value_stack: vec![],
        }
    }
}

impl<'gc> DatumVisitor for ValueConvert<'gc> {
    // TODO allow source to directly be converted into value
}

impl<'gc> ExternalRepresentationVisitor for ValueConvert<'gc> {
    // TODO allow external representations to be turned into values
    fn visit_number(&mut self, value: crate::SchemeNumber, _labels: &[usize]) {
        match value {
            SchemeNumber::Exact(ExactReal::Integer { value, is_neg }) => {
                // saturate at limits
                self.value_stack.push(Ok(Gc::new(
                    self.mutation,
                    RefLock::new(Value::Number(
                        i64::try_from(value)
                            .unwrap_or(i64::MAX)
                            .saturating_mul(if is_neg { -1 } else { 1 }),
                    )),
                )));
            }
            num => self
                .value_stack
                .push(Err(ValueConvertError::UnsupportedNumber(num))),
        }
    }
}

#[derive(Collect, Clone, Copy, Debug)]
#[collect(require_static)]
pub struct String(pub lasso::Spur);
impl From<lasso::Spur> for String {
    fn from(value: lasso::Spur) -> Self {
        Self(value)
    }
}
/*
use gc_arena::Mutation;
use std::string::String as StdString;
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct GcString<'gc>(Gc<'gc, StdString>);
impl<'gc> GcString<'gc> {
    pub fn new(mc: &Mutation<'gc>, string: impl AsRef<str>) -> Self {
        Self(Gc::new(mc, string.as_ref().to_owned()))
    }
}
*/
// Steal a little bit of linked list
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct ConsCell<'gc> {
    car: Option<ValuePtr<'gc>>,
    cdr: Option<ValuePtr<'gc>>,
}
