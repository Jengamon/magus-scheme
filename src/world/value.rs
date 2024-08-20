//! Representation of Scheme values
use std::collections::HashMap;

use gc_arena::{Collect, Gc, GcRefLock, Mutation, RefLock};

use crate::{
    runtime::external::ExternalRepresentationVisitor, DatumVisitor, ExactReal, Procedure,
    SchemeNumber,
};

pub use port::{InputPort, OutputPort, PortType};

mod port;

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
    // Strings might not need to be in the GC, so
    // onlu allow interned strings for now
    // GcString(GcString<'gc>),
    // I/O with ports
    InputPort(Gc<'gc, InputPort>),
    OutputPort(Gc<'gc, OutputPort>),

    Cons(ConsCell<'gc>),
    // Represents something runnable
    Procedure(Gc<'gc, Procedure>),
    // TODO records
    // I want to handle userdata the same way as we handle records
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
pub type ValuePtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;

pub type Integer = i64;
