//! Representation of Scheme values
use gc_arena::{Collect, Gc, RefLock};

use crate::runtime::Procedure;

pub use port::{InputPort, OutputPort, PortType};

mod port;

// Type that stores all possible values!
#[derive(Collect, Clone, Copy, Default, Debug)]
#[collect(no_drop)]
pub enum Value<'gc> {
    // This is the value written as ()
    #[default]
    Null,
    // "Returning" (more formally, evaluating to) this value is an error
    Undefined,
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
    Procedure(Procedure<'gc>),
}
pub type ValuePtr<'gc> = Gc<'gc, Value<'gc>>;

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
    car: Option<ValueRefPtr<'gc>>,
    cdr: Option<ValueRefPtr<'gc>>,
}
pub type ValueRefPtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;

pub type Integer = i64;
