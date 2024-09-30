//! Representation of Scheme values
use std::collections::HashMap;

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    runtime::{external::ExternalRepresentationVisitor, EnvironmentPtr},
    DatumVisitor, ExactReal, Procedure, SchemeNumber,
};

pub use port::{InputPort, OutputPort, PortType};

use super::{userstruct::UserStruct, WorldArena};

mod port;

pub type ValuePtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;
pub type Integer = i64;

// Type that stores all possible values!
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub enum Value<'gc> {
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
    Vector(Vector<'gc>),
    // Strings might not need to be in the GC, so
    // onlu allow interned strings for now
    // GcString(GcString<'gc>),
    // I/O with ports
    InputPort(Gc<'gc, InputPort>),
    OutputPort(Gc<'gc, OutputPort>),

    Cons(ConsCell<'gc>),
    // Represents something runnable
    Procedure(Gc<'gc, Procedure<'gc>>),
    Environment(EnvironmentPtr<'gc>),
    UserStruct(UserStruct<'gc>),
    Error(Gc<'gc, ErrorBox>),
    // TODO records
    // I want to handle userdata the same way as we handle records
}

pub trait ValueVisitor<'gc> {
    fn visit_value(&mut self, value: ValuePtr<'gc>) {
        match *value.borrow() {
            Value::Undefined => self.visit_undefined(value),
            Value::Void => self.visit_void(value),
            Value::Vector(vec) => self.visit_vector(vec, value),
            Value::Cons(cons) => self.visit_cons(cons, value),
            Value::Number(int) => self.visit_number(int, value),
            Value::String(str) => self.visit_string(str, value),
            Value::Symbol(sym) => self.visit_symbol(sym, value),
            Value::Bool(bool) => self.visit_bool(bool, value),
            Value::Char(char) => self.visit_char(char, value),
            Value::InputPort(inp) => self.visit_input_port(inp, value),
            Value::OutputPort(oup) => self.visit_output_port(oup, value),
            Value::Procedure(_proc) => todo!(),
            Value::Environment(_env) => todo!(),
            Value::UserStruct(_uss) => todo!(),
            Value::Error(_err) => todo!(),
        }
    }

    fn visit_undefined(&mut self, value: ValuePtr<'gc>) {
        let _ = value;
    }

    fn visit_void(&mut self, value: ValuePtr<'gc>) {
        let _ = value;
    }

    fn visit_number(&mut self, integer: Integer, value: ValuePtr<'gc>) {
        let _ = value;
        _ = integer;
    }

    fn visit_string(&mut self, string: String, value: ValuePtr<'gc>) {
        let _ = value;
        _ = string;
    }

    fn visit_symbol(&mut self, symbol: String, value: ValuePtr<'gc>) {
        let _ = value;
        _ = symbol;
    }

    fn visit_bool(&mut self, bool: bool, value: ValuePtr<'gc>) {
        let _ = value;
        _ = bool;
    }

    fn visit_char(&mut self, char: char, value: ValuePtr<'gc>) {
        let _ = value;
        _ = char;
    }

    fn visit_input_port(&mut self, input_port: Gc<'gc, InputPort>, value: ValuePtr<'gc>) {
        let _ = value;
        _ = input_port;
    }

    fn visit_output_port(&mut self, output_port: Gc<'gc, OutputPort>, value: ValuePtr<'gc>) {
        let _ = value;
        _ = output_port;
    }

    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: ValuePtr<'gc>) {
        let _ = value;
        _ = cons;
    }

    fn visit_vector(&mut self, vec: Vector<'gc>, value: ValuePtr<'gc>) {
        let _ = value;
        _ = vec;
    }
    // TODO procedure, environment, userstruct, error
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

// // used for `display` impl
// ACTAUMAJDIJLLY because strings are stored in the interner, we have to use the World
// to get display or write working
// impl<'gc> fmt::Display for Value<'gc> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Value::Null => write!(f, "()"),
//             Value::Undefined => write!(f, "#<error>"),
//             Value::Void => write!(f, "#<undef>"),
//             Value::Number(n) => write!(f, "{n}"),
//             Value::String(s) => write!(f, r#""{}""#, s.0),
//             Value::Symbol(s) => write!(f, "{}"),
//             Value::Bool(b) => write!(f, "{b}"),
//             Value::Char(c) => todo!(),
//             Value::InputPort(_) => todo!(),
//             Value::OutputPort(_) => todo!(),
//             Value::Cons(_) => todo!(),
//             Value::Procedure(_) => todo!(),
//             Value::Environment(_) => todo!(),
//             Value::UserStruct(_) => todo!(),
//         }
//     }
// }

#[derive(thiserror::Error, Debug)]
pub enum ValueConvertError {
    // currently only support exact integers
    #[error("unsupported number {0}")]
    UnsupportedNumber(SchemeNumber),
}

#[expect(unused)]
// TODO Use world instead, to allow for WorldRoot::null_val usage (more like Self::ensure_null)
pub struct ValueConvert<'world, 'gc> {
    mutation: &'world Mutation<'gc>,
    interner: &'world mut lasso::Rodeo,
    arena: &'world WorldArena<'gc>,
    value_stack: Vec<Result<ValuePtr<'gc>, ValueConvertError>>,
    labeled: HashMap<usize, ValuePtr<'gc>>,
}

impl<'w, 'gc> ValueConvert<'w, 'gc> {
    pub fn new(
        mutation: &'w Mutation<'gc>,
        arena: &'w WorldArena<'gc>,
        interner: &'gc mut lasso::Rodeo,
    ) -> Self {
        Self {
            mutation,
            interner,
            arena,
            value_stack: vec![],
            labeled: HashMap::default(),
        }
    }
}

impl<'w, 'gc> DatumVisitor for ValueConvert<'w, 'gc> {
    // TODO allow source to directly be converted into value
}

impl<'w, 'gc> ExternalRepresentationVisitor for ValueConvert<'w, 'gc> {
    fn visit_number(&mut self, value: crate::SchemeNumber) {
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

// FIXME ASK Make this like UserStruct, but *with* additional data, to handle
// error predicates
#[derive(Collect, Debug)]
#[collect(require_static)]
pub struct ErrorBox(pub Box<dyn std::error::Error>);
impl<T: std::error::Error + 'static> From<T> for ErrorBox {
    fn from(value: T) -> Self {
        Self(Box::new(value))
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

#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct Vector<'gc> {
    pub vec: Gc<'gc, RefLock<Vec<ValuePtr<'gc>>>>,
}

// Steal a little bit of linked list
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct ConsCell<'gc> {
    pub car: Option<ValuePtr<'gc>>,
    pub cdr: Option<ValuePtr<'gc>>,
}

impl<'gc> ConsCell<'gc> {
    pub fn empty() -> Self {
        Self {
            car: None,
            cdr: None,
        }
    }

    pub(super) fn from_iter<
        T: IntoIterator<
            Item = ValuePtr<'gc>,
            IntoIter = impl DoubleEndedIterator<Item = ValuePtr<'gc>>,
        >,
    >(
        mc: &Mutation<'gc>,
        iter: T,
    ) -> Self {
        let mut current = ConsCell::empty();
        for item in iter.into_iter().rev() {
            let new_cell = ConsCell {
                cdr: Some(Gc::new(mc, RefLock::new(Value::Cons(current)))),
                car: Some(item),
            };

            current = new_cell;
        }

        current
    }
}
