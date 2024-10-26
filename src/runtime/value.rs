//! Representation of Scheme values
use core::fmt;
use std::rc::Rc;
use std::string::String as StdString;

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::{IntoResolver, RodeoResolver};

use crate::{environment::EnvironmentPtr, SchemeNumber};

use super::{
    error::SchemeErrorPtr,
    lambda::LambdaPtr,
    port::{InputPort, OutputPort},
    userstruct::UserStruct,
};

pub type ValuePtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;

#[derive(Collect, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[collect(require_static)]
pub enum ValueType {
    Undefined,
    Void,
    Number,
    Inexact,
    String,
    Symbol,
    Bool,
    Char,
    Vector,
    Bytevector,
    InputPort,
    OutputPort,
    Cons,
    Environment,
    UserStruct,
    Transformer,
    Lambda,
    Error,
}

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
    Number(i64),
    Inexact(f64),
    // Strings must be easily accessed/edited, so prefer to store a "String"
    // over slices or intered strings
    String(Gc<'gc, RefLock<StdString>>),
    // the value of '<ident> (quote <ident>)
    Symbol(Symbol),
    Bool(bool),
    Char(char),
    Vector(Vector<'gc>),
    Bytevector(Bytevector<'gc>),
    // Strings might not need to be in the GC, so
    // onlu allow interned strings for now
    // GcString(GcString<'gc>),
    // I/O with ports
    InputPort(Gc<'gc, InputPort>),
    OutputPort(Gc<'gc, OutputPort>),

    Cons(ConsCell<'gc>),
    // Represents something runnable
    // Procedure(Gc<'gc, Procedure>),
    Environment(EnvironmentPtr<'gc>),
    UserStruct(UserStruct<'gc>),
    // QUESTION Move from ErrorBox to an Any based pointer that
    // can specify predicate type (read-error?, file-error?, etc.)
    // Error(Gc<'gc, ErrorBox>),
    // TODO records
    // I want to handle userdata the same way as we handle records
    // TODO syntax-rules (transformers)
    // the return value of `syntax-rules`
    Transformer(RuntimeTransformer<'gc>),
    // Uniquely our lambda's are typed, it's just that (for now)
    // Scheme code simply marks all parameters as untyped
    Lambda(LambdaPtr<'gc>),
    // A Scheme-side error
    Error(SchemeErrorPtr<'gc>),
}

impl<'gc> Value<'gc> {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Undefined => ValueType::Undefined,
            Value::Void => ValueType::Void,
            Value::Number(_) => ValueType::Number,
            Value::Inexact(_) => ValueType::Inexact,
            Value::String(_) => ValueType::String,
            Value::Symbol(_) => ValueType::Symbol,
            Value::Bool(_) => ValueType::Bool,
            Value::Char(_) => ValueType::Char,
            Value::Vector(_) => ValueType::Vector,
            Value::Bytevector(_) => ValueType::Bytevector,
            Value::InputPort(_) => ValueType::InputPort,
            Value::OutputPort(_) => ValueType::OutputPort,
            Value::Cons(_) => ValueType::Cons,
            Value::Environment(_) => ValueType::Environment,
            Value::UserStruct(_) => ValueType::UserStruct,
            Value::Transformer(_) => ValueType::Transformer,
            Value::Lambda(_) => ValueType::Lambda,
            Value::Error(_) => ValueType::Error,
        }
    }

    pub fn into_ptr(self, mc: &Mutation<'gc>) -> ValuePtr<'gc> {
        Gc::new(mc, RefLock::new(self))
    }

    pub fn resolve_into<K: lasso::Key>(
        self,
        resolver: impl IntoResolver<Resolver = RodeoResolver<K>> + 'static,
    ) -> ResolvedValue<'gc, K> {
        self.resolve(Rc::new(resolver.into_resolver()))
    }

    pub fn resolve<K: lasso::Key>(self, resolver: Rc<RodeoResolver<K>>) -> ResolvedValue<'gc, K> {
        ResolvedValue {
            value: self,
            resolver,
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ResolvedValue<'gc, K: lasso::Key> {
    value: Value<'gc>,
    #[collect(require_static)]
    resolver: Rc<RodeoResolver<K>>,
}

impl<'gc, K: lasso::Key> fmt::Debug for ResolvedValue<'gc, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedValue")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<'gc> fmt::Display for ResolvedValue<'gc, lasso::Spur> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Undefined => write!(f, "#<undef>"),
            Value::Void => write!(f, "#<void>"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Inexact(fp) => write!(f, "{fp}"),
            Value::String(s) => write!(f, "\"{}\"", s.borrow().replace('\"', "\\\"")),
            Value::Symbol(sym) => write!(f, "'{}", self.resolver.resolve(&sym.0)),
            Value::Bool(b) => write!(f, "#{}", if b { "t" } else { "f" }),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Vector(vec) => {
                write!(f, "#(")?;
                for (idx, elem) in vec.vec.borrow().iter().copied().enumerate() {
                    if idx != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", elem.borrow().resolve(self.resolver.clone()))?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Bytevector(bv) => {
                write!(f, "#u8(")?;
                for (idx, elem) in bv.vec.borrow().iter().enumerate() {
                    if idx != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "#x{:02x}", elem)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::InputPort(_) => todo!(),
            Value::OutputPort(_) => todo!(),
            // TODO this needs special handling, b/c a cons might recurse into itself
            Value::Cons(_) => todo!(),
            Value::Environment(_) => todo!(),
            Value::UserStruct(_) => write!(f, "<user {:p}>", &self.value),
            Value::Transformer(_) => todo!(),
            Value::Lambda(lambda) => write!(f, "<lambda {:p}>", *lambda.borrow()),
            Value::Error(_) => todo!(),
        }
    }
}

pub trait ValueVisitor<'gc> {
    fn visit_value(&mut self, value: ValuePtr<'gc>) {
        match *value.borrow() {
            Value::Undefined => self.visit_undefined(value),
            Value::Void => self.visit_void(value),
            Value::Vector(vec) => self.visit_vector(vec, value),
            Value::Bytevector(vec) => self.visit_bytevector(vec, value),
            Value::Cons(cons) => self.visit_cons(cons, value),
            Value::Number(int) => self.visit_number(int, value),
            Value::Inexact(iex) => self.visit_inexact(iex, value),
            Value::String(str) => self.visit_string(str.as_ref().borrow().as_str(), value),
            Value::Symbol(sym) => self.visit_symbol(sym, value),
            Value::Bool(bool) => self.visit_bool(bool, value),
            Value::Char(char) => self.visit_char(char, value),
            Value::InputPort(inp) => self.visit_input_port(inp, value),
            Value::OutputPort(oup) => self.visit_output_port(oup, value),
            // Value::Procedure(_proc) => todo!(),
            Value::Environment(_env) => todo!(),
            Value::UserStruct(_uss) => todo!(),
            // Value::Error(_err) => todo!(),
            Value::Transformer(_trans) => todo!(),
            Value::Lambda(_lam) => todo!(),
            Value::Error(_err) => todo!(),
        }
    }

    fn visit_undefined(&mut self, value: ValuePtr<'gc>) {
        let _ = value;
    }

    fn visit_void(&mut self, value: ValuePtr<'gc>) {
        let _ = value;
    }

    fn visit_number(&mut self, integer: i64, value: ValuePtr<'gc>) {
        let _ = value;
        _ = integer;
    }

    fn visit_inexact(&mut self, integer: f64, value: ValuePtr<'gc>) {
        let _ = value;
        _ = integer;
    }

    fn visit_string(&mut self, string: &str, value: ValuePtr<'gc>) {
        let _ = value;
        _ = string;
    }

    fn visit_symbol(&mut self, symbol: Symbol, value: ValuePtr<'gc>) {
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

    fn visit_bytevector(&mut self, vec: Bytevector<'gc>, value: ValuePtr<'gc>) {
        let _ = value;
        _ = vec;
    }
    // TODO procedure, environment, userstruct, error, transformer
}

#[derive(thiserror::Error, Debug)]
pub enum ValueConvertError {
    // currently only support exact integers
    #[error("unsupported number {0}")]
    UnsupportedNumber(SchemeNumber),
}

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct RuntimeTransformer<'gc>(pub Gc<'gc, ()>);
impl<'gc> core::fmt::Debug for RuntimeTransformer<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<transformer {:p}>", self.0)
    }
}

#[derive(Collect, Clone, Copy, Debug)]
#[collect(require_static)]
pub struct Symbol(pub lasso::Spur);
impl From<lasso::Spur> for Symbol {
    fn from(value: lasso::Spur) -> Self {
        Self(value)
    }
}

#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct Bytevector<'gc> {
    pub vec: Gc<'gc, RefLock<Vec<u8>>>,
}
impl<'gc> From<Gc<'gc, RefLock<Vec<u8>>>> for Bytevector<'gc> {
    fn from(value: Gc<'gc, RefLock<Vec<u8>>>) -> Self {
        Self { vec: value }
    }
}

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
                cdr: Some(Value::Cons(current).into_ptr(mc)),
                car: Some(item),
            };

            current = new_cell;
        }

        current
    }
}
