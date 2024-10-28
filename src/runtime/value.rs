//! Representation of Scheme values
use core::fmt;
use std::string::String as StdString;
use std::{cell::RefCell, rc::Rc};

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

// implements logic behind eqv?
// where as ValuePtr::eq implements eq? logic
impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Undefined => matches!(other, Value::Undefined),
            Value::Void => matches!(other, Value::Void),
            Value::Number(n) => matches!(other, Value::Number(on) if on == n),
            Value::Inexact(i) => matches!(other, Value::Inexact(oi) if oi == i),
            Value::String(s) => matches!(other, Value::String(os) if os == s),
            Value::Symbol(sym) => matches!(other, Value::Symbol(osym) if osym == sym),
            Value::Bool(b) => matches!(other, Value::Bool(ob) if ob == b),
            Value::Char(c) => matches!(other, Value::Char(oc) if oc == c),
            Value::Vector(Vector { vec: vp }) => {
                matches!(other, Value::Vector(Vector { vec: ovp }) if Gc::ptr_eq(*vp, *ovp))
            }
            Value::Bytevector(_) => todo!(),
            Value::InputPort(_) => todo!(),
            Value::OutputPort(_) => todo!(),
            Value::Cons(ConsCell {
                car: Some(car),
                cdr: Some(cdr),
            }) => {
                matches!(other, Value::Cons(ConsCell { car: Some(ocar), cdr: Some(ocdr)}) if Gc::ptr_eq(*car, *ocar) && Gc::ptr_eq(*cdr, *ocdr))
            }
            Value::Cons(ConsCell {
                car: None,
                cdr: Some(cdr),
            }) => {
                matches!(other, Value::Cons(ConsCell { car:None, cdr: Some(ocdr)}) if Gc::ptr_eq(*cdr, *ocdr))
            }
            Value::Cons(ConsCell {
                car: Some(car),
                cdr: None,
            }) => {
                matches!(other, Value::Cons(ConsCell { car: Some(ocar), cdr: None}) if Gc::ptr_eq(*car, *ocar) )
            }
            Value::Cons(ConsCell {
                car: None,
                cdr: None,
            }) => {
                matches!(
                    other,
                    Value::Cons(ConsCell {
                        car: None,
                        cdr: None
                    })
                )
            }
            Value::Environment(_) => todo!(),
            Value::UserStruct(_) => todo!(),
            Value::Transformer(_) => todo!(),
            Value::Lambda(_) => todo!(),
            Value::Error(_) => todo!(),
        }
    }
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
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        self.resolve(Rc::new(resolver.into_resolver()), null_ptr)
    }

    pub fn resolve<K: lasso::Key>(
        self,
        resolver: Rc<RodeoResolver<K>>,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        ResolvedValue {
            value: self,
            null_ptr,
            resolver,
        }
    }

    pub fn as_lambda(&self) -> Option<LambdaPtr<'gc>> {
        match self {
            Self::Lambda(lam) => Some(*lam),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Symbol(sym) => Some(*sym),
            _ => None,
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ResolvedValue<'gc, K: lasso::Key> {
    value: Value<'gc>,
    null_ptr: ValuePtr<'gc>,
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

struct ConsPrinter<'a, 'gc, K: lasso::Key> {
    cons: &'a ConsCell<'gc>,
    resolver: Rc<RodeoResolver<K>>,
    null: ValuePtr<'gc>,
    encountered: Rc<RefCell<Vec<Value<'gc>>>>,
}

impl<'a, 'gc, K: lasso::Key> fmt::Display for ConsPrinter<'a, 'gc, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // recurse into the value, keeping track of encountered cons cells
        // so that we don't recurse into them
        let cons_value = Value::Cons(*self.cons);

        if *self.null.borrow() == cons_value {
            if self.encountered.borrow().is_empty() {
                // we are starting at the null cons, so close it out
                write!(f, "()")?;
            }
            return Ok(());
        }

        write!(f, "(")?;

        if self.encountered.borrow().contains(&cons_value) {
            // write as self-recursive list
            write!(f, "...)")?;
            return Ok(());
        }

        // handle car and cdr, adding a dot if cdr is *not* a cons cell
        self.encountered.borrow_mut().push(cons_value);
        let _ = self.resolver;
        let _ = f;
        // pop encountered and close the list
        self.encountered.borrow_mut().pop();
        write!(f, "<notnull>)")
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
                    write!(
                        f,
                        "{}",
                        elem.borrow().resolve(self.resolver.clone(), self.null_ptr)
                    )?;
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
            Value::Cons(ref cons) => {
                write!(
                    f,
                    "{}",
                    ConsPrinter {
                        cons,
                        resolver: self.resolver.clone(),
                        null: self.null_ptr,
                        encountered: Rc::new(RefCell::new(Vec::new())),
                    }
                )
            }
            Value::Environment(_) => todo!(),
            Value::UserStruct(user) => {
                let label = user.label().unwrap_or("userdata");
                write!(f, "<{label} {:p}>", &self.value)
            }
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
            Value::Environment(env) => self.visit_environment(env, value),
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

    fn visit_environment(&mut self, env: EnvironmentPtr<'gc>, value: ValuePtr<'gc>) {
        let _ = env;
        let _ = value;
    }
    // TODO procedure,  userstruct, error, transformer
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

#[derive(Collect, Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

    fn is_circular_impl(&self, self_ptr: ValuePtr<'gc>, stack: &mut Vec<ValuePtr<'gc>>) -> bool {
        stack.push(self_ptr);
        if let Some(val) = self.car {
            if let Value::Cons(cell) = *val.borrow() {
                if cell.is_circular_impl(val, stack) {
                    return true;
                }
            }
        }
        if let Some(val) = self.cdr {
            if let Value::Cons(cell) = *val.borrow() {
                if cell.is_circular_impl(val, stack) {
                    return true;
                }
            }
        }
        assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
        false
    }

    pub fn is_circular(&self, self_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_circular_impl(self_ptr, &mut stack)
    }

    fn is_param_list_impl(&self, self_ptr: ValuePtr<'gc>, stack: &mut Vec<ValuePtr<'gc>>) -> bool {
        stack.push(self_ptr);
        if let Some(val) = self.car {
            match *val.borrow() {
                Value::Cons(cell) => {
                    if !cell.is_param_list_impl(val, stack) {
                        return false;
                    }
                }
                Value::Symbol(_) => {}
                _ => return false,
            }
        }
        if let Some(val) = self.cdr {
            match *val.borrow() {
                Value::Cons(cell) => {
                    if !cell.is_param_list_impl(val, stack) {
                        return false;
                    }
                }
                Value::Symbol(_) => {}
                _ => return false,
            }
        }
        assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
        true
    }

    pub fn is_param_list(&self, self_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_param_list_impl(self_ptr, &mut stack)
    }

    pub fn from_iter<
        T: IntoIterator<
            Item = ValuePtr<'gc>,
            IntoIter = impl DoubleEndedIterator<Item = ValuePtr<'gc>>,
        >,
    >(
        mc: &Mutation<'gc>,
        null: ValuePtr<'gc>,
        iter: T,
    ) -> ValuePtr<'gc> {
        let mut current = null;
        for item in iter.into_iter().rev() {
            let new_cell = Value::Cons(ConsCell {
                cdr: Some(current),
                car: Some(item),
            })
            .into_ptr(mc);

            current = new_cell;
        }

        current
    }
}
