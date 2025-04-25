//! Representation of Scheme values

use core::fmt;
use std::rc::Rc;
use std::string::String as StdString;

use gc_arena::{Collect, Gc, Mutation, RefLock, Static};
use lasso::IntoResolver;

use crate::environment::StackEnvironmentPtr;
use crate::interpreter::thread::ThreadFrame;

use super::lambda::Lambda;
use super::{
    error::SchemeErrorPtr,
    // lambda::LambdaPtr,
    port::{InputPort, OutputPort},
    userstruct::UserStruct,
};

mod number;

pub use number::{ComplexNumber, ComplexNumberPtr, Number, NumberPtr};

pub type ValuePtr<'gc> = Gc<'gc, RefLock<Value<'gc>>>;

#[derive(Collect, Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[collect(require_static)]
pub enum ValueType {
    Values,
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
    Record,
    InputPort,
    OutputPort,
    Cons,
    Environment,
    UserStruct,
    Lambda,
    Continuation,
    Promise,
    Parameter,
    Error,
}

// Type that stores all possible values!
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub enum Value<'gc> {
    // Special form of value representing multiple returns
    // Is generally opaque
    Values(Gc<'gc, Vec<ValuePtr<'gc>>>),
    // Attempting to access this value is an error
    // (but the binding exists for the purposes of set!)
    // TODO allow syntax-rules special form to define an auxillary macro `undefined`
    // that provides this value to macros
    Undefined,
    // the return value of set! and definitions (define, define-record-type, define-syntax)
    Void,
    // TODO Expand supported numbers, so that Number becomes a big num,
    // and add support for exact rationals and complex numbers
    // (and polar numbers once support lands in the lexer)
    // From R7RS report around 1.3.3, that implementation restrictions are discouraged.
    // We want to support w/e madness a programmer dreams of in the language, but the external program can feel free
    // to convert the given data into the format they need for their usage.
    Number(NumberPtr<'gc>),
    // TODO Add ComplexNumber and ComplexInexact
    Inexact(f64),
    // Strings must be easily accessed/edited, so prefer to store a "String"
    // over slices or intered strings
    String(String<'gc>),
    /// the value of `'<ident>` `(quote <ident>)`
    Symbol(Symbol),
    Bool(bool),
    Char(char),
    Vector(VectorPtr<'gc>),
    Bytevector(Bytevector<'gc>),
    Record(()),
    // Strings might not need to be in the GC, so
    // onlu allow interned strings for now
    // GcString(GcString<'gc>),
    // I/O with ports
    InputPort(Gc<'gc, InputPort>),
    OutputPort(Gc<'gc, OutputPort>),

    Cons(ConsCell<'gc>),
    // Represents something runnable
    // Procedure(Gc<'gc, Procedure>),
    // TODO Change this to an enum that support Immutable (which would just be import sets)
    // and Mutable (which is just a CompilerPtr (the environment ptr is implicitly the default env ptr of the compiler))
    // so that `eval` and rebuild the compiler environment when used
    Environment(StackEnvironmentPtr<'gc>),
    UserStruct(UserStruct<'gc>),
    // Uniquely our lambda's are typed, it's just that (for now)
    // Scheme code simply marks all parameters as untyped
    Lambda(Lambda<'gc>),
    // A continuation is a chunk and program counter
    // bundled together, and is treated as a callable
    Continuation(ContinuationPtr<'gc>),
    // Used by (scheme lazy) to implement call-by-need
    // you can think of this as a thunk lambda (a lambda with no args)
    // where the execution result is memoized (so that multiple calls to "force" it will
    // always result in the same value, which is calculated in the environment where it was forced
    // for the first time)
    // So this a blob of bytecode that is to be evaluated in a surrounding chunk's environment,
    // (just a blob and a memoize slot)
    Promise(PromisePtr<'gc>),
    Parameter(Parameter<'gc>),
    // TODO Impl native parameter objects
    //
    // These parameter objects should add something to a frame that is handled at the same time as dynamic-wind
    // A parameter object is conceptually a stack of values, where each frame that has a value for that parameter
    // push it onto the object when it sets it, and when execution leaves the scope it was set in, the value is
    // popped
    /// A Scheme-side error
    Error(SchemeErrorPtr<'gc>),
}

// implements logic behind equal?
impl Value<'_> {
    pub fn is_equal(self, other: Self) -> bool {
        match self {
            Value::Values(v) => {
                matches!(other, Value::Values(ov) if v.len() == ov.len() && v.iter().zip(ov.iter()).all(|(v, ov)|
                    v.borrow().is_equal(*ov.borrow())
                ))
            }
            Value::Undefined => false,
            Value::Void => matches!(other, Value::Void),
            Value::Number(n) => matches!(other, Value::Number(on) if on == n),
            Value::Inexact(i) => matches!(other, Value::Inexact(oi) if oi == i),
            Value::String(s) => {
                matches!(other, Value::String(os) if s.string == os.string)
            }
            Value::Symbol(sym) => matches!(other, Value::Symbol(osym) if osym == sym),
            Value::Bool(b) => matches!(other, Value::Bool(ob) if ob == b),
            Value::Char(c) => matches!(other, Value::Char(oc) if oc == c),
            Value::Vector(_vp) => {
                // Have to handle circular structures
                todo!()
            }
            Value::Cons(_) => {
                // Have to handle circular structures
                todo!()
            }
            Value::Bytevector(bv) => {
                if let Value::Bytevector(obv) = other {
                    bv.vec.len() == obv.vec.len()
                        && bv.vec.iter().zip(obv.vec.iter()).all(|(b, ob)| b == ob)
                } else {
                    false
                }
            }
            Value::Record(_) => todo!(),
            Value::InputPort(ip) => matches!(other, Value::InputPort(oip) if Gc::ptr_eq(ip, oip)),
            Value::OutputPort(op) => {
                matches!(other, Value::OutputPort(oop) if Gc::ptr_eq(op, oop))
            }
            Value::Environment(_) => todo!(),
            Value::UserStruct(us) => {
                matches!(other, Value::UserStruct(ous) if us == ous)
            }
            Value::Lambda(lptr) => matches!(other, Value::Lambda(optr) if lptr == optr),
            Value::Continuation(c) => matches!(other, Value::Continuation(oc) if c == oc),
            Value::Promise(p) => matches!(other, Value::Promise(op) if Gc::ptr_eq(p, op)),
            Value::Parameter(p) => matches!(other, Value::Parameter(op) if p == op),
            Value::Error(e) => matches!(other, Value::Error(oe) if Gc::ptr_eq(e, oe)),
        }
    }
}

// implements logic behind eqv?
// where as ValuePtr::eq implements eq? logic
impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Values(v) => matches!(other, Value::Values(ov) if v == ov),
            Value::Undefined => false,
            Value::Void => matches!(other, Value::Void),
            Value::Number(n) => matches!(other, Value::Number(on) if on == n),
            Value::Inexact(i) => matches!(other, Value::Inexact(oi) if oi == i),
            Value::String(s) => {
                matches!(other, Value::String(os) if Gc::ptr_eq(s.string, os.string))
            }
            Value::Symbol(sym) => matches!(other, Value::Symbol(osym) if osym == sym),
            Value::Bool(b) => matches!(other, Value::Bool(ob) if ob == b),
            Value::Char(c) => matches!(other, Value::Char(oc) if oc == c),
            Value::Vector(vp) => {
                matches!(other, Value::Vector(ovp) if Gc::ptr_eq(*vp, *ovp))
            }
            Value::Bytevector(bv) => {
                matches!(other, Value::Bytevector(obv) if Gc::ptr_eq(bv.vec, obv.vec))
            }
            Value::Record(_) => todo!(),
            Value::InputPort(ip) => matches!(other, Value::InputPort(oip) if Gc::ptr_eq(*ip, *oip)),
            Value::OutputPort(op) => {
                matches!(other, Value::OutputPort(oop) if Gc::ptr_eq(*op, *oop))
            }
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
            // *technically* is wrong, but as long as there is no way to manufacture ConsCell w/ (None None),
            // this is essentially correct
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
            Value::UserStruct(us) => {
                matches!(other, Value::UserStruct(ous) if us == ous)
            }
            Value::Lambda(lptr) => matches!(other, Value::Lambda(optr) if lptr == optr),
            Value::Continuation(c) => matches!(other, Value::Continuation(oc) if c == oc),
            Value::Promise(p) => matches!(other, Value::Promise(op) if Gc::ptr_eq(*p, *op)),
            Value::Parameter(p) => matches!(other, Value::Parameter(op) if p == op),
            Value::Error(e) => matches!(other, Value::Error(oe) if Gc::ptr_eq(*e, *oe)),
        }
    }
}

impl<'gc> Value<'gc> {
    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Values(_) => ValueType::Values,
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
            Value::Record(_) => ValueType::Record,
            Value::InputPort(_) => ValueType::InputPort,
            Value::OutputPort(_) => ValueType::OutputPort,
            Value::Cons(_) => ValueType::Cons,
            Value::Environment(_) => ValueType::Environment,
            Value::UserStruct(_) => ValueType::UserStruct,
            Value::Lambda(_) => ValueType::Lambda,
            Value::Continuation(_) => ValueType::Continuation,
            Value::Promise(_) => ValueType::Promise,
            Value::Parameter(_) => ValueType::Parameter,
            Value::Error(_) => ValueType::Error,
        }
    }

    pub fn into_ptr(self, mc: &Mutation<'gc>) -> ValuePtr<'gc> {
        Gc::new(mc, RefLock::new(self))
    }

    pub fn resolve_into<K: lasso::Resolver>(
        value_ptr: ValuePtr<'gc>,
        resolver: impl IntoResolver<Resolver = K> + 'static,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        Self::resolve(value_ptr, Rc::new(resolver.into_resolver()), null_ptr)
    }

    pub fn resolve<K: lasso::Resolver>(
        value_ptr: ValuePtr<'gc>,
        resolver: Rc<K>,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        ResolvedValue {
            value: *value_ptr.borrow(),
            value_ptr,
            null_ptr,
            resolver,
        }
    }
}

enum ConsInner<'a, 'gc> {
    Cons(&'a ConsCell<'gc>),
    Vec(&'a Vector<'gc>),
}
// Handles printing possibly self-referential structures
struct CircularPrinter<'a, 'gc, K: lasso::Resolver> {
    cons: ConsInner<'a, 'gc>,
    self_ptr: ValuePtr<'gc>,
    resolver: Rc<K>,
    null_ptr: ValuePtr<'gc>,
}

// TODO Change this to an implementation of Brent's algorithm
// and DFS (as it currently is)
impl<K: lasso::Resolver> fmt::Display for CircularPrinter<'_, '_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // recurse into the value, keeping track of encountered cons cells
        // so that we don't recurse into them (labeling them as we encounter them)
        // then we should print the values as labeled.
        // todo!()
        write!(f, "(TODO SELF-RECURSIVE)")
    }
}

/// Checks if a given symbol is a valid unpiped Scheme identifier
// Using the definition used for `write`
fn is_valid_scheme_identifier(s: &str) -> bool {
    if !s.is_ascii() || s.chars().any(|c| !c.is_ascii_graphic()) {
        // Scheme identifiers must be in ASCII (and not whitespace)
        return false;
    } else if s.starts_with(|c: char| c.is_ascii_digit()) {
        // due to lexing technicalities, we don't allow unquoted idenfiers to start with an ASCII digit
        return false;
    }
    true
}

pub fn escape_write_char(c: char, is_single: bool) -> Vec<char> {
    match c {
        '\t' => vec!['\\', 't'],
        '\n' => vec!['\\', 'n'],
        '\r' => vec!['\\', 'r'],
        '\\' => vec!['\\', '\\'],
        c if c.is_control() => {
            let mut val = c as u32;
            let chars = {
                let mut digits = vec![];
                while val > 0 {
                    digits.push(
                        char::from_digit(val % 16, 16).expect("[ICE] char to escape conversion"),
                    );
                    val /= 16;
                }
                digits.reverse();
                if !is_single {
                    // add the coda (if string)
                    digits.push(';');
                }
                digits
            };
            ['\\', 'x'].into_iter().chain(chars).collect()
        }
        '\"' => vec!['\\', '"'],
        c => vec![c],
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ResolvedValue<'gc, R: lasso::Resolver> {
    value: Value<'gc>,
    value_ptr: ValuePtr<'gc>,
    null_ptr: ValuePtr<'gc>,
    #[collect(require_static)]
    resolver: Rc<R>,
}

impl<R: lasso::Resolver> Clone for ResolvedValue<'_, R> {
    fn clone(&self) -> Self {
        Self {
            value: self.value,
            value_ptr: self.value_ptr,
            null_ptr: self.null_ptr,
            resolver: self.resolver.clone(),
        }
    }
}

impl<K: lasso::Resolver> fmt::Debug for ResolvedValue<'_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedValue")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<K: lasso::Resolver> fmt::Display for ResolvedValue<'_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Values(v) => {
                write!(f, "#<values count={}>", v.len())
            }
            Value::Undefined => write!(f, "#<undef>"),
            Value::Void => write!(f, "#<void>"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Inexact(fp) if fp.is_infinite() && fp.is_sign_negative() => write!(f, "-inf.0"),
            Value::Inexact(fp) if fp.is_infinite() => write!(f, "+inf.0"),
            Value::Inexact(fp) if fp.is_nan() => write!(f, "+nan.0"),
            Value::Inexact(fp) => write!(f, "{fp}"),
            Value::String(s) => write!(
                f,
                "\"{}\"",
                s.borrow()
                    .chars()
                    .flat_map(|c| escape_write_char(c, false))
                    .collect::<Box<str>>()
            ),
            Value::Symbol(sym) if is_valid_scheme_identifier(self.resolver.resolve(&sym.0)) => {
                write!(f, "{}", self.resolver.resolve(&sym.0))
            }
            Value::Symbol(sym) => write!(f, "|{}|", self.resolver.resolve(&sym.0)),
            Value::Bool(b) => write!(f, "#{}", if b { "t" } else { "f" }),
            Value::Char(c) => write!(
                f,
                "#\\{}",
                escape_write_char(c, true).into_iter().collect::<Box<str>>()
            ),
            Value::Vector(ref vec) if vec.is_circular(self.value_ptr) => {
                write!(
                    f,
                    "{}",
                    CircularPrinter {
                        cons: ConsInner::Vec(vec),
                        self_ptr: self.value_ptr,
                        resolver: self.resolver.clone(),
                        null_ptr: self.null_ptr,
                    }
                )
            }
            Value::Vector(ref vec) => {
                // just dfs the structure, we know it isn't circular
                write!(f, "#(")?;
                for (idx, elem) in vec.vec.iter().enumerate() {
                    if idx != 0 {
                        write!(f, " ")?;
                    }
                    write!(
                        f,
                        "{}",
                        ResolvedValue {
                            value: *elem.borrow(),
                            value_ptr: *elem,
                            null_ptr: self.null_ptr,
                            resolver: Rc::clone(&self.resolver)
                        }
                    )?;
                }
                write!(f, ")")
            }
            Value::Bytevector(bv) => {
                write!(f, "#u8(")?;
                for (idx, elem) in bv.vec.iter().enumerate() {
                    if idx != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            // Handle with cons printer (or just display name and member names, so we don't have to!!)
            Value::Record(_) => todo!(),
            Value::InputPort(_) => todo!(),
            Value::OutputPort(_) => todo!(),
            // TODO this needs special handling, b/c a cons might recurse into itself
            Value::Cons(ref cons) if cons.is_circular(self.value_ptr) => {
                write!(
                    f,
                    "{}",
                    CircularPrinter {
                        cons: ConsInner::Cons(cons),
                        self_ptr: self.value_ptr,
                        resolver: self.resolver.clone(),
                        null_ptr: self.null_ptr,
                    }
                )
            }
            Value::Cons(ref cons) => {
                // we know we aren't cyclical at *all*, so just dfs

                // special handling for null
                if Gc::ptr_eq(self.value_ptr, self.null_ptr) {
                    return write!(f, "()");
                }

                write!(f, "(")?;
                let mut cons = *cons;
                loop {
                    let car = cons.car;
                    let cdr = cons.cdr;
                    if let Some(car) = car {
                        if !Gc::ptr_eq(car, self.null_ptr) {
                            write!(
                                f,
                                "{}",
                                ResolvedValue {
                                    value: *car.borrow(),
                                    value_ptr: car,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver)
                                }
                            )?;
                        } else {
                            write!(f, "()")?;
                        }
                    } else {
                        write!(f, "()")?;
                    }
                    if !cdr.is_none_or(|cdr| Gc::ptr_eq(cdr, self.null_ptr)) {
                        write!(f, " ")?;
                        let Some(cdr) = cdr else {
                            unreachable!();
                        };

                        if let Value::Cons(c) = *cdr.borrow() {
                            cons = c;
                        } else {
                            write!(
                                f,
                                ". {})",
                                ResolvedValue {
                                    value: *cdr.borrow(),
                                    value_ptr: cdr,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver),
                                }
                            )?;
                            break;
                        }
                    } else {
                        write!(f, ")")?;
                        break;
                    }
                }

                Ok(())
            }
            Value::Environment(_) => todo!(),
            Value::UserStruct(user) => {
                let label = user.label().unwrap_or("userdata");
                write!(f, "#<{label} {:p}>", &self.value)
            }
            // Value::Lambda(lambda) => write!(f, "<lambda {:p}>", *lambda.borrow()),
            Value::Lambda(lambda) => write!(f, "#<lambda {lambda:p}>"),
            Value::Continuation(cont) => write!(f, "#<continuation {cont}>"),
            Value::Promise(p) => write!(
                f,
                "#<promise {} . {p:p}>",
                if p.borrow().is_evaled() { "#t" } else { "#f" }
            ),
            Value::Parameter(p) => write!(f, "#<parameter {p:p}>"),
            Value::Error(e) => write!(f, "#<error {e:p}>"),
        }
    }
}

#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct String<'gc> {
    string: Gc<'gc, RefLock<StdString>>,
    /// Should this string be mutated?
    ///
    /// Native lambdas are technically free to disrespect this flag,
    /// but it is used to be compliant with the results of something like `(symbol->string)`
    pub frozen: bool,
}
impl<'gc> String<'gc> {
    /// Create a new string that should not be mutated
    pub fn new_frozen(string: Gc<'gc, RefLock<StdString>>) -> Self {
        Self {
            string,
            frozen: true,
        }
    }
}
impl<'gc> std::ops::Deref for String<'gc> {
    type Target = Gc<'gc, RefLock<StdString>>;
    fn deref(&self) -> &Self::Target {
        &self.string
    }
}
impl<'gc> From<Gc<'gc, RefLock<StdString>>> for String<'gc> {
    fn from(value: Gc<'gc, RefLock<StdString>>) -> Self {
        Self {
            string: value,
            frozen: false,
        }
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

#[derive(Clone, Copy, Debug, Collect)]
#[collect(no_drop)]
pub struct Bytevector<'gc> {
    pub vec: Gc<'gc, Static<im_rc::Vector<u8>>>,
}
impl<'gc> From<Gc<'gc, Static<im_rc::Vector<u8>>>> for Bytevector<'gc> {
    fn from(value: Gc<'gc, Static<im_rc::Vector<u8>>>) -> Self {
        Self { vec: value }
    }
}

#[derive(Clone, Debug)]
pub struct Vector<'gc> {
    pub vec: im_rc::Vector<ValuePtr<'gc>>,
}
impl<'gc> Vector<'gc> {
    pub fn new(value: im_rc::Vector<ValuePtr<'gc>>) -> Self {
        Self { vec: value }
    }

    pub fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::Vector(Gc::new(mc, self))
    }
}
pub type VectorPtr<'gc> = Gc<'gc, Vector<'gc>>;

#[allow(unsafe_code)]
unsafe impl<'gc> Collect<'gc> for Vector<'gc> {
    fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
        for ptr in self.vec.iter() {
            ptr.trace(cc);
        }
    }
}

impl<'gc> Vector<'gc> {
    fn is_circular_impl(&self, self_ptr: ValuePtr<'gc>, stack: &mut Vec<ValuePtr<'gc>>) -> bool {
        stack.push(self_ptr);
        for val in self.vec.iter().copied() {
            if stack.contains(&val) {
                return true;
            }

            match *val.borrow() {
                Value::Cons(cell) => {
                    if cell.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                Value::Vector(vec) => {
                    if vec.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
        false
    }

    /// Returns if a vector is circular (self-referential)
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_circular(&self, self_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_circular_impl(self_ptr, &mut stack)
    }
}

/// The state of execution frames at some point in time
#[derive(Debug, Collect, Clone, PartialEq, Eq)]
#[collect(no_drop)]
pub struct Continuation<'gc> {
    pub(crate) frames: Rc<[ThreadFrame<'gc>]>,
}
pub type ContinuationPtr<'gc> = Gc<'gc, Continuation<'gc>>;

impl<'gc> Continuation<'gc> {
    pub(crate) fn new(frames: impl IntoIterator<Item = ThreadFrame<'gc>>) -> Self {
        Self {
            frames: Rc::from(frames.into_iter().collect::<Vec<_>>().as_slice()),
        }
    }
}

impl fmt::Display for Continuation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:p} ({})", self.frames.len())
    }
}

/// A promise is a lambda that is run at most once
#[derive(Clone, Copy, Collect, Debug)]
#[collect(no_drop)]
pub enum Promise<'gc> {
    /// Will be called with 0 args
    Unevaled(Lambda<'gc>),
    /// The promise has been evaluated, so just return the value
    Evaled(ValuePtr<'gc>),
}
pub type PromisePtr<'gc> = Gc<'gc, RefLock<Promise<'gc>>>;

impl<'gc> Promise<'gc> {
    pub fn promise(mc: &Mutation<'gc>, promise: PromisePtr<'gc>, value: ValuePtr<'gc>) {
        if matches!(*promise.borrow(), Promise::Unevaled(_)) {
            *promise.borrow_mut(mc) = Promise::Evaled(value);
        }
    }

    pub fn label(
        &self,
        mc: &Mutation<'gc>,
        maybe_label: Option<usize>,
        f: impl FnOnce() -> usize,
    ) -> Option<Self> {
        if let Promise::Unevaled(l) = self {
            let l = if let Some(upvalue_index) = maybe_label {
                l.label(mc, upvalue_index)
            } else {
                l.label(mc, f())
            };
            Some(Promise::Unevaled(l))
        } else {
            None
        }
    }
}

impl Promise<'_> {
    pub fn is_evaled(&self) -> bool {
        matches!(self, Promise::Evaled(_))
    }
}

#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
/// A dynamically bound value location with a
/// default value, and possibly a conversion lambda
pub struct Parameter<'gc> {
    id: Gc<'gc, ()>,
    pub(crate) init: ValuePtr<'gc>,
    pub(crate) convert: Option<Lambda<'gc>>,
}
impl PartialEq for Parameter<'_> {
    fn eq(&self, other: &Self) -> bool {
        Gc::ptr_eq(self.id, other.id)
    }
}
impl Eq for Parameter<'_> {}
impl std::fmt::Pointer for Parameter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.id)
    }
}

impl<'gc> Parameter<'gc> {
    pub fn new(mc: &Mutation<'gc>, init: ValuePtr<'gc>) -> Self {
        Self {
            id: Gc::new(mc, ()),
            init,
            convert: None,
        }
    }

    pub fn with_convert(mc: &Mutation<'gc>, init: ValuePtr<'gc>, convert: Lambda<'gc>) -> Self {
        Self {
            id: Gc::new(mc, ()),
            init,
            convert: Some(convert),
        }
    }
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
            if stack.contains(&val) {
                return true;
            }

            match *val.borrow() {
                Value::Cons(cell) => {
                    if cell.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                Value::Vector(vec) => {
                    if vec.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        if let Some(val) = self.cdr {
            if stack.contains(&val) {
                return true;
            }

            match *val.borrow() {
                Value::Cons(cell) => {
                    if cell.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                Value::Vector(vec) => {
                    if vec.is_circular_impl(val, stack) {
                        return true;
                    }
                }
                _ => {}
            }
        }
        assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
        false
    }

    /// Returns if a cons cell is circular (self-referential)
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_circular(&self, self_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_circular_impl(self_ptr, &mut stack)
    }

    fn is_list_impl(
        &self,
        self_ptr: ValuePtr<'gc>,
        null_ptr: ValuePtr<'gc>,
        stack: &mut Vec<ValuePtr<'gc>>,
    ) -> bool {
        if let Some(val) = self.cdr {
            match *val.borrow() {
                Value::Cons(_) if Gc::ptr_eq(val, null_ptr) => return true,
                // only non-self recursive values are considered lists (for now)
                // (and probably ever)
                Value::Cons(cell) if stack.iter().all(|ptr| !Gc::ptr_eq(*ptr, self_ptr)) => {
                    stack.push(self_ptr);
                    if !cell.is_list_impl(val, null_ptr, stack) {
                        return false;
                    }
                    assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
                }
                _ => return false,
            }
        }
        true
    }

    /// Returns if a cons cell is a valid list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_list(&self, self_ptr: ValuePtr<'gc>, null_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_list_impl(self_ptr, null_ptr, &mut stack)
    }

    fn is_listable_impl(
        &self,
        self_ptr: ValuePtr<'gc>,
        null_ptr: ValuePtr<'gc>,
        stack: &mut Vec<ValuePtr<'gc>>,
    ) -> bool {
        if let Some(val) = self.cdr {
            match *val.borrow() {
                Value::Cons(_) if Gc::ptr_eq(val, null_ptr) => return true,
                // only non-self recursive values are considered lists (for now)
                // (and probably ever)
                Value::Cons(cell) if stack.iter().all(|ptr| !Gc::ptr_eq(*ptr, self_ptr)) => {
                    stack.push(self_ptr);
                    if cell.is_listable_impl(val, null_ptr, stack) {
                        return true;
                    }
                    assert!(Gc::ptr_eq(stack.pop().unwrap(), self_ptr));
                }
                _ => return false,
            }
        }
        true
    }

    /// Returns if a cons cell can look like a list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_listable(&self, self_ptr: ValuePtr<'gc>, null_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = vec![];
        self.is_listable_impl(self_ptr, null_ptr, &mut stack)
    }

    /// Get the values of a list (permissively treating improper lists as proper but skipping
    /// the last value)
    pub fn list_values(
        &self,
        self_ptr: ValuePtr<'gc>,
        null_ptr: ValuePtr<'gc>,
    ) -> impl IntoIterator<Item = ValuePtr<'gc>> + use<'gc> {
        if !self.is_listable(self_ptr, null_ptr) {
            return if let Some(car) = self.car {
                vec![car]
            } else {
                vec![]
            };
        }

        if Gc::ptr_eq(self_ptr, null_ptr) {
            vec![]
        } else {
            let car = self.car.unwrap_or(null_ptr);
            let cdr = if let Some(v) = self.cdr {
                match *v.borrow() {
                    _ if Gc::ptr_eq(v, null_ptr) => vec![],
                    Value::Cons(c) => c.list_values(v, null_ptr),
                    _ => vec![],
                }
            } else {
                vec![]
            };

            std::iter::once(car).chain(cdr).collect::<Vec<_>>()
        }
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
