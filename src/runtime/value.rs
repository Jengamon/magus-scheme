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
    String(Gc<'gc, RefLock<StdString>>),
    // the value of '<ident> (quote <ident>)
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
    Promise(()),
    // A Scheme-side error
    Error(SchemeErrorPtr<'gc>),
}

// implements logic behind eqv?
// where as ValuePtr::eq implements eq? logic
impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Values(v) => matches!(other, Value::Values(ov) if v == ov),
            Value::Undefined => matches!(other, Value::Undefined),
            Value::Void => matches!(other, Value::Void),
            Value::Number(n) => matches!(other, Value::Number(on) if on == n),
            Value::Inexact(i) => matches!(other, Value::Inexact(oi) if oi == i),
            Value::String(s) => matches!(other, Value::String(os) if Gc::ptr_eq(*s, *os)),
            Value::Symbol(sym) => matches!(other, Value::Symbol(osym) if osym == sym),
            Value::Bool(b) => matches!(other, Value::Bool(ob) if ob == b),
            Value::Char(c) => matches!(other, Value::Char(oc) if oc == c),
            Value::Vector(vp) => {
                matches!(other, Value::Vector(ovp) if Gc::ptr_eq(*vp, *ovp))
            }
            Value::Bytevector(_) => todo!(),
            Value::Record(_) => todo!(),
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
            Value::UserStruct(_) => todo!(),
            Value::Lambda(lptr) => matches!(other, Value::Lambda(optr) if lptr == optr),
            Value::Continuation(c) => matches!(other, Value::Continuation(oc) if c == oc),
            Value::Promise(_) => todo!(),
            Value::Error(_) => todo!(),
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

    // pub fn as_symbol(&self) -> Option<Symbol> {
    //     match self {
    //         Self::Symbol(sym) => Some(*sym),
    //         _ => None,
    //     }
    // }
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
        // so that we don't recurse into them
        todo!()
    }
}

/// Checks if a given symbol is a valid unpiped Scheme identifier
fn is_valid_scheme_identifier(s: &str) -> bool {
    if !s.is_ascii() || s.chars().any(|c| !c.is_ascii_graphic()) {
        // Scheme identifiers must be in ASCII (and not whitespace)
        return false;
    }

    // TODO add refinements, as we are currently too permissive atm
    true
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
            Value::String(s) => write!(f, "\"{}\"", s.borrow().replace('\"', "\\\"")),
            Value::Symbol(sym) if is_valid_scheme_identifier(self.resolver.resolve(&sym.0)) => {
                write!(f, "{}", self.resolver.resolve(&sym.0))
            }
            Value::Symbol(sym) => write!(f, "|{}|", self.resolver.resolve(&sym.0)),
            Value::Bool(b) => write!(f, "#{}", if b { "t" } else { "f" }),
            Value::Char(c) => write!(f, "#\\{c}"),
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
                    return write!(f, "'()");
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
                            write!(f, "'()")?;
                        }
                    } else {
                        write!(f, "'()")?;
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
            Value::Promise(_) => todo!(),
            Value::Error(e) => write!(f, "#<error {e:p}>"),
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

// FIXME make this a struct of usize (stack index) and a ThreadPtr (a "brand")
// (the brand will keep us from trying to execute a continuation on the wrong thread)
// FIXME FIXME Racket is very helpful. (from a racket manual) We start by formulating a representation of the context.
// An evaluation context will be represented as a continuation: a list of frames, where a frame is a single flat evaluation context,
// i.e. either a conditional or application context with no nested evaluation context inside (the hole will be represented by []).
// Conceptually, the continuation is a stack of actions that remain to be done.
// It’s also easy to see that continuations and evaluation contexts are inter-convertible: the inner-most part of an evaluation context is the first frame of a continuation; the outer-most part of the context corresponds to the final frame of a continuation; an empty context is represented by an empty list of frames.
//
// So a continuation is the entire frame state from a point in time. When we sub in frames, if a frame has a handler, and !Gc::ptr_eq to the
// frame in its position, then we handle any dynamic-wind handlers it may have.
/// A bytecode chunk and program counter bundled together
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
                    if cell.is_list_impl(val, null_ptr, stack) {
                        return true;
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
