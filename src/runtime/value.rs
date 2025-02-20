//! Representation of Scheme values

use core::fmt;
use std::string::String as StdString;
use std::{cell::RefCell, rc::Rc};

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::IntoResolver;

use crate::bytecode::ChunkPtr;
use crate::environment::StackEnvironmentPtr;

use super::lambda::{Arity, Lambda};
use super::{
    error::SchemeErrorPtr,
    // lambda::LambdaPtr,
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
    Lambda,
    Continuation,
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
    Environment(StackEnvironmentPtr<'gc>),
    UserStruct(UserStruct<'gc>),
    // Uniquely our lambda's are typed, it's just that (for now)
    // Scheme code simply marks all parameters as untyped
    Lambda(Lambda<'gc>),
    // A continuation is a chunk and program counter
    // bundled together, and is treated as a callable
    Continuation(ContinuationPtr<'gc>),
    // A Scheme-side error
    Error(SchemeErrorPtr<'gc>),
}

// implements logic behind eqv?
// where as ValuePtr::eq implements eq? logic
impl PartialEq for Value<'_> {
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
            Value::Lambda(lptr) => matches!(other, Value::Lambda(optr) if lptr == optr),
            Value::Continuation(c) => matches!(other, Value::Continuation(oc) if c == oc),

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
            Value::Lambda(_) => ValueType::Lambda,
            Value::Continuation(_) => ValueType::Continuation,
            Value::Error(_) => ValueType::Error,
        }
    }

    pub fn into_ptr(self, mc: &Mutation<'gc>) -> ValuePtr<'gc> {
        Gc::new(mc, RefLock::new(self))
    }

    pub fn resolve_into<K: lasso::Resolver>(
        self,
        resolver: impl IntoResolver<Resolver = K> + 'static,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        self.resolve(Rc::new(resolver.into_resolver()), null_ptr)
    }

    pub fn resolve<K: lasso::Resolver>(
        self,
        resolver: Rc<K>,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K> {
        ResolvedValue {
            value: self,
            null_ptr,
            resolver,
        }
    }

    // pub fn as_lambda(&self) -> Option<LambdaPtr<'gc>> {
    //     match self {
    //         Self::Lambda(lam) => Some(*lam),
    //         _ => None,
    //     }
    // }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Symbol(sym) => Some(*sym),
            _ => None,
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ResolvedValue<'gc, R: lasso::Resolver> {
    value: Value<'gc>,
    null_ptr: ValuePtr<'gc>,
    #[collect(require_static)]
    resolver: Rc<R>,
}

impl<'gc, R: lasso::Resolver> Clone for ResolvedValue<'gc, R> {
    fn clone(&self) -> Self {
        Self {
            value: self.value,
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

enum ConsInner<'a, 'gc> {
    Cons(&'a ConsCell<'gc>),
    Vec(&'a Vector<'gc>),
}
// Handles printing possibly self-referential structures
struct ConsPrinter<'a, 'gc, K: lasso::Resolver> {
    cons: ConsInner<'a, 'gc>,
    resolver: Rc<K>,
    null_ptr: ValuePtr<'gc>,
    encountered: Rc<RefCell<Vec<Value<'gc>>>>,
}

impl<K: lasso::Resolver> fmt::Display for ConsPrinter<'_, '_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // recurse into the value, keeping track of encountered cons cells
        // so that we don't recurse into them
        let value = match self.cons {
            ConsInner::Cons(cons) => Value::Cons(*cons),
            ConsInner::Vec(vec) => Value::Vector(*vec),
        };

        if *self.null_ptr.borrow() == value {
            if self.encountered.borrow().is_empty() {
                // we are starting at the null cons, so close it out
                write!(f, "()")?;
            }
            return Ok(());
        }

        write!(
            f,
            "{}",
            if matches!(self.cons, ConsInner::Vec(_)) {
                "#("
            } else {
                "("
            }
        )?;

        if self.encountered.borrow().contains(&value) {
            // write as self-recursive list
            write!(f, "...)")?;
            return Ok(());
        }

        // handle car and cdr, adding a dot if cdr is *not* a cons cell
        self.encountered.borrow_mut().push(value);
        // we handle both
        match self.cons {
            ConsInner::Cons(cons) => {
                match cons.car.map(|p| *p.borrow()) {
                    Some(Value::Cons(car)) => todo!(),
                    Some(Value::Vector(cdr)) => todo!(),
                    Some(value) => {
                        // this value is *definitely* not self-referential, so it's ok to
                        // use ResolvedValue
                        write!(
                            f,
                            "{}",
                            ResolvedValue {
                                value,
                                null_ptr: self.null_ptr,
                                resolver: Rc::clone(&self.resolver)
                            }
                        )?;
                    }
                    None => {}
                };
                match cons.cdr.map(|p| *p.borrow()) {
                    Some(Value::Cons(cdr)) => todo!(),
                    Some(Value::Vector(cdr)) => todo!(),
                    Some(value) => {
                        // this value is *definitely* not self-referential, so it's ok to
                        // use ResolvedValue
                        write!(
                            f,
                            " . {}",
                            ResolvedValue {
                                value,
                                null_ptr: self.null_ptr,
                                resolver: Rc::clone(&self.resolver)
                            }
                        )?;
                    }
                    None => {}
                };
            }
            ConsInner::Vec(vec) => {
                for vptr in vec.vec.borrow().iter() {
                    match *vptr.borrow() {
                        Value::Cons(cdr) => todo!(),
                        Value::Vector(cdr) => todo!(),
                        value => {
                            todo!()
                        }
                    }
                }
            }
        }
        // pop encountered and close the list
        self.encountered.borrow_mut().pop();
        write!(f, ")")
    }
}

impl<K: lasso::Resolver> fmt::Display for ResolvedValue<'_, K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Undefined => write!(f, "#<undef>"),
            Value::Void => write!(f, "#<void>"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Inexact(fp) => write!(f, "{fp}"),
            Value::String(s) => write!(f, "\"{}\"", s.borrow().replace('\"', "\\\"")),
            Value::Symbol(sym) => write!(f, "'{}", self.resolver.resolve(&sym.0.into())),
            Value::Bool(b) => write!(f, "#{}", if b { "t" } else { "f" }),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Vector(ref vec) => {
                // write!(f, "#(")?;
                // for (idx, elem) in vec.vec.borrow().iter().copied().enumerate() {
                //     if idx != 0 {
                //         write!(f, " ")?;
                //     }
                //     write!(
                //         f,
                //         "{}",
                //         elem.borrow().resolve(self.resolver.clone(), self.null_ptr)
                //     )?;
                // }
                // write!(f, ")")?;
                // Ok(())
                write!(
                    f,
                    "{}",
                    ConsPrinter {
                        cons: ConsInner::Vec(vec),
                        resolver: self.resolver.clone(),
                        null_ptr: self.null_ptr,
                        encountered: Rc::new(RefCell::new(Vec::new())),
                    }
                )
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
                        cons: ConsInner::Cons(cons),
                        resolver: self.resolver.clone(),
                        null_ptr: self.null_ptr,
                        encountered: Rc::new(RefCell::new(Vec::new())),
                    }
                )
            }
            Value::Environment(_) => todo!(),
            Value::UserStruct(user) => {
                let label = user.label().unwrap_or("userdata");
                write!(f, "#<{label} {:p}>", &self.value)
            }
            // Value::Lambda(lambda) => write!(f, "<lambda {:p}>", *lambda.borrow()),
            Value::Lambda(lambda) => write!(f, "#<lambda {lambda:p}>"),
            Value::Continuation(cont) => write!(f, "#<continuation {cont}>"),
            Value::Error(e) => write!(f, "{}", e.display(self.resolver.as_ref())),
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

// TODO same as for Vector
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

// TODO Explore Clojure Immutable Vectors and
// Relaxed Radix Balanced Trees for the backing implementation
// (note that while these datatypes are immutable, they are immutable from
// Rust's perspective [using Gc w/o RefLock]. we can still have something like vector-set! "mutate"
// a value by changing what the `vec` pointer is pointing to)
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct Vector<'gc> {
    pub vec: Gc<'gc, RefLock<Vec<ValuePtr<'gc>>>>,
}

/// A bytecode chunk and program counter bundled together
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub enum Continuation<'gc> {
    /// Makes the calling from a bytecode frame with the thread state
    /// of the continuation source
    Continue {
        pc: usize,
        chunk: ChunkPtr<'gc>,
        #[collect(require_static)]
        arity: Arity,
        handler: Option<Lambda<'gc>>,
        args: Box<[ValuePtr<'gc>]>,
    },
    /// A null continuation causes the program to pop frames until it hits
    /// a native lambda frame
    Null,
}
pub type ContinuationPtr<'gc> = Gc<'gc, Continuation<'gc>>;

impl PartialEq for Continuation<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Continuation::Continue {
                    pc,
                    chunk,
                    arity,
                    handler,
                    args,
                },
                Continuation::Continue {
                    pc: opc,
                    chunk: ochunk,
                    arity: oarity,
                    handler: ohandler,
                    args: oargs,
                },
            ) => {
                pc == opc
                    && Gc::ptr_eq(*chunk, *ochunk)
                    && arity == oarity
                    && handler == ohandler
                    && args == oargs
            }
            (Continuation::Null, Continuation::Null) => true,
            _ => false,
        }
    }
}
impl Eq for Continuation<'_> {}

impl fmt::Display for Continuation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Continuation::Continue { pc, chunk, .. } => write!(f, "{chunk:p}@{pc}"),
            Continuation::Null => write!(f, "terminate"),
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

    /// Returns if a cons cell is circular (self-referential)
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
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

    /// Returns if a cons cell is a valid param list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
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
