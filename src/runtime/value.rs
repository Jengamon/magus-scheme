//! Representation of Scheme values

use core::fmt;
use std::marker::PhantomData;
use std::rc::Rc;
use std::string::String as StdString;

use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable, Static};
use lasso::IntoResolver;
use unicode_general_category::get_general_category;

use crate::environment::StackEnvironmentPtr;
use crate::interpreter::thread::ThreadFrame;
use crate::stdlib::base::ErrorObject;

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

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Values => write!(f, "values"),
            Self::Undefined => write!(f, "undefined"),
            Self::Void => write!(f, "void"),
            Self::Number => write!(f, "number"),
            Self::Inexact => write!(f, "inexact"),
            Self::String => write!(f, "string"),
            Self::Symbol => write!(f, "symbol"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::Vector => write!(f, "vector"),
            Self::Bytevector => write!(f, "bytevector"),
            Self::Record => write!(f, "record"),
            Self::InputPort => write!(f, "inputport"),
            Self::OutputPort => write!(f, "outputport"),
            Self::Cons => write!(f, "cons"),
            Self::Environment => write!(f, "environment"),
            Self::UserStruct => write!(f, "userstruct"),
            Self::Lambda => write!(f, "lambda"),
            Self::Continuation => write!(f, "continuation"),
            Self::Promise => write!(f, "promise"),
            Self::Parameter => write!(f, "parameter"),
            Self::Error => write!(f, "error"),
        }
    }
}

impl ValueType {
    /// Types that can contain ValuePtr, and thus can be self-recursive data structures
    pub fn can_recurse(self) -> bool {
        matches!(
            self,
            Self::Vector | Self::Record | Self::Cons | Self::Parameter | Self::Promise
        )
    }

    /// Types that do not contain a value
    pub fn unit_type(self) -> bool {
        matches!(self, Self::Void | Self::Undefined)
    }
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
    Parameter(ParameterPtr<'gc>),
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
            Value::Parameter(p) => matches!(other, Value::Parameter(op) if Gc::ptr_eq(p, op)),
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
                ..
            }) => {
                matches!(other, Value::Cons(ConsCell { car: Some(ocar), cdr: Some(ocdr), ..}) if Gc::ptr_eq(*car, *ocar) && Gc::ptr_eq(*cdr, *ocdr))
            }
            Value::Cons(ConsCell {
                car: None,
                cdr: Some(cdr),
                ..
            }) => {
                matches!(other, Value::Cons(ConsCell { car:None, cdr: Some(ocdr), ..}) if Gc::ptr_eq(*cdr, *ocdr))
            }
            Value::Cons(ConsCell {
                car: Some(car),
                cdr: None,
                ..
            }) => {
                matches!(other, Value::Cons(ConsCell { car: Some(ocar), cdr: None, ..}) if Gc::ptr_eq(*car, *ocar) )
            }
            // *technically* is wrong, but as long as there is no way to manufacture ConsCell w/ (None None),
            // this is essentially correct
            Value::Cons(ConsCell {
                car: None,
                cdr: None,
                ..
            }) => {
                matches!(
                    other,
                    Value::Cons(ConsCell {
                        car: None,
                        cdr: None,
                        ..
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
            Value::Parameter(p) => matches!(other, Value::Parameter(op) if Gc::ptr_eq(*p, *op)),
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

    #[expect(private_bounds)]
    pub fn resolve_into<K: lasso::Resolver, M: WriteMode + Collect<'gc>>(
        value_ptr: ValuePtr<'gc>,
        resolver: impl IntoResolver<Resolver = K> + 'static,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K, M> {
        Self::resolve(value_ptr, Rc::new(resolver.into_resolver()), null_ptr)
    }

    #[expect(private_bounds)]
    pub fn resolve<K: lasso::Resolver, M: WriteMode + Collect<'gc>>(
        value_ptr: ValuePtr<'gc>,
        resolver: Rc<K>,
        null_ptr: ValuePtr<'gc>,
    ) -> ResolvedValue<'gc, K, M> {
        ResolvedValue {
            value: *value_ptr.borrow(),
            value_ptr,
            null_ptr,
            resolver,
            _marker: PhantomData,
        }
    }
}

enum ConsInner<'a, 'gc> {
    Cons(&'a ConsCell<'gc>),
    Vec(&'a Vector<'gc>),
}
// Handles printing possibly self-referential structures
// TODO Accept a WriteMode
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
pub fn is_valid_unpiped_scheme_identifier(s: &str) -> bool {
    use unicode_general_category::GeneralCategory::*;
    // we don't allow Zl, Zp, Zs, Ps, Pi, Pf, Pe, Cs, Cf, Cc anywhere (except U+200C and U+200D)
    // nor Nd, Mc, Me initially
    if s.chars().any(|c| {
        matches!(
            get_general_category(c),
            Control
                | Format
                | Surrogate
                | ClosePunctuation
                | FinalPunctuation
                | InitialPunctuation
                | OpenPunctuation
                | LineSeparator
                | ParagraphSeparator
                | SpaceSeparator
        ) && !['\u{200d}', '\u{200c}'].contains(&c)
    }) {
        return false;
    } else if s.starts_with(|c: char| {
        matches!(
            get_general_category(c),
            DecimalNumber | SpacingMark | EnclosingMark
        )
    }) {
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

trait WriteMode {}
#[derive(Collect)]
#[collect(require_static)]
pub struct ModeWrite {}
impl WriteMode for ModeWrite {}
#[derive(Collect)]
#[collect(require_static)]
pub struct ModeDisplay {}
impl WriteMode for ModeDisplay {}
#[derive(Collect)]
#[collect(no_drop)]
// TODO Add a way to select "modes" (Display, Write, WriteShared(?), WriteSimple(?)) at *compile time* (typestate)
#[expect(private_bounds)]
pub struct ResolvedValue<'gc, R: lasso::Resolver, M: WriteMode + Collect<'gc> = ModeWrite> {
    value: Value<'gc>,
    value_ptr: ValuePtr<'gc>,
    null_ptr: ValuePtr<'gc>,
    #[collect(require_static)]
    resolver: Rc<R>,
    _marker: PhantomData<M>,
}

#[expect(private_bounds)]
impl<'gc, R: lasso::Resolver, M: WriteMode + Collect<'gc>> ResolvedValue<'gc, R, M> {
    pub fn value_ptr(&self) -> ValuePtr<'gc> {
        self.value_ptr
    }
}

impl<'gc, R: lasso::Resolver, M: WriteMode + Collect<'gc>> Clone for ResolvedValue<'gc, R, M> {
    fn clone(&self) -> Self {
        Self {
            value: self.value,
            value_ptr: self.value_ptr,
            null_ptr: self.null_ptr,
            resolver: self.resolver.clone(),
            _marker: PhantomData,
        }
    }
}

impl<'gc, K: lasso::Resolver, M: WriteMode + Collect<'gc>> fmt::Debug for ResolvedValue<'gc, K, M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedValue")
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl<K: lasso::Resolver> fmt::Display for ResolvedValue<'_, K, ModeWrite> {
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
            Value::Inexact(fp) if fp.fract() == 0.0 => write!(f, "{fp}."),
            Value::Inexact(fp) => write!(f, "{fp}"),
            Value::String(s) => write!(
                f,
                "\"{}\"",
                s.borrow()
                    .chars()
                    .flat_map(|c| escape_write_char(c, false))
                    .collect::<Box<str>>()
            ),
            Value::Symbol(sym)
                if is_valid_unpiped_scheme_identifier(self.resolver.resolve(&sym.0)) =>
            {
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
                        ResolvedValue::<K, ModeWrite> {
                            value: *elem.borrow(),
                            value_ptr: *elem,
                            null_ptr: self.null_ptr,
                            resolver: Rc::clone(&self.resolver),
                            _marker: PhantomData,
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
            Value::Cons(ref cons) if cons.is_circular(self.value_ptr, None) => {
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
                                ResolvedValue::<K, ModeWrite> {
                                    value: *car.borrow(),
                                    value_ptr: car,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver),
                                    _marker: PhantomData,
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
                                ResolvedValue::<K, ModeWrite> {
                                    value: *cdr.borrow(),
                                    value_ptr: cdr,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver),
                                    _marker: PhantomData,
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

impl<K: lasso::Resolver> fmt::Display for ResolvedValue<'_, K, ModeDisplay> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Values(v) => {
                write!(f, "#<values count={}>", v.len())
            }
            Value::Undefined => write!(f, "#<undef>"),
            Value::Void => write!(f, ""),
            Value::Number(n) => write!(f, "{n}"),
            Value::Inexact(fp) if fp.is_infinite() && fp.is_sign_negative() => write!(f, "-inf.0"),
            Value::Inexact(fp) if fp.is_infinite() => write!(f, "+inf.0"),
            Value::Inexact(fp) if fp.is_nan() => write!(f, "+nan.0"),
            Value::Inexact(fp) if fp.fract() == 0.0 => write!(f, "{fp}."),
            Value::Inexact(fp) => write!(f, "{fp}"),
            Value::String(s) => write!(f, "{}", s.borrow()),
            Value::Symbol(sym) => write!(f, "{}", self.resolver.resolve(&sym.0)),
            Value::Bool(b) => write!(f, "#{}", if b { "t" } else { "f" }),
            Value::Char(c) => write!(
                f,
                "{}",
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
                        ResolvedValue::<K, ModeDisplay> {
                            value: *elem.borrow(),
                            value_ptr: *elem,
                            null_ptr: self.null_ptr,
                            resolver: Rc::clone(&self.resolver),
                            _marker: PhantomData,
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
            Value::Cons(ref cons) if cons.is_circular(self.value_ptr, None) => {
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
                                ResolvedValue::<K, ModeDisplay> {
                                    value: *car.borrow(),
                                    value_ptr: car,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver),
                                    _marker: PhantomData,
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
                                ResolvedValue::<K, ModeDisplay> {
                                    value: *cdr.borrow(),
                                    value_ptr: cdr,
                                    null_ptr: self.null_ptr,
                                    resolver: Rc::clone(&self.resolver),
                                    _marker: PhantomData,
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
                if let Ok(s) = user.downcast::<Rootable![ErrorObject<'_>]>() {
                    write!(f, "{}", s.message.string.borrow())
                } else {
                    let label = user.label().unwrap_or("userdata");
                    write!(f, "#<{label} {:p}>", &self.value)
                }
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
            Value::Error(e) => write!(f, "{}", e.error_type),
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
                    if cell.is_circular(val, Some(stack)) {
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

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
/// A dynamically bound value location with a
/// default value, and possibly a conversion lambda
pub struct Parameter<'gc> {
    init: ValuePtr<'gc>,
    pub(crate) convert: Option<Lambda<'gc>>,
    // TODO an fxhash::FxHashMap of usize (which are the addresses of ThreadFrame::id) to ValuePtr
    // to find the value of a parameter we climb up the stack of frame in existence, and if not found *then* we use init
    // (to find the "raw value", it ofc can require a lambda to *actually* figure out)
    // the key is the address of a `ThreadFrame::id`
    frame_values: fxhash::FxHashMap<usize, ValuePtr<'gc>>,
}
pub type ParameterPtr<'gc> = Gc<'gc, RefLock<Parameter<'gc>>>;

impl<'gc> Parameter<'gc> {
    pub fn new(init: ValuePtr<'gc>) -> Self {
        Self {
            init,
            convert: None,
            frame_values: Default::default(),
        }
    }

    pub fn with_convert(init: ValuePtr<'gc>, convert: Lambda<'gc>) -> Self {
        Self {
            init,
            convert: Some(convert),
            frame_values: Default::default(),
        }
    }

    /// Parameterize a value for a given frame
    pub fn parameterize(&mut self, frame_id: Gc<()>, value: ValuePtr<'gc>) {
        self.frame_values
            .insert((&raw const *frame_id.as_ref()).addr(), value);
    }

    // Get the value of a parameter in the current dynamic context
    pub fn base_value(&self, frame_stack: &[Gc<()>]) -> ValuePtr<'gc> {
        // look up the frame ids in reverse, if one hits, then the value is the value for that frame, otherwise
        // the value is the init value
        let found_value = frame_stack.iter().rev().find_map(|idp| {
            let id = (&raw const *idp.as_ref()).addr();
            self.frame_values.get(&id).copied()
        });

        found_value.unwrap_or(self.init)
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
    pub fn new(car: Option<ValuePtr<'gc>>, cdr: Option<ValuePtr<'gc>>) -> Self {
        Self { car, cdr }
    }

    pub fn empty() -> Self {
        Self {
            car: None,
            cdr: None,
        }
    }

    /// Returns if a cons cell can look like a list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_circular(
        &self,
        self_ptr: ValuePtr<'gc>,
        addtl_stack: Option<&[ValuePtr<'gc>]>,
    ) -> bool {
        let mut stack = fxhash::FxHashSet::from_iter([(&raw const *self_ptr.borrow()).addr()]);

        if let Some(addtl_stack) = addtl_stack {
            stack.extend(
                addtl_stack
                    .iter()
                    .map(|ptr| (&raw const *ptr.borrow()).addr()),
            )
        }

        macro_rules! check_ptr {
            ($ptr:expr) => {{
                if stack.contains(&(&raw const *($ptr).borrow()).addr()) {
                    return true;
                }

                if ($ptr).borrow().value_type().can_recurse() {
                    stack.insert((&raw const *($ptr).borrow()).addr());
                }
            }};
        }

        let mut current = *self;
        while let Some(vp) = current.cdr {
            if let Some(car) = current.car {
                check_ptr!(car);
            }
            check_ptr!(vp);

            let Value::Cons(c) = *vp.borrow() else {
                return false;
            };
            current = c;
        }
        if let Some(car) = current.car {
            check_ptr!(car);
        }

        false
    }

    /// Returns if a cons cell can look like a list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_list(&self, self_ptr: ValuePtr<'gc>, null_ptr: ValuePtr<'gc>) -> bool {
        let mut stack = fxhash::FxHashSet::from_iter([(&raw const *self_ptr.borrow()).addr()]);

        let mut current = *self;
        while let Some(vp) = current.cdr {
            if Gc::ptr_eq(vp, null_ptr) {
                break;
            }

            if stack.contains(&(&raw const *vp.borrow()).addr()) {
                // Cyclical structure is *not* a list
                return false;
            }
            stack.insert((&raw const *vp.borrow()).addr());

            let Value::Cons(c) = *vp.borrow() else {
                return false;
            };
            current = c;
        }

        true
    }

    /// Returns if a cons cell can look like a list
    ///
    /// # Parameters
    /// - `self_ptr`: [`ValuePtr`] pointing to this [`ConsCell`]
    pub fn is_listable(&self, self_ptr: ValuePtr<'gc>, null_ptr: ValuePtr<'gc>) -> bool {
        self.cdr
            .is_none_or(|ptr| Gc::ptr_eq(null_ptr, ptr) || !Gc::ptr_eq(self_ptr, ptr))
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
            // We *know* we are a valid list, so use that info to optimize
            let mut list_values = vec![self.car.unwrap_or(null_ptr)];

            let mut current = *self;
            while let Some(cdr) = current.cdr {
                if Gc::ptr_eq(null_ptr, cdr) {
                    break;
                }

                let Value::Cons(c) = *cdr.borrow() else { break };

                list_values.push(c.car.unwrap_or(null_ptr));

                current = c;
            }

            list_values
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
