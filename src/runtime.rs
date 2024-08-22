//! runtimes are responsible for reading worlds and executing on its contents (relative to a specific program)

use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use gc_arena::{Collect, Gc, Mutation, RefLock};
use interpreter::Interpreter;
use lasso::Rodeo;

use crate::world::value::ValuePtr;

pub mod external;
pub mod interpreter;
pub mod scheme_base;

// First implement a treewalk interpreter that gets the sematics of Scheme correct, then
// start using this as we start a VM to make it faster (?).

/// Errors as held in the program
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub enum Error<'gc> {
    Value(ValuePtr<'gc>),
    Static(Gc<'gc, BoxError>),
}

#[derive(Collect, Clone, Debug)]
#[collect(require_static)]
pub struct BoxError(pub Rc<dyn std::error::Error>);

impl<E> From<E> for BoxError
where
    E: std::error::Error + 'static,
{
    fn from(value: E) -> Self {
        Self(Rc::new(value))
    }
}

// It is up to runtimes to implement procedures
#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub enum Procedure<'gc> {
    Code(Code<'gc>),
    Native(NativeProcedure),
}

impl<'gc> Procedure<'gc> {
    // report the arity of a procedure
    pub fn arity(&self) -> Arity {
        match self {
            Self::Code(code) => {
                _ = code;
                todo!("report arity for code blocks")
            }
            Self::Native(nat) => nat.0.borrow().arity(),
        }
    }
}

/*
    (+ (+ 1 2) 3)

    we should transform this into

    evaluate + -> a
    evaluate + -> b
    evaluate 1 -> c
    evaluate 2 -> d
    call top 3
    evaluate 3 -> e
    call top 3
    result last value on stack
*/

/// A list to be executed
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Code<'gc> {
    pub(crate) repr: Vec<ValuePtr<'gc>>,
    // TODO Frame design to be able to point to any valid representation within repr
    pub(crate) pc: usize,
    // TODO Store arity information
    // for now, just using Arity
    pub(crate) arity: Arity,
    /*
        we use a frames and jumps addressing system:
        elements in each list are enumerated, and then
        the length of each list is calculated, so
        ( + (+ x y) z w)
        as these numbers
        ( 0 (1 2 3) 4 5)
        and jumps are recorded where lists end with a count to consume, so
        the jumps for this are (3, 3) (4, 4) meaning after pushing the value of
        y to the stack, evaluate the top 3 items (bottom is always a procedure if correct)
        then after evaluating w evaluate the top 4 items
    */
}

// TODO Display impl

impl<'gc> Code<'gc> {}

#[derive(Collect, Clone)]
#[collect(require_static)]
pub struct NativeProcedure(Rc<RefCell<dyn Callback>>);
impl<T: Callback + 'static> From<T> for NativeProcedure {
    fn from(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

impl fmt::Debug for NativeProcedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.0.borrow().name() {
            write!(f, "#<procedure {name} {:p}>", self.0)
        } else {
            write!(f, "#<procedure {:p}>", self.0)
        }
    }
}

// TODO Rework this as we have to make this interruptable b/c
// fuel can run out at any time. but we *only* make it interruptable
// NOTE Error mechanism? Not natively, entirely handled by the Interpreter.
// Native code can *raise* an error tho.

#[derive(Collect, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Environment<'gc> {
    parent: Option<Gc<'gc, RefLock<Environment<'gc>>>>,
    inner: Gc<'gc, RefLock<EnvironmentInner<'gc>>>,
}

impl<'gc> Environment<'gc> {}

/// Environemnts define the context for execution, with variable mappings
/// macro definitions, and up to 1 reference to a parent environment
#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct EnvironmentInner<'gc> {
    pub values: HashMap<Box<str>, ValuePtr<'gc>>,
}

/// Nice argument handler
pub struct Arguments<'gc>(pub Vec<ValuePtr<'gc>>);
impl<'gc> Arguments<'gc> {
    /// Creates an error if the arity does not match
    pub fn force_arity<const N: usize>(self) -> Result<[ValuePtr<'gc>; N], BoxError> {
        #[derive(thiserror::Error, Debug)]
        #[error("arity mismatch: found {0} arguments, expected {N}")]
        struct ArityMismatch<const N: usize>(usize);

        let arg_len = self.0.len();
        TryInto::<[ValuePtr<'gc>; N]>::try_into(self.0)
            .map_err(|_| BoxError::from(ArityMismatch::<N>(arg_len)))
    }
}

/// Procedure arity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Collect)]
#[collect(require_static)]
pub enum Arity {
    Exact(usize),
    Min(usize),
}

impl Arity {
    pub fn is_satisfied(&self, len: usize) -> bool {
        match self {
            Self::Exact(e) => *e == len,
            Self::Min(m) => *m <= len,
        }
    }
}

// macros interact with code before it is evaluated,
// and have a fresh environment for executing the code it generates
// (they *have* to be hygenic, how do we do?)
// (maybe macros can "insert" an environment and evaluate code in different contexts?)
// NOTE hygenic macros means that macros are always evaluated in the context they are
// defined in (see README for notes)
// (we maybe can think of macros as functions that operate on lists)
// callback are done after and thus can interact with its surrounding environment

/// A native callback is quite simple: it must map from
/// a Vec<Value<'gc>> -> Result<Value<'gc>, Error<'gc>> in essence
// lovely thing about scheme: Native code cannot make calls, they can only return a callable
// the other code might call, or not.
// Callbacks are essentially CPS procedures

pub enum CallbackSuccess<'gc> {
    /// Return a given value
    Return(ValuePtr<'gc>),
    /// Evaluate a given value in a given environment as the return value of this
    /// function.
    Eval(ValuePtr<'gc>, Option<()>),
}
type CallbackResult<'gc> = Result<CallbackSuccess<'gc>, Error<'gc>>;

pub trait Callback {
    fn name(&self) -> Option<&str> {
        None
    }

    fn arity(&self) -> Arity;

    fn call<'gc>(
        &mut self,
        mc: &Mutation<'gc>,
        interner: &mut Rodeo,
        // continuation: Gc<'gc, Procedure>,
        // give access to the continuation *and*
        // environment (and possibly setting stuff in the environment)
        // b/c of this, fuel manipulation/interrupting
        // and changing the continuation can be handled by the interpreter's
        // public interface
        interpreter: &'gc Interpreter<'gc>,
        // args is guranteed to respect arity
        args: Arguments<'gc>,
        // None for second return means to not chenge the current continuation!
    ) -> CallbackResult<'gc>;
}

/*
so that:
pub struct CallWithCurrentContinuation;
impl Callback for CallWithCurrentContinuation {
    fn name(&self) -> Option<&str> { Some("call-with-current-continuation") }
    fn arity(&self) -> Arity { Arity::Exact(1) }
    fn call<'gc>(
        &mut self,
        environment: Environment<'gc>,
        interner: &mut Rodeo,
        continuation: Gc<'gc, Continuation>,
        fuel: &mut Fuel,
        args: Arguments<'gc>,
    ) -> Result<(CallbackResult<'gc>, Option<Continuation>), Error<'gc>> {
        #[derive(thiserror::Error, Debug)]
        enum CallCcError {
            #[error("cannot call a non-procedure")]
            NoProc,
            #[error("procedure must have exactly 1 argument")]
            ProcArgs,
        }

        let [Value::Procedure(proc)] = args.force_arity::<1>()? else {
            return Err(CallCcError::NoProc)?;
        };

        // the proc must have exactly 1 argument: the continuation
        if proc.arity() == Arity::Exact(1) {
            Ok((CallbackReturn::Value(Value::Procedure(
continuation.procedure()

            )), proc.into()))
        } else {
            Err(CallCcError::ProcArgs)?
        }
    }
}*/
