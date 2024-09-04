//! runtimes are responsible for reading worlds and executing on its contents (relative to a specific program)

use core::fmt;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::Rodeo;

use crate::world::{
    value::{Value, ValuePtr},
    WorldArena,
};

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
    Code(Lambda<'gc>),
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

#[derive(Collect, Debug, Clone, Copy, PartialEq, Eq)]
#[collect(require_static)]
pub(crate) struct InternedFilename(pub(crate) lasso::Spur);

/// A list to be executed
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Lambda<'gc> {
    pub(crate) repr: Vec<ValuePtr<'gc>>,
    // TODO Store arity information
    // for now, just using Arity
    pub(crate) arity: Arity,
    // list the source(s) of this lambda
    pub(crate) sources: Box<(InternedFilename, (usize, usize))>,
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

impl<'gc> Lambda<'gc> {}

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

// TODO make more involved, so that this can be the type stored in Value
// for `environment`
// TODO Allow for "frozen" environments to implement `environment` b/c
// "The bindings of the environment represented by the specifier are immutable, as is the environment itself."
// It should interact with set!, define, and friends to prevent any modification of the environment.
/// Environemnts define the context for execution, with variable mappings
/// macro definitions, and up to 1 reference to a parent environment
#[derive(Collect, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Environment<'gc> {
    parent: Option<EnvironmentPtr<'gc>>,
    inner: Gc<'gc, RefLock<EnvironmentInner<'gc>>>,
    /// This will make all [`Self::define`]s fail as it makes the
    /// bindings immutable
    is_frozen: bool,
}
pub type EnvironmentPtr<'gc> = Gc<'gc, RefLock<Environment<'gc>>>;

#[derive(thiserror::Error, Debug)]
#[error("environment is frozen")]
pub struct FrozenError;

impl<'gc> Environment<'gc> {
    pub fn new(mc: &Mutation<'gc>, parent: Option<EnvironmentPtr<'gc>>) -> Self {
        Self {
            parent,
            inner: Gc::new(
                mc,
                RefLock::new(EnvironmentInner {
                    values: HashMap::default(),
                }),
            ),
            is_frozen: false,
        }
    }

    /// Sets the frozen flag. This is not a reversible operation.
    pub fn freeze(&mut self, mc: &Mutation<'gc>) {
        self.is_frozen = true;
        for binding in self.inner.borrow_mut(mc).values.values_mut() {
            binding.is_frozen = true;
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Binding<'gc>> {
        if let Some(value) = self.inner.borrow().values.get(name.as_ref()) {
            Some(*value)
        } else if let Some(parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    /// Creates a new binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment the definition is attempted in is frozen
    pub fn define(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl AsRef<str>,
        value: ValuePtr<'gc>,
        is_frozen: bool,
    ) -> Result<Option<Binding<'gc>>, FrozenError> {
        self.is_frozen
            .then(|| {
                self.inner
                    .borrow_mut(mc)
                    .values
                    .insert(Box::from(name.as_ref()), Binding { value, is_frozen })
            })
            .ok_or(FrozenError)
    }
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
struct EnvironmentInner<'gc> {
    pub values: HashMap<Box<str>, Binding<'gc>>,
}

/// Represents the value at a certain location in an environment.
///
/// A particular binding can be frozen to make sure that no change of its held value
/// is made through it.
#[derive(Collect, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Binding<'gc> {
    value: ValuePtr<'gc>,
    is_frozen: bool,
}

impl<'gc> Binding<'gc> {
    pub fn read<T>(&self, func: impl FnOnce(Ref<Value<'gc>>) -> T) -> T {
        func(self.value.borrow())
    }

    pub fn write<T>(
        &self,
        mc: &Mutation<'gc>,
        func: impl FnOnce(RefMut<Value<'gc>>) -> T,
    ) -> Option<T> {
        if self.is_frozen {
            None
        } else {
            Some(func(self.value.borrow_mut(mc)))
        }
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

pub enum CallbackReturn<'gc> {
    /// Suspend this call, and return to the main loop
    Suspend,
    /// Return the value at the top of the stack
    Return,
    /// Evaluate the top of the stack in the given environment
    /// and push its return value to our stack
    Eval(Option<EnvironmentPtr<'gc>>),
    /// Make the top of the stack the current continuation,
    /// with the previous continuation as its argument
    Continuation,
}
type CallbackResult<'gc> = Result<CallbackReturn<'gc>, Error<'gc>>;

// TODO Prolly make this more Sequence-like...
// so that they can freely call back into Interpreter code...
// this is done by providing a stack, and having returns simply indicate actions
// to make
pub trait Callback {
    fn name(&self) -> Option<&str> {
        None
    }

    fn arity(&self) -> Arity;

    fn call<'gc>(
        &mut self,
        mc: &Mutation<'gc>,
        arena: &WorldArena,
        rodeo: &mut Rodeo,
        // continuation: Gc<'gc, Procedure>,
        // give access to the continuation *and*
        // environment (and possibly setting stuff in the environment)
        // b/c of this, fuel manipulation/interrupting
        // and changing the continuation can be handled by the interpreter's
        // public interface
        // interpreter: &'gc Interpreter<'gc>,
        // stack starts with the arguments on it
        stack: &'gc mut Vec<ValuePtr<'gc>>,
    ) -> CallbackResult<'gc>;
}

#[derive(Collect)]
#[collect(no_drop)]
pub enum MacroReturn<'gc> {
    /// Evaluate a value in the environment of this macro's invocation,
    /// or a given environment, then return to this macro
    Eval(Option<EnvironmentPtr<'gc>>),
    /// Rewrite this macro's invocation into the value at the top of
    /// the stack
    Rewrite,
}

pub trait Macro {
    fn rewrite<'gc>(
        &mut self,
        mc: &Mutation<'gc>,
        arena: &WorldArena,
        rodeo: &mut Rodeo,
        // environment this macro is being invoked in
        environment: EnvironmentPtr<'gc>,
        // starts with unevaluated inputs
        stack: &'gc mut Vec<ValuePtr<'gc>>,
    ) -> Result<MacroReturn<'gc>, Error<'gc>>;
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
