// Lambdas are procedures that include type information that is checked at runtime

use core::fmt;
use std::rc::Rc;

use gc_arena::{unsize, Collect, Gc, Mutation, RefLock};

use crate::Fuel;

use super::{FromValue, IntoValue, ValuePtr, ValueType};

/// A filter for types
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct Typecheck {
    filter: TypeFilter,
}
pub type TypeFilter = Rc<dyn Fn(&[ValueType]) -> anyhow::Result<()>>;

impl Typecheck {
    pub fn new(filter: impl Fn(&[ValueType]) -> anyhow::Result<()> + 'static) -> Self {
        Self {
            filter: Rc::new(filter),
        }
    }

    pub fn check(&self, signature: &[ValueType]) -> anyhow::Result<()> {
        (self.filter)(signature)
    }
}

impl<F> From<F> for Typecheck
where
    F: Fn(&[ValueType]) -> anyhow::Result<()> + 'static,
{
    fn from(value: F) -> Self {
        Self::new(value)
    }
}

struct FilterDebug<'a>(&'a TypeFilter);
impl<'a> fmt::Debug for FilterDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<type filter {:p}>", self.0)
    }
}

impl fmt::Debug for Typecheck {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Typecheck")
            .field("filter", &FilterDebug(&self.filter))
            .finish()
    }
}

/// Stages of a lambda call
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub enum LambdaStage {
    /// we have to typecheck the input stack
    Typecheck,
    /// executing the lambda, the stack freely changes here
    Execution,
}

// Access to data on a stack, local to an evaluation
// also has the bookkeeping for this lambda invocation
#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct LambdaCall<'gc> {
    pub stack: Vec<ValuePtr<'gc>>,
    stage: LambdaStage,

    lambda_id: Gc<'gc, ()>,
}

impl<'gc> LambdaCall<'gc> {
    pub fn stage(&self) -> LambdaStage {
        self.stage
    }

    pub fn push(&mut self, mc: &Mutation<'gc>, value: impl IntoValue<'gc>) {
        self.stack
            .push(Gc::new(mc, RefLock::new(value.into_value(mc))))
    }

    pub fn pop<V: FromValue<'gc>>(&mut self) -> Option<V> {
        self.stack.pop().and_then(|vp| V::from_value(*vp.borrow()))
    }

    // TODO, consume the stack and convert the value ptr to the expected types
    pub fn consume(&mut self) -> () {}
}

// TODO Error handling
pub enum ProcedureReturn<'gc> {
    /// Call the given value as code.
    ///
    /// If `is_tail` is set, it is called as a tailcall, and this procedure will have returned.
    /// If not, the result of the call will be pushed to the top of the stack
    Call { code: ValuePtr<'gc>, is_tail: bool },

    /// This procedure has ended.
    ///
    /// Return the last value on the stack to the caller (or void if there is nothing)
    Return,
}

#[derive(Debug, thiserror::Error)]
pub enum ProcedureError<'gc> {
    #[error("raised value: {0:?}")]
    Value(ValuePtr<'gc>),
    #[error(transparent)]
    General(#[from] anyhow::Error),
}

pub type ProcedureResult<'gc> = Result<ProcedureReturn<'gc>, ProcedureError<'gc>>;
pub trait Procedure<'gc>: Collect {
    fn execute(
        &mut self,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        fuel: &mut Fuel,
    ) -> ProcedureResult<'gc>;
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Callback<R, F> {
    root: R,
    #[collect(require_static)]
    func: F,
}

impl<'gc, R, F> Procedure<'gc> for Callback<R, F>
where
    R: 'gc + Collect,
    F: 'static
        + FnMut(&mut R, &Mutation<'gc>, &mut LambdaCall<'gc>, &mut Fuel) -> ProcedureResult<'gc>,
{
    fn execute(
        &mut self,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        fuel: &mut Fuel,
    ) -> ProcedureResult<'gc> {
        (self.func)(&mut self.root, mc, call, fuel)
    }
}

impl<F> Callback<(), F>
where
    F: for<'gc> FnMut(
            &mut (),
            &Mutation<'gc>,
            &mut LambdaCall<'gc>,
            &mut Fuel,
        ) -> ProcedureResult<'gc>
        + 'static,
{
    fn from_fn<'gc>(mc: &Mutation<'gc>, func: F) -> ProcedurePtr<'gc> {
        let cb = Gc::new(mc, RefLock::new(Self { root: (), func }));
        unsize!(cb => RefLock<dyn Procedure<'gc>>)
    }
}

impl<'gc, R, F> Callback<R, F>
where
    R: Collect + 'gc,
    F: 'static
        + FnMut(&mut R, &Mutation<'gc>, &mut LambdaCall<'gc>, &mut Fuel) -> ProcedureResult<'gc>,
{
    fn from_fn_with(mc: &Mutation<'gc>, root: R, func: F) -> ProcedurePtr<'gc> {
        let cb = Gc::new(mc, RefLock::new(Self { root, func }));
        unsize!(cb => RefLock<dyn Procedure<'gc>>)
    }
}

// Centerpiece, produces typecheck info, and runs the lambda
// We *must* take notes from piccolo Sequences, so that these are
// interruptable and smoothly integrate with any executor
#[derive(Clone, Collect)]
#[collect(no_drop)]
pub struct Lambda<'gc> {
    typecheck: Option<Typecheck>,
    proc: ProcedurePtr<'gc>,

    // This identifes *this* lambda, as opposed to a different one
    id: Gc<'gc, ()>,
}
type ProcedurePtr<'gc> = Gc<'gc, RefLock<dyn Procedure<'gc> + 'gc>>;
pub type LambdaPtr<'gc> = Gc<'gc, RefLock<Lambda<'gc>>>;

/// Errors in executing the lambda
#[derive(thiserror::Error, Debug)]
pub enum LambdaExecError<'gc> {
    #[error("lambda mismatch: got {call_id:p}, was {id:p}")]
    LambdaMismatch {
        id: Gc<'gc, ()>,
        call_id: Gc<'gc, ()>,
    },
    #[error("typecheck failed: {0}")]
    TypecheckFailure(#[from] anyhow::Error),
    #[error(transparent)]
    ProcedureError(ProcedureError<'gc>),
}

impl<'gc> Lambda<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        proc: impl for<'gca> FnMut(
                &mut (),
                &Mutation<'gca>,
                &mut LambdaCall<'gca>,
                &mut Fuel,
            ) -> ProcedureResult<'gca>
            + 'static,
    ) -> Self {
        Self {
            typecheck: None,
            proc: Callback::from_fn(mc, proc),
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_root<R: Collect + 'gc>(
        mc: &Mutation<'gc>,
        root: R,
        proc: impl for<'gca> FnMut(
                &mut R,
                &Mutation<'gca>,
                &mut LambdaCall<'gca>,
                &mut Fuel,
            ) -> ProcedureResult<'gca>
            + 'static,
    ) -> Self {
        Self {
            typecheck: None,
            proc: Callback::from_fn_with(mc, root, proc),
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_typecheck(
        mc: &Mutation<'gc>,
        typecheck: Typecheck,
        proc: impl for<'gca> FnMut(
                &mut (),
                &Mutation<'gca>,
                &mut LambdaCall<'gca>,
                &mut Fuel,
            ) -> ProcedureResult<'gca>
            + 'static,
    ) -> Self {
        Self {
            typecheck: Some(typecheck),
            proc: Callback::from_fn(mc, proc),
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_root_typecheck<R: Collect + 'gc>(
        mc: &'gc Mutation<'gc>,
        typecheck: Typecheck,
        root: R,
        proc: impl FnMut(&mut R, &Mutation<'gc>, &mut LambdaCall<'gc>, &mut Fuel) -> ProcedureResult<'gc>
            + 'static,
    ) -> Lambda<'gc> {
        Self {
            typecheck: Some(typecheck),
            proc: Callback::from_fn_with(mc, root, proc),
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_procedure<P>(mc: &Mutation<'gc>, proc: P) -> Self
    where
        P: for<'gca> Procedure<'gca> + 'static,
    {
        Self {
            typecheck: None,
            proc: unsize!(Gc::new(mc, RefLock::new(proc)) => RefLock<dyn Procedure<'gc>>),
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_typecheck_procedure<P>(mc: &Mutation<'gc>, typecheck: Typecheck, proc: P) -> Self
    where
        P: for<'gca> Procedure<'gca> + 'static,
    {
        Self {
            typecheck: Some(typecheck),
            proc: unsize!(Gc::new(mc, RefLock::new(proc)) => RefLock<dyn Procedure<'gc>>),
            id: Gc::new(mc, ()),
        }
    }

    /// Create a new lambda call for this lambda
    pub fn call(&self, initial_stack: impl IntoIterator<Item = ValuePtr<'gc>>) -> LambdaCall<'gc> {
        LambdaCall {
            stack: initial_stack.into_iter().collect(),
            stage: LambdaStage::Typecheck,
            lambda_id: self.id,
        }
    }

    /// Execute a lambda
    ///
    /// This will fail if the id of the given call does not match with
    /// this lambda
    pub fn execute(
        &mut self,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        fuel: &mut Fuel,
    ) -> Result<ProcedureReturn<'gc>, LambdaExecError<'gc>> {
        if !Gc::ptr_eq(self.id, call.lambda_id) {
            // the call was not produced by this lambda
            return Err(LambdaExecError::LambdaMismatch {
                id: self.id,
                call_id: call.lambda_id,
            });
        }

        // check the stage, and do the thing accordingly
        loop {
            match call.stage {
                LambdaStage::Typecheck => {
                    let signature: Vec<_> =
                        call.stack.iter().map(|v| v.borrow().value_type()).collect();
                    if let Some(check) = self.typecheck.as_ref() {
                        if let Err(e) = check.check(&signature) {
                            eprintln!("typecheck error: {e}");
                            return Err(LambdaExecError::TypecheckFailure(e));
                        }
                    }
                    call.stage = LambdaStage::Execution;
                }
                LambdaStage::Execution => {
                    break Ok(self
                        .proc
                        .borrow_mut(mc)
                        .execute(mc, call, fuel)
                        .map_err(LambdaExecError::ProcedureError)?)
                }
            }
        }
    }
}

struct ProcDebug<'a>(ProcedurePtr<'a>);
impl<'a> fmt::Debug for ProcDebug<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<proc {:p}>", &self.0.borrow())
    }
}
impl<'gc> fmt::Debug for Lambda<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lambda")
            .field("typecheck", &self.typecheck)
            .field("proc", &ProcDebug(self.proc))
            .finish()
    }
}
