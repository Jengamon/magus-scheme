// Lambdas are procedures that include type information that is checked at runtime

use core::fmt;
use std::rc::Rc;

use gc_arena::{unsize, Collect, Gc, Mutation, RefLock};
use rowan::TextRange;

use crate::treewalk::{StackValue, TreewalkExecutor};
use crate::Fuel;

use crate::runtime::{
    convert::{FromValue, IntoValue},
    error::SchemeErrorPtr,
};
use crate::value::{ValuePtr, ValueType};

/// A filter for types
// TODO Make Typecheck design mirror Callback and Procedure, so that a
// type check can hold Gc data
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
    /// if in an error context, this is the stage of the lambda
    Erroring { is_typechecked: bool },
}

// Access to data on a stack, local to an evaluation
// also has the bookkeeping for this lambda invocation
/// TODO Introduce a call context, which wraps the call, interpreter, and fuel borrows in a neat package (maybe)
#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub struct LambdaCall<'gc> {
    /// TODO use StackValue
    pub stack: Vec<StackValue<'gc>>,
    args: usize,
    stage: LambdaStage,

    lambda_id: Gc<'gc, ()>,
    // TODO Hold source information about the lambda so that
    // stack values can be synthesized
    #[collect(require_static)]
    range: Option<TextRange>,
    source_id: Option<usize>,
}

impl<'gc> LambdaCall<'gc> {
    pub fn stage(&self) -> LambdaStage {
        self.stage
    }

    /// Get the number of arguments passed into this lambda call
    pub fn args(&self) -> usize {
        self.args
    }

    pub fn push(&mut self, mc: &Mutation<'gc>, value: impl IntoValue<'gc>) {
        // self.range is the range of code from which this call was created
        let new_value = StackValue {
            value: Gc::new(mc, RefLock::new(value.into_value(mc))),
            range: self.range,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
            source_id: self.source_id,
        };
        self.stack.push(new_value)
    }

    /// TODO Work with StackValues
    pub fn pop<V: FromValue<'gc>>(&mut self) -> Result<V, Option<StackValue<'gc>>> {
        let ptr = self.stack.pop();
        ptr.and_then(|vp| V::from_value(*vp.borrow())).ok_or(ptr)
    }
}

impl<'gc> fmt::Display for LambdaCall<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<lambda call {:p} [stack len {}]>",
            self.lambda_id,
            self.stack.len()
        )
    }
}

// TODO Error handling
pub enum ProcedureReturn<'gc> {
    /// Call the given value as code.
    ///
    /// If `is_tail` is set, it is called as a tailcall, and this procedure will have returned.
    /// If not, the result of the call will be pushed to the top of the stack
    Call {
        code: StackValue<'gc>,
        is_tail: bool,
    },

    /// This procedure has ended.
    ///
    /// Return the last value on the stack to the caller (or void if there is nothing)
    Return,

    /// The procedure is in progress, so return to it
    Suspend,
}

#[derive(Debug, thiserror::Error)]
pub enum ProcedureError<'gc> {
    #[error("raised value: {0:?}")]
    Value(ValuePtr<'gc>),
    #[error(transparent)]
    General(#[from] anyhow::Error),
    #[error("propagated Scheme error: {0:?}")]
    Scheme(SchemeErrorPtr<'gc>),
}

pub type ProcedureResult<'gc> = Result<ProcedureReturn<'gc>, ProcedureError<'gc>>;
pub trait Procedure<'gc>: Collect {
    fn execute(
        &mut self,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        interpreter: &mut TreewalkExecutor<'gc>,
        fuel: &mut Fuel,
    ) -> ProcedureResult<'gc>;

    /// Error pathway
    ///
    /// By default, simply propages the error
    fn error(
        &mut self,
        error: SchemeErrorPtr<'gc>,
        _mc: &Mutation<'gc>,
        _call: &mut LambdaCall<'gc>,
        _interpreter: &mut TreewalkExecutor<'gc>,
        _fuel: &mut Fuel,
    ) -> ProcedureResult<'gc> {
        Err(ProcedureError::Scheme(error))
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct Callback<R, F> {
    root: R,
    #[collect(require_static)]
    func: F,
}

impl<'gc, R, F> Procedure<'gc> for Callback<R, F>
where
    R: 'gc + Collect,
    F: 'static
        + FnMut(
            &mut R,
            &Mutation<'gc>,
            &mut LambdaCall<'gc>,
            &mut TreewalkExecutor<'gc>,
            &mut Fuel,
        ) -> ProcedureResult<'gc>,
{
    fn execute(
        &mut self,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        interpreter: &mut TreewalkExecutor<'gc>,
        fuel: &mut Fuel,
    ) -> ProcedureResult<'gc> {
        (self.func)(&mut self.root, mc, call, interpreter, fuel)
    }
}

impl<F> Callback<(), F>
where
    F: for<'gc> FnMut(
            &mut (),
            &Mutation<'gc>,
            &mut LambdaCall<'gc>,
            &mut TreewalkExecutor<'gc>,
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
        + FnMut(
            &mut R,
            &Mutation<'gc>,
            &mut LambdaCall<'gc>,
            &mut TreewalkExecutor<'gc>,
            &mut Fuel,
        ) -> ProcedureResult<'gc>,
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
                &mut TreewalkExecutor<'gca>,
                &mut Fuel,
            ) -> ProcedureResult<'gca>
            + 'static,
    ) -> Self {
        Self::with_procedure_ptr(mc, Callback::from_fn(mc, proc))
    }

    pub fn with_root<R: Collect + 'gc>(
        mc: &Mutation<'gc>,
        root: R,
        proc: impl FnMut(
                &mut R,
                &Mutation<'gc>,
                &mut LambdaCall<'gc>,
                &mut TreewalkExecutor<'gc>,
                &mut Fuel,
            ) -> ProcedureResult<'gc>
            + 'static,
    ) -> Self {
        Self::with_procedure_ptr(mc, Callback::from_fn_with(mc, root, proc))
    }

    pub fn with_typecheck(
        mc: &Mutation<'gc>,
        typecheck: Typecheck,
        proc: impl for<'gca> FnMut(
                &mut (),
                &Mutation<'gca>,
                &mut LambdaCall<'gca>,
                &mut TreewalkExecutor<'gca>,
                &mut Fuel,
            ) -> ProcedureResult<'gca>
            + 'static,
    ) -> Self {
        Self::with_typecheck_procedure_ptr(mc, typecheck, Callback::from_fn(mc, proc))
    }

    pub fn with_root_typecheck<R: Collect + 'gc>(
        mc: &'gc Mutation<'gc>,
        typecheck: Typecheck,
        root: R,
        proc: impl FnMut(
                &mut R,
                &Mutation<'gc>,
                &mut LambdaCall<'gc>,
                &mut TreewalkExecutor<'gc>,
                &mut Fuel,
            ) -> ProcedureResult<'gc>
            + 'static,
    ) -> Lambda<'gc> {
        Self::with_typecheck_procedure_ptr(mc, typecheck, Callback::from_fn_with(mc, root, proc))
    }

    pub fn with_procedure<P>(mc: &Mutation<'gc>, proc: P) -> Self
    where
        P: Procedure<'gc> + 'gc,
    {
        Self::with_procedure_ptr(
            mc,
            unsize!(Gc::new(mc, RefLock::new(proc)) => RefLock<dyn Procedure<'gc>>),
        )
    }

    pub fn with_typecheck_procedure<P>(mc: &Mutation<'gc>, typecheck: Typecheck, proc: P) -> Self
    where
        P: Procedure<'gc> + 'gc,
    {
        Self::with_typecheck_procedure_ptr(
            mc,
            typecheck,
            unsize!(Gc::new(mc, RefLock::new(proc)) => RefLock<dyn Procedure<'gc>>),
        )
    }

    pub fn with_procedure_ptr(mc: &Mutation<'gc>, proc: ProcedurePtr<'gc>) -> Self {
        Self {
            typecheck: None,
            proc,
            id: Gc::new(mc, ()),
        }
    }

    pub fn with_typecheck_procedure_ptr(
        mc: &Mutation<'gc>,
        typecheck: Typecheck,
        proc: ProcedurePtr<'gc>,
    ) -> Self {
        Self {
            typecheck: Some(typecheck),
            proc,
            id: Gc::new(mc, ()),
        }
    }

    /// Create a new lambda call for this lambda
    pub fn call(
        &self,
        initial_stack: impl IntoIterator<Item = StackValue<'gc>>,
        source_id: Option<usize>,
        range: Option<TextRange>,
    ) -> LambdaCall<'gc> {
        let stack: Vec<_> = initial_stack.into_iter().collect();
        LambdaCall {
            args: stack.len(),
            stack,
            stage: LambdaStage::Typecheck,
            lambda_id: self.id,
            source_id,
            range,
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
        interpreter: &mut TreewalkExecutor<'gc>,
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
                        .execute(mc, call, interpreter, fuel)
                        .map_err(LambdaExecError::ProcedureError)?)
                }
                LambdaStage::Erroring { is_typechecked } => {
                    // we were on the error pathway, so restore the stage then continue on
                    if is_typechecked {
                        call.stage = LambdaStage::Execution;
                    } else {
                        call.stage = LambdaStage::Typecheck;
                    }
                }
            }
        }
    }

    /// Execute a lambda on the error pathway
    ///
    /// This will fail if the id of the given call does not match with
    /// this lambda
    ///
    /// This skips typechecking for that lambda, and sets the stage to erroring
    pub fn execute_erroring(
        &mut self,
        error: SchemeErrorPtr<'gc>,
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        interpreter: &mut TreewalkExecutor<'gc>,
        fuel: &mut Fuel,
    ) -> Result<ProcedureReturn<'gc>, LambdaExecError<'gc>> {
        if !Gc::ptr_eq(self.id, call.lambda_id) {
            // the call was not produced by this lambda
            return Err(LambdaExecError::LambdaMismatch {
                id: self.id,
                call_id: call.lambda_id,
            });
        }

        // Force stage into erroring (if not already)
        match call.stage {
            LambdaStage::Typecheck => {
                // typechecking hasn't occured yet
                call.stage = LambdaStage::Erroring {
                    is_typechecked: false,
                };
            }
            LambdaStage::Execution => {
                // typechecking has already occured
                call.stage = LambdaStage::Erroring {
                    is_typechecked: true,
                };
            }
            LambdaStage::Erroring { .. } => {}
        };

        // call into the error pathway
        self.proc
            .borrow_mut(mc)
            .error(error, mc, call, interpreter, fuel)
            .map_err(LambdaExecError::ProcedureError)
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

impl<'gc> fmt::Pointer for Lambda<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.id)
    }
}
