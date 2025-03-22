use gc_arena::{Collect, Gc, Mutation, RefLock, Static};

use crate::{
    Fuel, Value, ValueType,
    bytecode::{Bytecode, ChunkPtr, ImportFallback, SourceData},
    compiler::World,
    environment::{StackEnvironment, StackEnvironmentPtr},
    runtime::{
        convert::IntoValue,
        error::{SchemeError, SchemeErrorPtr, SchemeErrorType, StackFrame},
        lambda::{
            Arity, DynamicWind, Lambda, LambdaError, LambdaReturn, NativeLambdaContext,
            NativeLambdaPtr,
        },
    },
    value::{self, ConsCell, Continuation, ContinuationPtr, ValuePtr},
};

use super::{Context, Includer};

/// A thread's execution state can either be interpreting bytecode or running a native function or macro
#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
pub enum Execution<'gc> {
    Bytecode {
        chunk: ChunkPtr<'gc>,
        #[collect(require_static)]
        arity: Arity,
        pc: usize,
        upvalue_index: Option<usize>,
        fallback: ImportFallback<'gc>,
    },
    Native {
        native: NativeLambdaPtr<'gc>,
    },
}

impl Execution<'_> {
    fn source_data(&self) -> Option<SourceData> {
        match self {
            Execution::Bytecode { chunk, pc, .. } => chunk.find_label(*pc),
            _ => None,
        }
    }
}

impl<'gc> Execution<'gc> {
    fn from_lambda(lambda: Lambda<'gc>, prev_fallback: ImportFallback<'gc>) -> Self {
        match lambda {
            Lambda::Native(gc) => Self::Native { native: gc },
            Lambda::Compiled(gc) => Self::Bytecode {
                chunk: gc.chunk,
                arity: gc.arity,
                pc: 0,
                upvalue_index: Some(gc.upvalue_id.expect("cannot use unlabeled lambda")),
                fallback: gc.chunk.fallback.or(prev_fallback),
            },
        }
    }
}

impl PartialEq for Execution<'_> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Execution::Bytecode {
                chunk,
                arity,
                pc,
                upvalue_index,
                fallback,
            } => {
                let Self::Bytecode {
                    chunk: ochunk,
                    arity: oarity,
                    pc: opc,
                    upvalue_index: oupvalue_index,
                    fallback: ofallback,
                } = other
                else {
                    return false;
                };
                Gc::ptr_eq(*chunk, *ochunk)
                    && arity == oarity
                    && pc == opc
                    && upvalue_index == oupvalue_index
                    && (matches!((fallback, ofallback), (None, None))
                        || matches!((fallback, ofallback), (Some(f), Some(of)) if Gc::ptr_eq(*f, *of)))
            }
            Execution::Native { native } => {
                let Self::Native { native: onative } = other else {
                    return false;
                };
                Gc::ptr_eq(*native, *onative)
            }
        }
    }
}
impl Eq for Execution<'_> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExecutionKind {
    Bytecode { pc: usize },
    Native,
}

#[derive(Collect, Clone)]
#[collect(no_drop)]
pub struct ThreadFrame<'gc> {
    execution: Execution<'gc>,
    // error handler
    handler: Option<Lambda<'gc>>,
    // dynamic-wind before and after
    dynamic_wind: DynamicWind<'gc>,
    args: Box<[ValuePtr<'gc>]>,
    exception: Option<SchemeErrorPtr<'gc>>,
    env: StackEnvironmentPtr<'gc>,
    bottom: usize,
    // TODO add a "promise slot" that when a frame is exiting, will fill the promise with the value it is
    // exiting the frame with.
}

impl ThreadFrame<'_> {
    /// Get the kind of frame
    pub fn kind(&self) -> ExecutionKind {
        match self.execution {
            Execution::Bytecode { pc, .. } => ExecutionKind::Bytecode { pc },
            Execution::Native { .. } => ExecutionKind::Native,
        }
    }
}

impl<'gc> ThreadFrame<'gc> {
    pub fn env(&self, mc: &Mutation<'gc>) -> StackEnvironmentPtr<'gc> {
        let mut env = *self.env.borrow();
        env.freeze();
        Gc::new(mc, RefLock::new(env))
    }
}

impl PartialEq for ThreadFrame<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.execution == other.execution
            && self.handler == other.handler
            && self.dynamic_wind == other.dynamic_wind
            && self.args == other.args
            && (matches!((self.exception, other.exception), (Some(se), Some(oe)) if Gc::ptr_eq(se, oe))
                || self.exception.is_none() && other.exception.is_none())
            && Gc::ptr_eq(self.env, other.env)
            && self.bottom == other.bottom
    }
}
impl Eq for ThreadFrame<'_> {}

impl std::fmt::Debug for ThreadFrame<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("ThreadFrame");
        debug
            .field("args", &self.args)
            .field("exception", &self.exception)
            .field("bottom", &self.bottom)
            .field("execution", &self.kind());
        if let Execution::Bytecode {
            arity,
            pc,
            upvalue_index,
            ..
        } = &self.execution
        {
            debug
                .field("arity", arity)
                .field("pc", pc)
                .field("upvalue_index", upvalue_index);
        }
        debug.finish_non_exhaustive()
    }
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum LambdaException {
    #[error("mismatched arity: expected {expected} arguments, got {got}")]
    MismatchedArity { expected: Arity, got: usize },
}

// The type responsible for expanding the stack (also making sure that)
// if by some act of malevolence a Values becomes self-referential, ensuring that
// we do not iterate infinitely (infinite recursion's for the lists and vecs, values don't do that)
struct StackExpander<'gc> {
    stack: Stack<'gc>,
    encountered_values: Vec<ValuePtr<'gc>>,
    in_progress: Option<Vec<ValuePtr<'gc>>>,
}

impl<'gc> Iterator for StackExpander<'gc> {
    type Item = ValuePtr<'gc>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.in_progress.as_mut().and_then(|inp| inp.pop()) {
            return Some(item);
        }

        // in_progress is def empty, so clear it
        self.in_progress = None;

        // pop a value off the stack. If there is nothing left, then we're done!
        let val = self.stack.pop()?;

        match *val.borrow() {
            Value::Values(values) => {
                // Check that we haven't already read this out (or if values is empty)
                if self.encountered_values.iter().any(|p| Gc::ptr_eq(*p, val)) || values.is_empty()
                {
                    // "continue"
                    return self.next();
                }

                // We've encountered this value, so don't process it ever again
                self.encountered_values.push(val);

                // return the first value, and set the rest to in_progress
                self.in_progress = Some(values.iter().skip(1).rev().copied().collect());
                Some(values[0])
            }
            _ => Some(val),
        }
    }
}

pub type ThreadPtr<'gc> = Gc<'gc, RefLock<Thread<'gc>>>;
pub type Stack<'gc> = Vec<ValuePtr<'gc>>;
/// Runs all code
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc> {
    stack: Stack<'gc>,
    frames: Vec<ThreadFrame<'gc>>,
    error: Option<SchemeErrorPtr<'gc>>,
    // used in the creation of self-referential datastructures and recursive structures
    holes: fxhash::FxHashMap<usize, ValuePtr<'gc>>,
    // Upvalues for executing code
    upvalues: Vec<ValuePtr<'gc>>,
    // Mapping for lambdas to upvalue
    upvalue_mapping: fxhash::FxHashMap<usize, fxhash::FxHashMap<usize, usize>>,
    // counter for the upvalue mapping
    next_upvalue_index: usize,
}

impl Default for Thread<'_> {
    fn default() -> Self {
        Self::new_empty()
    }
}

// Public-facing API
impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>, chunk: ChunkPtr<'gc>) -> Self {
        Self {
            frames: vec![ThreadFrame {
                execution: Execution::Bytecode {
                    chunk,
                    pc: 0,
                    arity: Arity::Exact(0),
                    upvalue_index: None,
                    fallback: chunk.fallback,
                },
                env: Gc::new(
                    mc,
                    RefLock::new(StackEnvironment::new(mc, Some(chunk.import_env))),
                ),
                args: Box::from([]),
                handler: None,
                dynamic_wind: None,
                exception: None,
                bottom: 0,
            }],
            upvalues: Vec::with_capacity(chunk.upvalues),
            ..Self::new_empty()
        }
    }

    pub fn new_empty() -> Self {
        Self {
            frames: vec![],
            upvalues: Vec::new(),
            stack: vec![],
            error: None,
            holes: fxhash::FxHashMap::default(),
            upvalue_mapping: Default::default(),
            next_upvalue_index: 0,
        }
    }

    /// Creates a frame of evaluation for the given lambda
    pub fn call(
        &mut self,
        ctx: &Context<'_, 'gc>,
        lambda: Lambda<'gc>,
        error_handler: Option<Lambda<'gc>>,
        args: &[ValuePtr<'gc>],
    ) -> Result<(), LambdaException> {
        // Push args to stack
        for arg in args {
            self.stack.push(*arg);
        }
        self.call_lambda(ctx, lambda, args.len(), false)?;
        // Install error handler (if any)
        self.frames.last_mut().unwrap().handler = error_handler;
        Ok(())
    }

    fn allocate_upvalue_index(counter: &mut usize) -> usize {
        let c = *counter;
        *counter += 1;
        c
    }

    /// Creates a frame of evaluation for included code
    ///
    /// Clobbers the current frame if `tail` is true
    pub fn include(
        &mut self,
        mc: &Mutation<'gc>,
        chunk: ChunkPtr<'gc>,
        error_handler: Option<Lambda<'gc>>,
        tail: bool,
    ) {
        let execution = Execution::Bytecode {
            chunk,
            pc: 0,
            arity: Arity::Exact(0),
            upvalue_index: None,
            fallback: chunk.fallback,
        };

        if tail {
            if let Some(last) = self.frames.last_mut() {
                // Adjust the frame's parent to point to *this* chunk's import env
                last.env.borrow_mut(mc).reparent(Some(chunk.import_env));
                last.execution = execution;
                return;
            }
        }

        self.frames.push(ThreadFrame {
            execution,
            env: Gc::new(
                mc,
                RefLock::new(StackEnvironment::new(mc, Some(chunk.import_env))),
            ),
            args: Box::new([]),
            handler: error_handler,
            dynamic_wind: None,
            exception: None,
            bottom: self.stack.len(),
        });
    }

    /// Resets execution state & stack
    pub fn reset(&mut self) {
        self.reset_error();
        self.clear_stack();
        self.frames.clear();
        debug_assert!(self.is_finished());
    }

    /// Clears error
    #[inline]
    pub fn reset_error(&mut self) {
        let _ = self.error.take();
    }

    /// Clears stack
    #[inline]
    pub fn clear_stack(&mut self) {
        self.stack.clear();
        // holes only really matter to stack values, so, drop all holes
        self.holes.clear();
    }

    /// When a thread has no frames, then it is considered to be finished.
    pub fn is_finished(&self) -> bool {
        self.frames.is_empty()
    }

    /// If finished, returns the values that the program resulted in
    pub fn result(
        &self,
    ) -> Option<Result<impl Iterator<Item = ValuePtr<'gc>>, SchemeErrorPtr<'gc>>> {
        if !self.is_finished() {
            return None;
        }

        Some(if let Some(err) = self.error {
            Err(err)
        } else {
            // no actually we want to reverse this stack to preserve source order
            // and actually only return the value at the top of the stack (if it's Value::Values, then it can be more than 1~)
            Ok(StackExpander {
                stack: self.stack.iter().copied().rev().take(1).collect(),
                encountered_values: Vec::new(),
                in_progress: None,
            })
        })
    }

    // Defined on &mut rather than &self for lambda denial
    pub fn env(&mut self) -> Option<StackEnvironmentPtr<'gc>> {
        Self::current_env(&self.frames)
    }

    // Ditto
    pub fn envs(&mut self) -> impl Iterator<Item = StackEnvironmentPtr<'gc>> {
        self.frames.iter().map(|f| f.env)
    }

    fn current_env(frames: &[ThreadFrame<'gc>]) -> Option<StackEnvironmentPtr<'gc>> {
        frames.last().map(|f| f.env)
    }

    pub fn stack(&self) -> &[ValuePtr<'gc>] {
        self.stack.as_slice()
    }

    pub fn stack_mut(&mut self) -> &mut Stack<'gc> {
        &mut self.stack
    }

    pub fn frames(&self) -> &[ThreadFrame<'gc>] {
        self.frames.as_slice()
    }

    pub fn exception(&self) -> Option<SchemeErrorPtr<'gc>> {
        self.error
    }
}

// Private internals
impl<'gc> Thread<'gc> {
    /// Handles things that happen at the end of a frame, as well as returning it's value
    /// and adjusting the execution stack.
    ///
    /// This should be the last thing done on a step before looping back to fuelcheck.
    ///
    /// `should_pop` can be set to false in the case of tail calls
    // TODO Add an Option return value to indicate the frame that should be clobbered by tail calls (
    // once dynamic_wind handlers enter the fray)
    fn handle_frame_end(&mut self, ctx: &Context<'_, 'gc>, should_pop: bool) {
        let Some(frame) = self.frames.last_mut() else {
            unreachable!("[ICE] no frame present");
        };

        // used later
        let bottom = frame.bottom;
        // handle exception frame interaction with non-continuable errors
        if let Some(err) = frame.exception {
            if !err.error_type.is_continuable() {
                let index = self.error_handler_frame_index().unwrap();
                self.frames.drain(index..);
                self.error = Some(Gc::new(
                    ctx,
                    SchemeError {
                        backtrace: Self::make_backtrace(&self.frames),
                        error_type: SchemeErrorType::HandlerFailed(Gc::new(
                            ctx,
                            err.error_type.clone(),
                        )),
                    },
                ));
            }
        }

        // TODO Lambdas only return the last value of their body, so this should be
        // - Pop the value at top of stack (unless resulting stack is empty, then synthesize Void)
        // - drain anything frame.bottom..
        // - push the value we popped/synthesized earlier
        // Lambdas would use a "values" function to return more than one value.
        // Native lambdas support this logic natively (if their Return vec len == 1, that value is unwrapped,
        // if 0, return Void, otherwise returns Values)
        let ret_val = self.stack.pop();
        // drain any extra value on stack
        if self.stack.len() >= bottom {
            self.stack.drain(bottom..);
        }
        if let Some(ret) = ret_val {
            self.stack.push(ret);
        } else {
            self.stack.push(Value::Void.into_ptr(ctx));
        }

        // TODO If dynamic-wind is present. call the after
        if should_pop {
            self.frames.pop();
        }
    }

    fn make_backtrace(frames: &[ThreadFrame<'gc>]) -> Vec<StackFrame<'gc>> {
        frames
            .iter()
            .enumerate()
            .rev()
            .map(|(i, f)| {
                // get the source data of the *parent* frame, which is what called this frame
                let psd = i
                    .checked_sub(1)
                    .and_then(|idx| frames.get(idx))
                    .and_then(|pf| pf.execution.source_data());
                StackFrame {
                    source_filename: psd.map(|psd| psd.source_id),
                    range: psd.map(|psd| psd.range),
                    execution: f.execution,
                }
            })
            .collect()
    }

    /// Sets up the call to a lambda
    fn call_lambda(
        &mut self,
        ctx: &Context<'_, 'gc>,
        lambda: Lambda<'gc>,
        mut args: usize,
        is_tail: bool,
    ) -> Result<(), LambdaException> {
        // TODO First argument is lowest on the stack
        // TODO Remember the Value::Values counts for 1 value!!!, so no special handling
        // Only as output of the entire system is it special
        if !lambda.arity().is_satisfied(args) {
            return Err(LambdaException::MismatchedArity {
                expected: lambda.arity(),
                got: args,
            });
        }
        if self.stack.len() < args {
            // If this is triggered, we should check if the actual number of items on the stack still
            // satisfy the arity. If so, we patch it in as args, if not, we error with an arity mismatch
            if lambda.arity().is_satisfied(self.stack.len()) {
                args = self.stack.len();
            } else {
                return Err(LambdaException::MismatchedArity {
                    expected: lambda.arity(),
                    got: self.stack.len(),
                });
            }
        };
        let args: Vec<_> = self.stack.drain(self.stack.len() - args..).collect();
        // Handle the previous frame return here
        if is_tail {
            // Exiting the current frame
            self.handle_frame_end(ctx, false);
        }
        while let Some(v) = self.stack.last() {
            match *v.borrow() {
                Value::Void => {
                    self.stack.pop();
                }
                _ => {
                    break;
                }
            }
        }
        let new_frame = ThreadFrame {
            // Ignore voids at the top of the stack when determining the bottom of a frame
            bottom: self.stack.len(),
            execution: Execution::from_lambda(
                lambda,
                self.frames.last().and_then(|f| match f.execution {
                    Execution::Bytecode { fallback, .. } => fallback,
                    _ => None,
                }),
            ),
            handler: None,
            dynamic_wind: None,
            args: Box::from(args.as_slice()),
            exception: None,
            env: Gc::new(
                ctx,
                RefLock::new(StackEnvironment::new(ctx, Self::current_env(&self.frames))),
            ),
        };

        if is_tail {
            if let Some(frame) = self.frames.last_mut() {
                *frame = new_frame;
                return Ok(());
            }
        }

        self.frames.push(new_frame);
        Ok(())
    }

    /// Create a continuation that can be called at a later time (on this thread)
    /// to jump execution to what would happen after this point
    ///
    /// # Parameters
    /// - `is_tail`: will exclude the current frame if true.
    pub fn create_continuation(&self, mc: &Mutation<'gc>, is_tail: bool) -> Continuation<'gc> {
        let mut frames_copy: Vec<_> = if !is_tail {
            &self.frames[..]
        } else {
            &self.frames[..self.frames.len() - 1]
        }
        .iter()
        .map(|f| ThreadFrame {
            execution: match f.execution {
                Execution::Native { native } => Execution::Native {
                    native: native.borrow().continuation(mc).unwrap_or(native),
                },
                _ => f.execution,
            },
            ..f.clone()
        })
        .collect();
        // Adjust the last to actually be the continuation (if bytecode frame)
        if let Some(Execution::Bytecode { pc, chunk, .. }) =
            frames_copy.last_mut().map(|f| &mut f.execution)
        {
            // handle Jumps
            match chunk.code.get(*pc) {
                Some(code) => match code {
                    Bytecode::If { .. } => {
                        // don't move the pc, as the If instruction will handle it
                    }
                    Bytecode::Jump { jump } => {
                        // jump forward (as this is unconditional)
                        *pc += jump + 1;
                    }
                    _ => {
                        // all other instructions move linearly
                        *pc += 1;
                    }
                },
                None => {
                    // no instruction here, don't move pc, as the frame will be popped once handled
                }
            }
        };

        Continuation::new(frames_copy)
    }

    fn handle_continuation(&mut self, mc: &Mutation<'gc>, c: ContinuationPtr<'gc>, args: usize) {
        // TODO Make sure to add before and after calls on *top* of the native call for all
        // frames that are left
        // with all befores below all afters , e.g.:
        // if two dynamic-wind lambdas are to be exited [n f1 ... f2]
        // then the frame stack should look like
        // [n after(f2) after(f1) before(f1) before(f2)]
        // (which is reversed call order, because stack)
        // n can be a native frame or nothing (null continuation means "go to first native call below this")

        // TODO handle dynamic-wind and non-empty continuations
        // Don't advance the frame b/c it will be wiped by the continuation
        if c.frames.is_empty() {
            // wrap the last args values as a values object
            let values = self
                .stack
                .drain(self.stack.len() - args.min(self.stack.len())..);
            let values = Value::Values(Gc::new(mc, Vec::from_iter(values))).into_ptr(mc);
            self.stack.push(values);
            // We just dump execution
            self.frames.clear();
        } else {
            // self.frames = c.frames.to_vec();
            // TODO The above would work but for upvalues (and dynamic-wind handling TODO). Figure out why.
            // "Duh". The continuation at capture might not have an upvalue_index assigned at capture, while the current continuation
            // *might*. What is the behavior expected of a continuation call?
            // My idea is that the continuation copies the upvalue indices (if not present) of the current frame (if
            // at the start) or the previous frame (if there was an index), something to the effect of:
            // c.frames.iter()
            // .scan(last_upvalue_index, |upvalue_index, f| if f.upvalue_index.is_none() { ThreadFrame{upvalue_index, ..f} } else { *upvalue_index = f.upvalue_index; f }).collect()
            // TODO instead of the scan, investigate just replacing the upvalue_index of all continuation frames with the index of the
            // last frame
            // then we create dynamic-wind frames as necessary on top of these frames, where the handler copies the upvalue_index of the frame it comes from.
            // Then the frames we just created, together with the dynamic-wind frames generated from all frames (including the current ones) *replace* the current frames
            // (this is why we "cheat" then the continuation is empty, at that point, we only have to handle dynamic-wind)
            todo!("continuation handling {c:?}")
        }
    }

    fn fallback_handling(
        frames: &[ThreadFrame<'gc>],
        symbol: lasso::Spur,
    ) -> Option<ValuePtr<'gc>> {
        frames
            .iter()
            .rev()
            .filter_map(|frame| {
                if let Execution::Bytecode {
                    fallback: Some(fallback),
                    ..
                } = &frame.execution
                {
                    Some(*fallback)
                } else {
                    None
                }
            })
            // .inspect(|fb| eprintln!("FBFB: {:?}", fb.keys()))
            .find_map(|fb| fb.get(&Static(symbol)).copied())
            .filter(|&fallback| !matches!(*fallback.borrow(), Value::Undefined))
    }

    fn error_handler(&self) -> Option<Lambda<'gc>> {
        self.frames.iter().rev().find_map(|f| f.handler)
    }

    fn error_handler_frame_index(&self) -> Option<usize> {
        self.frames.iter().rposition(|f| f.handler.is_some())
    }

    // The amount of fuel a native call costs
    const NATIVE_COST: i32 = 4;

    /// Executes instructions until fuel determines it should not continue
    pub fn step(
        &mut self,
        ctx: Context<'_, 'gc>,
        interner: &mut lasso::Rodeo,
        // Used for (scheme eval) and its compilation process
        world: &World,
        includer: &dyn Includer,
        fuel: &mut Fuel,
    ) {
        macro_rules! make_error {
            ($err:expr) => {
                self.error = Some(Gc::new(
                    &ctx,
                    SchemeError {
                        backtrace: Self::make_backtrace(&self.frames),
                        error_type: $err,
                    },
                ));
            };
        }

        // When a step is called, we cleat the interrup status of fuel at this point
        fuel.clear_interrupt();

        while fuel.should_continue() {
            // This is here b/c we could be finished while fuel still remains
            if self.is_finished() {
                return;
            }

            // eprintln!("FRAMEC: {}", self.frames.len());

            if let Some(err) = self.error {
                // Find an error handler and set it up to run (if not handling one)
                if self.frames.last().unwrap().exception.is_none() {
                    if let Some(handler) = self.error_handler() {
                        if let Err(_err) = self.call_lambda(&ctx, handler, 1, true) {
                            // not a valid handler, so *take* it
                            let index = self.error_handler_frame_index().unwrap();
                            self.frames[index].handler.take();
                            make_error!(SchemeErrorType::HandlerFailed(Gc::new(
                                &ctx,
                                err.error_type.clone()
                            )));
                            continue;
                        }
                        // mark frame as exception
                        self.frames.last_mut().unwrap().exception = self.error.take();
                        continue;
                    } else {
                        // execution ends with this error
                        self.frames.drain(..);
                        return;
                    }
                }
            }

            let current_env = Self::current_env(&self.frames);
            let Some(frame) = self.frames.last_mut() else {
                // Nothing to do.
                return;
            };

            // handle execution
            match &mut frame.execution {
                Execution::Bytecode {
                    chunk,
                    pc,
                    arity,
                    upvalue_index,
                    ..
                } => {
                    // If framepointer is oob, then that means execution of this frame is finished
                    if chunk.code.len() <= *pc {
                        self.handle_frame_end(&ctx, true);
                        continue;
                    }
                    macro_rules! advance_to_next_inst {
                        () => {
                            *pc += 1;
                        };
                        ($frame:expr) => {
                            if let Some(Execution::Bytecode { pc, .. }) =
                                $frame.map(|f| &mut f.execution)
                            {
                                *pc += 1;
                            }
                        };
                        (undo $frame:expr) => {
                            if let Some(Execution::Bytecode { pc, .. }) =
                                $frame.map(|f| &mut f.execution)
                            {
                                *pc -= 1;
                            }
                        };
                    }
                    // The core of execution
                    let inst = chunk.code[*pc];
                    // eprintln!("EXEC >> {inst:?} {:?}", self.stack.len());
                    fuel.consume(inst.cost());
                    match inst {
                        Bytecode::PushNull => {
                            self.stack.push(ctx.null_value);
                            advance_to_next_inst!();
                        }
                        Bytecode::PushVoid => {
                            self.stack.push(Gc::new(&ctx, RefLock::new(Value::Void)));
                            advance_to_next_inst!();
                        }
                        Bytecode::PushBool { bool } => {
                            if bool {
                                self.stack.push(ctx.true_value)
                            } else {
                                self.stack.push(ctx.false_value)
                            }
                            advance_to_next_inst!();
                        }
                        Bytecode::PushConst { index } => {
                            self.stack.push(Gc::new(
                                &ctx,
                                RefLock::new(chunk.constants[index].clone().into_value(&ctx)),
                            ));
                            advance_to_next_inst!();
                        }
                        Bytecode::PushLambda { index } => {
                            self.stack.push(Gc::new(
                                &ctx,
                                RefLock::new(Value::Lambda({
                                    let l = chunk.lambdas[index];
                                    let l = if let Some(upvalue_index) = *upvalue_index {
                                        l.label(&ctx, upvalue_index)
                                    } else {
                                        l.label(
                                            &ctx,
                                            Self::allocate_upvalue_index(
                                                &mut self.next_upvalue_index,
                                            ),
                                        )
                                    };
                                    l
                                })),
                            ));
                            advance_to_next_inst!();
                        }
                        Bytecode::FetchArg { index } => {
                            if index < frame.args.len()
                                && !matches!(*frame.args[index].borrow(), Value::Undefined)
                            {
                                self.stack.push(frame.args[index]);
                                advance_to_next_inst!();
                            } else {
                                make_error!(SchemeErrorType::InvalidArg(index));
                            }
                        }
                        Bytecode::FetchRest => {
                            if let Arity::AtLeast(base) = *arity {
                                let cons = ConsCell::from_iter(
                                    &ctx,
                                    ctx.null_value,
                                    frame.args[base..].iter().copied(),
                                );
                                self.stack.push(cons);
                                advance_to_next_inst!();
                            } else {
                                make_error!(SchemeErrorType::InvalidRest);
                            }
                        }
                        Bytecode::SetUpvalue { index } => {
                            let upvalues_len = self.upvalues.len();
                            // // Assign a new upvalue scope if this one already exists
                            // let Some(upvalue_index) = *upvalue_index else {
                            //     todo!("not in upvalue scope");
                            // };
                            // if let Some(mapping) = self.upvalue_mapping.get(&lupvalue_index) {
                            //     if mapping.get(&index).is_some() {
                            //         // duplicate and make unique
                            //         let new_scope =
                            //             Self::allocate_upvalue_index(&mut self.next_upvalue_index);
                            //         self.upvalue_mapping.insert(new_scope, mapping.clone());
                            //         *upvalue_index = Some(new_scope);
                            //         // lupvalue_index = new_scope;
                            //     }
                            // }
                            // dbg!((&self.upvalue_mapping, *upvalue_index));
                            self.upvalue_mapping
                                .entry(upvalue_index.expect("not in upvaluable scope"))
                                .or_default()
                                .insert(index, upvalues_len);
                            let index = upvalues_len;
                            if upvalues_len <= index {
                                // Fill upvalues with undefineds
                                self.upvalues.extend(std::iter::repeat_n(
                                    Gc::new(&ctx, RefLock::new(Value::Undefined)),
                                    index - upvalues_len + 1,
                                ));
                            }
                            let Some(val) = self.stack.last().copied() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            self.upvalues[index] = val;
                            advance_to_next_inst!();
                        }
                        Bytecode::FetchUpvalue { index } => {
                            // Get the *actual* index or error
                            // dbg!((&self.upvalue_mapping, *upvalue_index));
                            let Some(index) = self
                                .upvalue_mapping
                                .get(upvalue_index.as_ref().expect("not in upvaluable scope"))
                                .and_then(|upm| upm.get(&index).copied())
                            else {
                                todo!("TODO upvalue misreference miscompilation");
                            };
                            if let Some(val) = self.upvalues.get(index) {
                                if !matches!(*val.borrow(), Value::Undefined) {
                                    self.stack.push(*val);
                                    advance_to_next_inst!();
                                } else {
                                    todo!("TODO upvalue misreference miscompilation");
                                }
                            } else {
                                todo!("TODO Handle if upvalue misreferenced miscompilation")
                            }
                        }
                        Bytecode::Reference { symbol } => {
                            // read a symbol from the environment
                            // if it is undefined (None or Some(Value::Undefined)), error, otherwise
                            // push the value to stack
                            //
                            // if-chaining would be *posh* here
                            if let Some(val) = current_env.unwrap().borrow().get(symbol) {
                                if !matches!(*val.read(|v| v.borrow()), Value::Undefined) {
                                    self.stack.push(*val.get().borrow());
                                    advance_to_next_inst!();
                                } else {
                                    make_error!(SchemeErrorType::EnvLoad(Box::from(
                                        interner.resolve(&symbol),
                                    )));
                                }
                            } else if let Some(fallback) =
                                Self::fallback_handling(&self.frames, symbol)
                            {
                                self.stack.push(fallback);
                                let Some(frame) = self.frames.last_mut() else {
                                    unreachable!()
                                };
                                let Execution::Bytecode { pc, .. } = &mut frame.execution else {
                                    unreachable!()
                                };
                                *pc += 1;
                            } else {
                                make_error!(SchemeErrorType::EnvLoad(Box::from(
                                    interner.resolve(&symbol),
                                )));
                            }
                        }
                        Bytecode::MakePair => {
                            // dbg!(&self.stack);
                            if self.stack.len() < 2 {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            }

                            let car = self.stack.pop();
                            let cdr = self.stack.pop();
                            self.stack
                                .push(Value::Cons(ConsCell { car, cdr }).into_ptr(&ctx));
                            advance_to_next_inst!();
                        }
                        Bytecode::MakeVector { length } => {
                            if self.stack.len() < length {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            }

                            let items = self.stack.drain(self.stack.len() - length..);
                            let vector = value::Vector::new(im_rc::Vector::from_iter(items))
                                .into_value(&ctx)
                                .into_ptr(&ctx);
                            self.stack.push(vector);
                            advance_to_next_inst!();
                        }
                        Bytecode::MakeHole { id } => {
                            // create an undefined value
                            let val = Gc::new(&ctx, RefLock::new(Value::Undefined));
                            // mark as a hole
                            if self.holes.contains_key(&id) {
                                make_error!(SchemeErrorType::AlreadyDefinedHole(id));
                                continue;
                            }
                            self.holes.insert(id, val);
                            self.stack.push(val);
                            advance_to_next_inst!();
                        }
                        Bytecode::FillHole { id } => {
                            // get the value at the top of the stack, and make the requisite hole if defined equal to the value
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };

                            if let Some(hole_ptr) = self.holes.get(&id) {
                                *hole_ptr.borrow_mut(&ctx) = *value.borrow();
                                self.holes.remove(&id);
                                advance_to_next_inst!();
                            } else {
                                make_error!(SchemeErrorType::UndefinedHole(id));
                            }
                        }
                        Bytecode::Duplicate => {
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };

                            self.stack.push(value);
                            self.stack.push(value);
                            advance_to_next_inst!();
                        }
                        Bytecode::Pop => {
                            let Some(_value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            advance_to_next_inst!();
                        }
                        Bytecode::Call { args } => {
                            let Some(val) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NonCallable);
                                continue;
                            };

                            match *val.borrow() {
                                Value::Lambda(mut l @ Lambda::Compiled(_)) => {
                                    // dbg!((&self.upvalue_mapping, *upvalue_index, &l));
                                    // Generate copy of lambda scope if it already exists (for new upvalues)
                                    if let Some(mapping) =
                                        l.get_label().and_then(|l| self.upvalue_mapping.get(&l))
                                    {
                                        // if we are compiled and *set* upvalues, we need a copy of the scope
                                        if matches!(l, Lambda::Compiled(c) if c.chunk.code.iter().any(|bc| matches!(bc, Bytecode::SetUpvalue { .. })))
                                        {
                                            let new_scope = Self::allocate_upvalue_index(
                                                &mut self.next_upvalue_index,
                                            );
                                            self.upvalue_mapping.insert(new_scope, mapping.clone());
                                            l = l.label(&ctx, new_scope);
                                        }
                                        // otherwise, we can continue as normal
                                    }
                                    let pc = *pc;
                                    let code = std::rc::Rc::clone(&chunk.code);
                                    // advance to next inst *before* pushing lambda
                                    advance_to_next_inst!();
                                    if let Err(err) = self.call_lambda(
                                        &ctx,
                                        l,
                                        args,
                                        pc + match code[pc] {
                                            // make sure true branches can also be properly registered as tail calls
                                            // because a jump unconditionally executes, the actual total movement is
                                            // jump + 1 plus the + 1 base from this instruction
                                            Bytecode::Jump { jump } => jump + 1,
                                            _ => 1,
                                        } >= code.len(),
                                    ) {
                                        make_error!(SchemeErrorType::LambdaException(err));
                                        advance_to_next_inst!(undo self.frames.last_mut());
                                        continue;
                                    };
                                }
                                Value::Lambda(l @ Lambda::Native(_)) => {
                                    let pc = *pc;
                                    let code = std::rc::Rc::clone(&chunk.code);
                                    // advance to next inst *before* pushing lambda
                                    advance_to_next_inst!();
                                    if let Err(err) = self.call_lambda(
                                        &ctx,
                                        l,
                                        args,
                                        pc + match code[pc] {
                                            // make sure true branches can also be properly registered as tail calls
                                            // because a jump unconditionally executes, the actual total movement is
                                            // jump + 1 plus the + 1 base from this instruction
                                            Bytecode::Jump { jump } => jump + 1,
                                            _ => 1,
                                        } >= code.len(),
                                    ) {
                                        make_error!(SchemeErrorType::LambdaException(err));
                                        advance_to_next_inst!(undo self.frames.last_mut());
                                        continue;
                                    };
                                }
                                Value::Continuation(c) => {
                                    self.handle_continuation(&ctx, c, args);
                                }
                                _ => {
                                    make_error!(SchemeErrorType::NonCallable);
                                }
                            }
                        }
                        Bytecode::Splice => {
                            // Pop the list (which is the list to splice) and then the value to append to the list
                            // Find the null at the end of the list... and replace the pointer
                            // pointing to null with a pointer pointing to the value as a list, then push the list back to stack
                            let Some(list_value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            let Value::Cons(list) = *list_value.borrow() else {
                                make_error!(SchemeErrorType::WrongValue {
                                    inst: "splice",
                                    expected: ValueType::Cons,
                                    kind: (*list_value.borrow()).value_type()
                                });
                                continue;
                            };

                            let Some(mut values): Option<Vec<_>> = list
                                .list_values(list_value, ctx.null_value)
                                .map(|v| v.into_iter().collect())
                            else {
                                make_error!(SchemeErrorType::ExpectedList("splice"));
                                continue;
                            };
                            let value_list = match *value.borrow() {
                                Value::Cons(_) if Gc::ptr_eq(value, ctx.null_value) => vec![],
                                Value::Cons(c) => {
                                    if let Some(v) = c.list_values(value, ctx.null_value) {
                                        v.into_iter().collect()
                                    } else {
                                        make_error!(SchemeErrorType::ExpectedList("splice"));
                                        continue;
                                    }
                                }
                                _ => vec![value],
                            };
                            values.extend(value_list);

                            self.stack
                                .push(ConsCell::from_iter(&ctx, ctx.null_value, values));
                            advance_to_next_inst!();
                        }
                        Bytecode::Define { symbol } => {
                            // Pop the top of stack and store in env as a given symbol
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            if current_env
                                .unwrap()
                                .borrow_mut(&ctx)
                                .define(&ctx, symbol, value, false)
                                .is_err()
                            {
                                make_error!(SchemeErrorType::FrozenDefine);
                                continue;
                            }

                            advance_to_next_inst!();
                        }
                        Bytecode::SetBangUpvalue { index } => {
                            // Pop the top of stack and store in given upvalue
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            // Get the *actual* index or error
                            let Some(index) = self
                                .upvalue_mapping
                                .get(upvalue_index.as_ref().expect("not in upvaluable scope"))
                                .and_then(|upm| upm.get(&index).copied())
                            else {
                                todo!("TODO upvalue misreference miscompilation");
                            };
                            if self.upvalues.len() <= index {
                                todo!("TODO upvalue misreference miscompilation");
                            }
                            // Get the *actual* index, or error
                            self.upvalues[index] = value;

                            advance_to_next_inst!();
                        }
                        Bytecode::SetBang { symbol } => {
                            // Pop the top of stack and store in env as a given symbol
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            if current_env
                                .unwrap()
                                .borrow_mut(&ctx)
                                .rebind(&ctx, symbol, value, interner)
                                .is_err()
                            {
                                make_error!(SchemeErrorType::NoName(Box::from(
                                    interner.resolve(&symbol)
                                )));
                                continue;
                            }

                            advance_to_next_inst!();
                        }
                        Bytecode::If { jump } => {
                            let Some(value) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NoValue(inst));
                                continue;
                            };
                            if *value.borrow() == Value::Bool(false) {
                                *pc += jump;
                            }

                            advance_to_next_inst!();
                        }
                        Bytecode::Jump { jump } => {
                            *pc += jump;

                            advance_to_next_inst!();
                        }
                        _ => todo!(),
                    }
                }
                Execution::Native { native } => {
                    // TODO Remeber to check when [call-end] a native call to check for non-continuable errors
                    // TODO Remember to handle dynamic-wind properly when processing LambdaReturn::Continue
                    fuel.consume(Self::NATIVE_COST);
                    // Make all our data fixed and nice to borrow
                    let native = *native;
                    let Some(frame) = self.frames.last() else {
                        unreachable!()
                    };
                    let args = frame.args.as_ref();
                    let lctx = NativeLambdaContext {
                        self_ptr: native,
                        thread_ctx: ctx,
                        world,
                        stack: &self.stack[frame.bottom..],
                        interner,
                        includer,
                        fuel,
                        env: frame.env,
                        frames: &self.frames,
                        thread_ref: self,
                    };
                    let res = if let Some(err) = self.error {
                        // Let native code interfere with errors
                        native.borrow_mut(lctx.thread_ctx.mc).error(lctx, args, err)
                    } else {
                        native.borrow_mut(lctx.thread_ctx.mc).run(lctx, args)
                    };
                    // rebind frame to be mutable
                    let Some(frame) = self.frames.last_mut() else {
                        unreachable!()
                    };
                    // now interpret the result!
                    match res {
                        Ok(LambdaReturn::Waiting) => {
                            // Call interrupt (in case the lambda itself doesn't) so that
                            // the interpreter loop is disrupted
                            fuel.interrupt();
                            continue;
                        }
                        Ok(LambdaReturn::Return(vals)) => {
                            if vals.len() == 1 {
                                self.stack.push(vals[0]);
                            } else if vals.is_empty() {
                                self.stack.push(Value::Void.into_ptr(&ctx));
                            } else {
                                self.stack
                                    .push(Value::Values(Gc::new(&ctx, vals)).into_ptr(&ctx));
                            }
                            self.handle_frame_end(&ctx, true);
                        }
                        Ok(LambdaReturn::Continue { cont, args }) => {
                            let args_len = args.len();
                            self.stack.extend(args);
                            self.handle_continuation(&ctx, cont, args_len);
                        }
                        Ok(LambdaReturn::Raise {
                            error,
                            is_continuable,
                        }) => {
                            if is_continuable {
                                make_error!(SchemeErrorType::RaiseContinuable(
                                    Value::resolve_into(error, interner.clone(), ctx.null_value)
                                ));
                            } else {
                                make_error!(SchemeErrorType::Raise(Value::resolve_into(
                                    error,
                                    interner.clone(),
                                    ctx.null_value
                                )));
                            }
                            self.handle_frame_end(&ctx, true);
                        }
                        Ok(LambdaReturn::Propagate(err)) => {
                            // TODO Check if same error
                            // If not, store the current error in the new error irritants
                            self.error = Some(err);
                            self.handle_frame_end(&ctx, true);
                        }
                        Ok(LambdaReturn::Call {
                            lambda,
                            args,
                            dynamic_wind,
                        }) => {
                            let args_len = args.len();
                            self.stack.extend(args);
                            // label the lambda
                            let lambda = lambda.label(
                                &ctx,
                                Self::allocate_upvalue_index(&mut self.next_upvalue_index),
                            );
                            if let Err(err) = self.call_lambda(&ctx, lambda, args_len, false) {
                                make_error!(SchemeErrorType::LambdaException(err));
                                continue;
                            };
                            // Set dynamic wind
                            self.frames.last_mut().unwrap().dynamic_wind = dynamic_wind;
                        }
                        Ok(LambdaReturn::TailCall {
                            lambda,
                            args,
                            dynamic_wind,
                        }) => {
                            let args_len = args.len();
                            self.stack.extend(args);
                            // label the lambda
                            let lambda = lambda.label(
                                &ctx,
                                Self::allocate_upvalue_index(&mut self.next_upvalue_index),
                            );
                            if let Err(err) = self.call_lambda(&ctx, lambda, args_len, true) {
                                make_error!(SchemeErrorType::LambdaException(err));
                                continue;
                            };
                            // Set dynamic wind
                            self.frames.last_mut().unwrap().dynamic_wind = dynamic_wind;
                            // self.handle_frame_end(&ctx, true);
                        }
                        Ok(LambdaReturn::SetExceptionHandler(handler)) => {
                            frame.handler = Some(handler);
                        }
                        Err(e) => {
                            // TODO Add current error to new error irritants if present
                            match e {
                                LambdaError::Continuable(ce) => {
                                    make_error!(SchemeErrorType::RustContinuable(
                                        std::rc::Rc::new(ce)
                                    ));
                                }
                                LambdaError::NonContinuable(e) => {
                                    make_error!(SchemeErrorType::Rust(std::rc::Rc::new(e)));
                                }
                            }
                            self.handle_frame_end(&ctx, true);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::{Gc, RefLock, arena};
    use rstest::{fixture, rstest};

    use crate::{
        Fuel,
        bytecode::{self, Bytecode::*, Constant},
        compiler::World,
        environment::Environment,
        interpreter::{Context, Includer, NullIncluder},
        runtime::convert::IntoValue,
    };

    use super::Thread;

    #[fixture]
    fn empty_world() -> World {
        World::default()
    }

    #[fixture]
    fn null_includer() -> impl Includer {
        NullIncluder
    }

    #[rstest]
    fn thread_definition(empty_world: World, null_includer: impl Includer) {
        // No world is needed anymore if the compiler is unused
        let world = &empty_world;
        let includer = &null_includer;

        let mut interner = lasso::Rodeo::new();
        const INITIAL_FUEL: i32 = 1;
        let mut run_cost = 0;
        let mut fuel = Fuel::with(INITIAL_FUEL);
        arena::rootless_mutate(|mc| {
            let import_env = Gc::new(mc, RefLock::new(Environment::new(mc, None)));
            import_env
                .borrow_mut(mc)
                .define(
                    mc,
                    interner.get_or_intern_static("cowl"),
                    4.into_value(mc).into_ptr(mc),
                    true,
                )
                .expect("failed to define cowl");
            // manual compilation of "cowl\n(define ram 3)\nram"
            // TODO Convert this into a thread (full execution) datatest
            let chunk = bytecode::Chunk::new(
                mc,
                [
                    Reference {
                        symbol: interner.get_or_intern_static("cowl"),
                    },
                    PushConst { index: 0 },
                    Define {
                        symbol: interner.get_or_intern_static("ram"),
                    },
                    Reference {
                        symbol: interner.get_or_intern_static("ram"),
                    },
                    // This triggers an error! yay~ todo moke this to a lambda actually
                    // Reference {
                    //     symbol: interner.get_or_intern_static("nuban"),
                    // },
                ],
                [Constant::Number(3)],
                [],
                [],
                0,
                import_env,
                Default::default(),
            );

            let thread = Gc::new(mc, RefLock::new(Thread::new(mc, chunk)));
            // thread
            //     .borrow_mut(mc)
            //     .env()
            //     .unwrap()
            //     .borrow_mut(mc)
            //     .define(
            //         mc,
            //         interner.get_or_intern_static("cowl"),
            //         32.into_value(mc).into_ptr(mc),
            //         false,
            //     )
            //     .expect("failed to define `cowl`");
            let ctx = Context::new_test_context(mc, thread);
            while !thread.borrow().is_finished() && run_cost < 1000 {
                thread
                    .borrow_mut(&ctx)
                    .step(ctx, &mut interner, world, includer, &mut fuel);
                run_cost += INITIAL_FUEL - fuel.remaining();
                dbg!(run_cost);
                fuel.refill(1000, INITIAL_FUEL);
                eprintln!("{:#?}", thread.borrow());
            }
            eprintln!("finished? {}", thread.borrow().is_finished());
            if let Some(Err(err)) = thread.borrow().result() {
                eprintln!("{}", err.display(&interner, []));
            } else {
                eprintln!(
                    "{:#?}",
                    thread
                        .borrow()
                        .result()
                        .map(|r| r.map(|i| i.collect::<Vec<_>>()))
                );
            }
        })
    }
}
