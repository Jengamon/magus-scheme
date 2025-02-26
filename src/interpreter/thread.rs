use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    Fuel, Value,
    bytecode::{Bytecode, ChunkPtr, SourceData},
    compiler::World,
    environment::{StackEnvironment, StackEnvironmentPtr},
    runtime::{
        convert::IntoValue,
        error::{SchemeError, SchemeErrorPtr, SchemeErrorType, StackFrame},
        lambda::{Arity, Lambda, NativeLambdaPtr},
    },
    value::{ConsCell, Continuation, ValuePtr},
};

use super::{Context, Includer};

/// A thread's execution state can either be interpreting bytecode or running a native function or macro
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
enum Execution<'gc> {
    Bytecode {
        chunk: ChunkPtr<'gc>,
        #[collect(require_static)]
        arity: Arity,
        pc: usize,
    },
    Native {
        native: NativeLambdaPtr<'gc>,
        // when this frame is created, the stack's length goes here. this prevents native lambdas
        // from seeing any value below this
        bottom: usize,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExecutionKind {
    Bytecode { pc: usize },
    Native,
}

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct ThreadFrame<'gc> {
    execution: Execution<'gc>,
    handler: Option<Lambda<'gc>>,
    args: Box<[ValuePtr<'gc>]>,
    is_exception: bool,
    env: StackEnvironmentPtr<'gc>,
    // used for multiple returns!
    bottom: usize,
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
    /// Make a continuation
    pub fn continuation(&self) -> Continuation<'gc> {
        match self.execution {
            Execution::Bytecode { chunk, pc, arity } => Continuation::Continue {
                // a continuation of something is the *next* thing it would do
                pc: pc + 1,
                chunk,
                arity,
                args: self.args.clone(),
                handler: self.handler,
                bottom: self.bottom,
            },
            // The other types of execution are not continuable, so create the "null continuation"
            // which when executed, causes all bytecode frames to end.
            _ => Continuation::Null,
        }
    }

    pub fn env(&self, mc: &Mutation<'gc>) -> StackEnvironmentPtr<'gc> {
        let mut env = *self.env.borrow();
        env.freeze();
        Gc::new(mc, RefLock::new(env))
    }
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum LambdaException {
    #[error("mismatched arity: expected {expected} arguments, got {got}")]
    MismatchedArity { expected: Arity, got: usize },
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
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>, chunk: ChunkPtr<'gc>) -> Self {
        Self {
            stack: vec![],
            frames: vec![ThreadFrame {
                execution: Execution::Bytecode {
                    chunk,
                    pc: 0,
                    arity: Arity::Exact(0),
                },
                env: Gc::new(
                    mc,
                    RefLock::new(StackEnvironment::new(mc, Some(chunk.import_env))),
                ),
                args: Box::from([]),
                handler: None,
                is_exception: false,
                bottom: 0,
            }],
            error: None,
        }
    }

    /// Creates a frame of evaluation for included code
    ///
    /// Clobbers the current frame if `tail` is true
    pub fn include(
        &mut self,
        mc: &Mutation<'gc>,
        chunk: ChunkPtr<'gc>,
        handler: Option<Lambda<'gc>>,
        tail: bool,
    ) {
        let execution = Execution::Bytecode {
            chunk,
            pc: 0,
            arity: Arity::Exact(0),
        };

        if tail {
            if let Some(last) = self.frames.last_mut() {
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
            handler,
            is_exception: false,
            bottom: self.stack.len(),
        });
    }

    /// When a thread has no frames, then it is considered to be finished.
    pub fn is_finished(&self) -> bool {
        self.frames.is_empty()
    }

    /// If finished, returns the values that the program resulted in
    pub fn result(&self) -> Option<Result<&[ValuePtr<'gc>], SchemeErrorPtr<'gc>>> {
        if !self.is_finished() {
            return None;
        }

        Some(if let Some(err) = self.error {
            Err(err)
        } else {
            Ok(&self.stack)
        })
    }

    // Defined on &mut rather than &self for lambda denial
    pub fn env(&mut self) -> Option<StackEnvironmentPtr<'gc>> {
        Self::current_env(&self.frames)
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

    fn make_backtrace(&self, interner: &mut lasso::Rodeo) -> Vec<StackFrame> {
        self.frames
            .iter()
            .rev()
            .map(|f| {
                let sd = f.execution.source_data();
                StackFrame {
                    source_filename: sd.map(|sd| sd.source_id),
                    range: sd.map(|sd| sd.range),
                    scope_label: Some(interner.get_or_intern(format!("{:?}", f.kind()))),
                }
            })
            .collect()
    }

    /// Sets up the call to a lambda
    fn call_lambda(
        &mut self,
        lambda: Lambda<'gc>,
        args: usize,
        tail: bool,
    ) -> Result<(), LambdaException> {
        todo!()
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
        ctx: Context<'gc>,
        interner: &mut lasso::Rodeo,
        world: &World,
        includer: &dyn Includer,
        fuel: &mut Fuel,
    ) {
        macro_rules! make_error {
            ($err:expr) => {
                self.error = Some(Gc::new(
                    &ctx,
                    SchemeError {
                        backtrace: self.make_backtrace(interner),
                        error_type: $err,
                    },
                ));
            };
        }

        while fuel.should_continue() {
            if self.is_finished() {
                return;
            }

            if let Some(err) = self.error {
                // Find an error handler and set it up to run (if not handling one)
                if !self.frames.last().unwrap().is_exception {
                    if let Some(handler) = self.error_handler() {
                        if let Err(_err) = self.call_lambda(handler, 1, true) {
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
                        self.frames.last_mut().unwrap().is_exception = true;
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
                Execution::Bytecode { chunk, pc, arity } => {
                    // TODO If erroring, check if frame defines a handler. If so, jump to that handler, if not,
                    // pop the frame, but in the end always continue
                    // If framepointer is oob, then that means execution of this frame is finished
                    if chunk.code.len() <= *pc {
                        // TODO We could make this a function, so that native lambdas can use the same code...
                        if frame.is_exception {
                            if let Some(err) = self.error {
                                if !err.error_type.is_continuable() {
                                    let index = self.error_handler_frame_index().unwrap();
                                    self.frames.drain(index..);
                                    make_error!(SchemeErrorType::HandlerFailed(Gc::new(
                                        &ctx,
                                        err.error_type.clone()
                                    )));
                                }
                            }
                        }
                        // TODO Lambdas only return the last value of their body, so this should be
                        // - Pop the value at top of stack (unless resulting stack is empty, then synthesize Void)
                        // - drain anything frame.bottom..
                        // - push the value we popped/synthesized earlier
                        // Lambdas would use a "values" function to return more than one value.
                        // Native lambdas support this logic natively (if their Return vec len == 1, that value is unwrapped,
                        // if 0, return Void, otherwise returns Values)
                        // FIXME Remember to do the same for native lambdas (so do it as a function)
                        self.frames.pop();
                        continue;
                    }
                    macro_rules! advance_to_next_inst {
                        () => {
                            *pc += 1;
                        };
                    }
                    // The core of execution
                    let inst = chunk.code[*pc];
                    fuel.consume(inst.cost());
                    match inst {
                        Bytecode::PushNull => {
                            self.stack.push(ctx.null_value);
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
                                RefLock::new(Value::Lambda(Lambda::Compiled(chunk.lambdas[index]))),
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
                            } else {
                                make_error!(SchemeErrorType::EnvLoad(Box::from(
                                    interner.resolve(&symbol),
                                )));
                            }
                        }
                        Bytecode::Call { args } => {
                            let Some(val) = self.stack.pop() else {
                                make_error!(SchemeErrorType::NonCallable);
                                continue;
                            };

                            match *val.borrow() {
                                Value::Lambda(l) => {
                                    let pc = *pc;
                                    let code_len = chunk.code.len();
                                    if let Err(err) = self.call_lambda(l, args, pc + 1 >= code_len)
                                    {
                                        make_error!(SchemeErrorType::LambdaException(err));
                                        continue;
                                    };
                                }
                                Value::Continuation(c) => match &*c {
                                    Continuation::Continue {
                                        pc: cpc,
                                        chunk: cchunk,
                                        arity: carity,
                                        handler,
                                        args,
                                        bottom,
                                    } => {
                                        *chunk = *cchunk;
                                        *pc = *cpc;
                                        *arity = *carity;
                                        frame.handler = *handler;
                                        frame.args = args.clone();
                                        frame.bottom = *bottom;
                                    }
                                    Continuation::Null => {
                                        while matches!(
                                            self.frames.last().map(|f| f.kind()),
                                            Some(ExecutionKind::Bytecode { .. })
                                        ) {
                                            self.frames.pop();
                                        }
                                    }
                                },
                                _ => {
                                    make_error!(SchemeErrorType::NonCallable);
                                }
                            }
                        }
                        Bytecode::Unpack { amount } => {
                            // If arity is Exact(1), we look at the top value, and if it is a Values, we error
                            // (so make sure we only *create* values if there is *more* that 1 value to treat like this)
                            // otherwise expect a values object the matches the requested arity, and error if it doesn't
                            // w/o unpacking values
                            todo!()
                        }
                        Bytecode::Define { symbol } => {
                            // Pop the top of stack and store in env as a given symbol
                            let value = self
                                .stack
                                .pop()
                                .unwrap_or_else(|| Gc::new(&ctx, RefLock::new(Value::Undefined)));
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
                        _ => todo!(),
                    }
                }
                Execution::Native { native, bottom } => {
                    // TODO Remeber to check when [call-end] a native call to check for non-continuable errors
                    fuel.consume(Self::NATIVE_COST);
                    todo!("native code")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use gc_arena::{Gc, RefLock, arena};
    use lasso::Interner;

    use crate::{
        Fuel,
        bytecode::{self, Bytecode::*, Constant},
        compiler::World,
        environment::Environment,
        interpreter::{Context, NullIncluder},
        runtime::convert::IntoValue,
    };

    use super::Thread;

    #[test]
    fn thread_definition() {
        let mut interner = lasso::Rodeo::new();
        let world = World::default();
        let includer = NullIncluder;
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
            // FIXME Get compiler working so that this becomes a datatest
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
                ],
                [Constant::Number(3)],
                [],
                import_env,
                Default::default(),
            );

            let thread = Gc::new(mc, RefLock::new(Thread::new(mc, chunk)));
            thread
                .borrow_mut(mc)
                .env()
                .unwrap()
                .borrow_mut(mc)
                .define(
                    mc,
                    interner.get_or_intern_static("cowl"),
                    32.into_value(mc).into_ptr(mc),
                    false,
                )
                .expect("failed to define `cowl`");
            let ctx = Context::new_test_context(mc, thread);
            while !thread.borrow().is_finished() && dbg!(run_cost) < 1000 {
                thread
                    .borrow_mut(&ctx)
                    .step(ctx, &mut interner, &world, &includer, &mut fuel);
                run_cost += INITIAL_FUEL - fuel.remaining();
                fuel.refill(1000, INITIAL_FUEL);
                eprintln!("{:#?}", thread.borrow());
            }
            eprintln!("finished? {}", thread.borrow().is_finished());
            if let Some(Err(err)) = thread.borrow().result() {
                eprintln!("{}", err.display(&interner));
            } else {
                eprintln!("{:#?}", thread.borrow().result());
            }
        })
    }
}
