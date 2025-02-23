use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    Fuel,
    bytecode::{Bytecode, ChunkPtr},
    environment::{StackEnvironment, StackEnvironmentPtr},
    runtime::{convert::IntoValue, error::SchemeErrorPtr, lambda::NativeLambdaPtr},
    value::{Continuation, ValuePtr},
};

use super::Context;

/// A thread's execution state can either be interpreting bytecode or running a native function or macro
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
enum Execution<'gc> {
    Bytecode {
        chunk: ChunkPtr<'gc>,
        pc: usize,
    },
    Native {
        native: NativeLambdaPtr<'gc>,
        args: Box<[ValuePtr<'gc>]>,
    },
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
    env: StackEnvironmentPtr<'gc>,
    // TODO macro environment to hold macros
    // Because scheme code (through `define-syntax`) *can* define macros
    // there is a separate environment for them.
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
            Execution::Bytecode { chunk, pc } => Continuation::Continue {
                // a continuation of something is the *next* thing it would do
                pc: pc + 1,
                chunk,
            },
            // The other types of execution are not continuable, so create the "null continuation"
            // which when executed, causes all bytecode frames to end.
            _ => Continuation::Null,
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
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>, chunk: ChunkPtr<'gc>) -> Self {
        Self {
            stack: vec![],
            frames: vec![ThreadFrame {
                execution: Execution::Bytecode { chunk, pc: 0 },
                env: Gc::new(
                    mc,
                    RefLock::new(StackEnvironment::new(mc, Some(chunk.import_stack_env))),
                ),
                // TODO macro env
            }],
            error: None,
        }
    }

    /// Creates a frame of evaluation for included code
    ///
    /// Clobbers the current frame if `tail` is true
    pub fn include(&mut self, mc: &Mutation<'gc>, chunk: ChunkPtr<'gc>, tail: bool) {
        let env = Gc::new(
            mc,
            RefLock::new(StackEnvironment::new(mc, Some(chunk.import_stack_env))),
        );
        // TODO macro env

        if tail {
            if let Some(last) = self.frames.last_mut() {
                last.execution = Execution::Bytecode { chunk, pc: 0 };
                last.env = env;
                // TODO macro env
                return;
            }
        }

        self.frames.push(ThreadFrame {
            execution: Execution::Bytecode { chunk, pc: 0 },
            env,
        });
    }

    /// When a thread has no frames, then it is considered to be finished.
    pub fn is_finished(&self) -> bool {
        self.frames.is_empty()
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

    /// Executes instructions until fuel determines it should not continue
    pub fn step(&mut self, ctx: Context<'gc>, interner: &mut lasso::Rodeo, fuel: &mut Fuel) {
        while fuel.should_continue() {
            if self.is_finished() {
                return;
            }

            let Some(frame) = self.frames.last_mut() else {
                // Nothing to do.
                return;
            };

            // handle execution
            match &mut frame.execution {
                Execution::Bytecode { chunk, pc } => {
                    // If framepointer is oob, then that means execution of this frame is finished
                    if chunk.code.len() <= *pc {
                        self.frames.pop();
                        continue;
                    }
                    // The core of execution
                    match chunk.code[*pc] {
                        Bytecode::PushBool { bool } => {
                            if bool {
                                self.stack.push(ctx.true_value)
                            } else {
                                self.stack.push(ctx.false_value)
                            }
                        }
                        Bytecode::PushConst { index } => {
                            self.stack.push(Gc::new(
                                &ctx,
                                RefLock::new(chunk.constants[index].clone().into_value(&ctx)),
                            ));
                        }
                        _ => todo!(),
                    }
                    *pc += 1;
                }
                Execution::Native { native, args } => {
                    todo!("native code")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {}
