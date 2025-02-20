use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    compiler::StandardChunkPtr,
    environment::{StackEnvironment, StackEnvironmentPtr},
    runtime::error,
    value::ValuePtr,
    Fuel,
};

/// A thread's execution state can either be interpreting bytecode or running a native function
#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
enum Execution<'gc> {
    Bytecode(StandardChunkPtr<'gc>),
    // TODO native function
    Native,
}

/// Controls what location a thread will go to next
#[derive(Debug, Collect, Clone, Copy, Default)]
#[collect(no_drop)]
struct Continuation<'gc> {
    /// If set, the next step will set [`Thread::pc`] before stepping
    next_pc: Option<usize>,
    /// If set, the next step will set [`Thread::execution`] to refer to this chunk
    /// before stepping
    next_chunk: Option<StandardChunkPtr<'gc>>,
}

#[derive(Debug, Collect, Clone, Copy)]
#[collect(no_drop)]
struct ThreadFrame<'gc> {
    /// Current program counter
    pc: usize,
    execution: Execution<'gc>,
    continuation: Continuation<'gc>,
    #[collect(require_static)]
    debug_info: error::StackFrame,
    env: StackEnvironmentPtr<'gc>,
    // TODO macro environment to execute macros
}

pub type ThreadPtr<'gc> = Gc<'gc, RefLock<Thread<'gc>>>;
/// Runs all code
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc> {
    stack: Vec<ValuePtr<'gc>>,
    frames: Vec<ThreadFrame<'gc>>,
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>, chunk: StandardChunkPtr<'gc>) -> Self {
        Self {
            stack: vec![],
            frames: vec![ThreadFrame {
                pc: 0,
                execution: Execution::Bytecode(chunk),
                continuation: Continuation::default(),
                debug_info: error::StackFrame {
                    range: Some(chunk.source_data.range()),
                    scope_label: None,
                    source_filename: Some(chunk.source_data.source_id()),
                },
                env: Gc::new(mc, RefLock::new(StackEnvironment::new(mc, None))),
            }],
        }
    }

    /// Executes instructions until fuel determines it should not continue
    pub fn step(&mut self, mc: &Mutation<'gc>, interner: &mut lasso::Rodeo, fuel: &mut Fuel) {
        while fuel.should_continue() {
            let Some(frame) = self.frames.last_mut() else {
                // Nothing to do.
                return;
            };

            // handle continuation
            if let Some(chunk) = frame.continuation.next_chunk.take() {
                frame.execution = Execution::Bytecode(chunk);
            }

            if let Some(pc) = frame.continuation.next_pc.take() {
                frame.pc = pc;
            }

            // handle execution
            match self.frames.last().map(|f| &f.execution) {
                Some(Execution::Bytecode(chunk)) => {
                    todo!()
                }
                Some(Execution::Native) => {
                    todo!("native code")
                }
                None => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {}
