/*

API idea revolves around "transient unstashing"

let (stashed_context, output) = Treewalk::in_scope(&mut world, None, |ctx| {
    // ... do stuff ...
    Ok(())
}) else ...;

let (_, from_same_context) = Treewalk::in_scope(&mut world, Some(stashed_context), |ctx| {
    // ... we are refering to the same context as above ...
    Ok(42)
});

assert!(from_same_context == 42);

Signature:

fn in_scope<T>(world: &mut World, ctx: Option<StashedContext>, func: impl FnOnce(Context) -> Result<T, RuntimeError>) -> (StashedContext, Result<T, RuntimeError>);

StashedContext *must* implement Clone
*/

use std::collections::{HashMap, VecDeque};

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    world::{
        value::{Value, ValueConvertError, ValuePtr},
        World, WorldAccess, WorldArena,
    },
    Fuel,
};

use super::{Environment, EnvironmentInner, EnvironmentPtr};

// An [`Interpreter`] will always interrupt *between* modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect, Default)]
#[collect(require_static)]
pub enum InterpreterMode {
    /// ready to start execution
    #[default]
    Ready,
    /// Expanding macros
    MacroExpansion,
    /// running resulting AST
    Running,
    /// run finished, result is ready
    Result,
}

/// An interpreter that knows how to operate frames of execution
// Equivish to piccolo Executor
// an individal stack of execution.
// afaik we don't need Lua-style coroutines
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub struct Interpreter<'gc> {
    // TODO Mode might not be stored, but instead calculated
    // by looking at the runtime frame stack
    mode: InterpreterMode,
    // current environment (publically accessible)
    pub environment: EnvironmentPtr<'gc>,
    // holds the value to be returned
    return_reg: ValuePtr<'gc>,
    // used to hold frames for evaluation
    // once we reach RUNTIME_STACK_MAX, we are not allowed to use any more frames
    // or we error with a StackOverflowError
    // note that macro expansion uses slots on the stack to make them
    // resumable
    runtime_stack: Vec<RuntimeFrame<'gc>>,

    current_item: Option<ValuePtr<'gc>>,
    // continuation
    continuation: VecDeque<ValuePtr<'gc>>,
    // stack
    stack: Vec<ValuePtr<'gc>>,
}

// b/c scheme has error handling, store this as a value, but use a unique type
#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("cannot execute a null value")]
    ExecuteNull,
}

impl<'gc> Interpreter<'gc> {
    pub fn new(mc: &Mutation<'gc>, arena: &WorldArena) -> Self {
        Self {
            mode: InterpreterMode::Ready,
            environment: Gc::new(
                mc,
                RefLock::new(Environment {
                    parent: None,
                    is_frozen: false,
                    inner: Gc::new(
                        mc,
                        RefLock::new(EnvironmentInner {
                            values: HashMap::default(),
                        }),
                    ),
                }),
            ),
            return_reg: arena.null_val,
        }
    }
    pub(crate) fn step(
        &mut self,
        mc: &Mutation<'gc>,
        access: &mut WorldAccess,
        files: (),
        fuel: &mut Fuel,
    ) {
        // something...
        let mut work_done = false;
        // don't do work if we're already done
        if self.mode == InterpreterMode::Result {
            return;
        }
        while fuel.should_continue() || !work_done {
            // If the runtime stack is not empty, work on it
            // TODO runtime stack
            // TODO continue our continuation
            // pop front of continuation
            if let Some(next) = self
                .current_item
                .take()
                .or_else(|| self.continuation.pop_front())
            {
                match *next.borrow() {
                    // Value::Null => {
                    //     // TODO dynamic-wind handling?
                    //     self.stack.push(Gc::new(
                    //         mc,
                    //         RefLock::new(Value::Error(Gc::new(
                    //             mc,
                    //             InterpreterError::ExecuteNull.into(),
                    //         ))),
                    //     ))
                    // }
                    Value::Undefined => todo!(),
                    Value::Void => todo!(),
                    Value::Number(_) => todo!(),
                    Value::String(_) => todo!(),
                    Value::Symbol(_) => todo!(),
                    Value::Bool(_) => todo!(),
                    Value::Char(_) => todo!(),
                    Value::InputPort(_) => todo!(),
                    Value::OutputPort(_) => todo!(),
                    Value::Cons(_) => todo!(),
                    Value::Procedure(_) => todo!(),
                    Value::Environment(_) => todo!(),
                    Value::UserStruct(_) => todo!(),
                    Value::Error(_) => todo!(),
                }
                work_done = true;
                _ = next;
            } else {
                self.mode = InterpreterMode::Result;
            }
        }
    }
}

// altrnatte frames for the current continuation
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
enum RuntimeFrame<'gc> {
    // this frame is for evaluating Rust procedure
    Native { stack: Vec<ValuePtr<'gc>> },
    // this frame is for macro expansion
    Macro { ustack: Vec<ValuePtr<'gc>> },
}

// an important thing to remember is that it is an error to attempt to evaluate
// a circular-referential list!!!
// defines (define, define-syntax, define-record-type) are only usable in a begin context

// my idea of steps of execution
// for now this is left-to-right
// 0. import items from libraries
// 1. expand macros until no macros are valid for expansion
// 2. eval left-to-right (binding check, then special forms)

// how to handle native procedures
// 1. pass in current environment pointer, fuel, interner, and arguments from the frame if

// Ok! even more valuable is this lesson:
// special forms *have* to be importable!
// the only "special form" that exists in the environment predefined is "import"
// (not even begin is safe!! well, it's more that a top-lever structure might happen to start
// with begin)
// so what do we do?
// *reifyyyyyyy*
// special forms are to be treated the exact same as macros (hygiene and all), except
// special forms have access to reading the current Scheme environment
// and knowing their evaluation context (where they are being called)
