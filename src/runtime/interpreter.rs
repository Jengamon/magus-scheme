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

use gc_arena::{Collect, Gc};

use crate::world::value::ValuePtr;

use super::Environment;

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
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub struct Interpreter<'gc> {
    // TODO Mode might not be stored, but instead calculated
    // by looking at the runtime frame stack
    mode: InterpreterMode,
    // current environment
    environment: Environment<'gc>,
    // used to hold values on the way to procedure evaluation
    stack: ValuePtr<'gc>,
    // used to hold frames for evaluation
    // once we reach RUNTIME_STACK_MAX, we are not allowed to use any more frames
    // or we error with a StackOverflowError
    // note that macro expansion uses slots on the stack to make them
    // resumable
    frame_stack: Vec<RuntimeFrame<'gc>>,
}

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
enum RuntimeFrame<'gc> {
    // this frame is for evaluating program data
    Program {},
    // this frame is for macro expansion
    Macro { useless: Gc<'gc, ()> },
}

// difference between define

// an important thing to remember is that it is an error to attempt to evaluate
// a circular-referential list!!!
// define is only usable in a begin context

// my idea of steps of execution
// for now this is left-to-right
// 0. import items from libraries
// 1. expand macros until no macros are valid for expansion
// 2. eval left-to-right (binding check, then special forms)

// how to handle native procedures
// 1. pass in current environment pointer, fuel, interner, and arguments from the frame if
