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

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    value::ConsCell,
    world::{
        value::{Value, ValuePtr},
        WorldArena,
    },
    Fuel,
};

use super::{Environment, EnvironmentPtr, Error};

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

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub struct Continuation<'gc> {
    // there's only one pointer!
    execution: Option<ValuePtr<'gc>>,
    pub stack: Vec<ValuePtr<'gc>>,
    runtime_stack: Vec<RuntimeFrame<'gc>>,
    pub environment: EnvironmentPtr<'gc>,
}

impl<'gc> Continuation<'gc> {
    pub fn new(
        execution: ValuePtr<'gc>,
        stack: Vec<ValuePtr<'gc>>,
        runtime_stack: Vec<RuntimeFrame<'gc>>,
        environment: EnvironmentPtr<'gc>,
    ) -> Self {
        Self {
            execution: Some(execution),
            stack,
            runtime_stack,
            environment,
        }
    }

    /// A continuation with no value to execute, which causes a return of the value of
    /// the result of the current continuation
    pub fn result(
        stack: Vec<ValuePtr<'gc>>,
        runtime_stack: Vec<RuntimeFrame<'gc>>,
        environment: EnvironmentPtr<'gc>,
    ) -> Self {
        Self {
            execution: None,
            stack,
            runtime_stack,
            environment,
        }
    }
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
    // holds the value to be returned
    return_reg: ValuePtr<'gc>,
    // used to hold frames for evaluation
    // once we reach RUNTIME_STACK_MAX, we are not allowed to use any more frames
    // or we error with a StackOverflowError
    // note that macro expansion uses slots on the stack to make them
    // resumable

    // current_item: Option<ValuePtr<'gc>>,
    // continuation
    // continuation: VecDeque<ValuePtr<'gc>>,
    // FIXME make continuation the current state (stack and runtime stack) along with the next value to execute
    // stack
    // stack: Vec<ValuePtr<'gc>>,
    // runtime_stack: Vec<RuntimeFrame<'gc>>,
    continuation: Continuation<'gc>,
}

// b/c scheme has error handling, store this as a value, but use a unique type
#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("cannot execute a null value")]
    ExecuteNull,
}

impl<'gc> Interpreter<'gc> {
    pub fn new(mc: &Mutation<'gc>, arena: &WorldArena<'gc>, initial_exec: ValuePtr<'gc>) -> Self {
        Self {
            mode: InterpreterMode::Ready,
            return_reg: arena.null_val,
            continuation: Continuation::new(
                initial_exec,
                Vec::new(),
                Vec::new(),
                Gc::new(mc, RefLock::new(Environment::new(mc, None))),
            ),
        }
    }

    pub fn step(&mut self, mc: &Mutation<'gc>, arena: &mut WorldArena<'gc>, fuel: &mut Fuel) {
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
            if let Some(exec) = self.continuation.execution {
                match *exec.borrow() {
                    // attempting to "execute" or return this produces an error!
                    // (also applies if an environment contains this value and it is being returned
                    // by symbol)
                    Value::Undefined => todo!(),
                    // symbol means to look up the value in the current environment, and
                    // put *that* into the return register (does *not* execute cons-cells!)
                    Value::Symbol(_) => todo!(),
                    // There are 2 forms of executable lists:
                    // after macro expansion
                    // - initial item is a *Procedure*. the args are evaluated and passed into the procedure as a continuation.
                    // - initial item is a symbol, in which case the value of the symbol is looked up in the environment
                    // and must be a Procedure for execution.
                    // - initial item is the symbol "begin", where there is special handling for the fact that begin is
                    // a sequence
                    Value::Cons(cons) => {
                        // expand macros
                        // execute procedure
                        // store procedure result in result_reg
                    }
                    // most values, if they end up in an execution slot,
                    // mean "set the return value of this context to this value"
                    // is just means to set the return_reg to the value given
                    Value::Void => todo!(),
                    Value::Bool(_) => todo!(),
                    Value::Char(_) => todo!(),
                    Value::InputPort(_) => todo!(),
                    Value::OutputPort(_) => todo!(),
                    Value::Number(_) => todo!(),
                    Value::String(_) => todo!(),
                    Value::Procedure(_) => todo!(),
                    Value::Environment(_) => todo!(),
                    Value::UserStruct(_) => todo!(),
                    Value::Error(_) => todo!(),
                    Value::Vector(_) => todo!(),
                }
                work_done = true;
            } else {
                self.mode = InterpreterMode::Result;
            }
        }
    }

    // attempt to expand macros on children of this cons recursively
    fn expand(
        &mut self,
        mc: &Mutation<'gc>,
        arena: &mut WorldArena<'gc>,
    ) -> Result<ValuePtr<'gc>, Error<'gc>> {
        // we expand macros for children bottom-up, right-to-left
        // so something like:
        // (argboy 3 (lambda (x) (+ x 1)))
        // if we have defined
        // (define-syntax argboy (syntax-rules () ((argboy v lam) (lam v))))
        // we expand the `lambda` first, into a procedure, then
        // we expand `argboy`.
        //
        // Any non-cons value is copied to the output as-is.
        //
        // NOTE We currently do not support expanding self-referetial lists, as they are not executable,
        // so if we encounter a cons that references itself, it is copied to the output *as-is*.
        //
        // NOTE If we don't find a macro for a cons, we copy the cons cell as-is. (distinct from
        // *finding* a macro, but its expansion failing)
        todo!()
    }
}

// altrnatte frames for the current continuation
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub enum RuntimeFrame<'gc> {
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
