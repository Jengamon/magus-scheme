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

use gc_arena::Collect;

use crate::world::value::ValuePtr;

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
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub struct Interpreter<'gc> {
    mode: InterpreterMode,
    stack: ValuePtr<'gc>,
}
