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
