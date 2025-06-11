use gc_arena::{Collect, Gc};

use crate::{
    Value, ValuePtr,
    runtime::{
        error::SchemeErrorPtr,
        lambda::{Arity, Lambda, LambdaResult, LambdaReturn, NativeLambda, NativeLambdaContext},
    },
};

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct WithExceptionHandler;

impl WithExceptionHandler {
    fn arguments<'gc>(args: &[ValuePtr<'gc>]) -> anyhow::Result<[Lambda<'gc>; 2]> {
        let Value::Lambda(handler) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "with-exception-handler expects a lambda as its first argument"
            ))?;
        };
        let Value::Lambda(thunk) = *args[1].borrow() else {
            return Err(anyhow::anyhow!(
                "with-exception-handler expects a lambda as its second argument"
            ))?;
        };
        if !handler.arity().is_satisfied(1) {
            return Err(anyhow::anyhow!(
                "with-exception-handler expects a 1-arity lambda as its first argument"
            ))?;
        }
        if !thunk.arity().is_satisfied(0) {
            return Err(anyhow::anyhow!(
                "with-exception-handler expects a 0-arity lambda as its second argument"
            ))?;
        }

        Ok([handler, thunk])
    }
}

impl<'gc> NativeLambda<'gc> for WithExceptionHandler {
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> LambdaResult<'gc> {
        // eprintln!("Calling as weh run");
        let [_handler, thunk] = Self::arguments(args)?;
        if ctx.stack.is_empty() {
            Ok(LambdaReturn::Call {
                lambda: thunk,
                args: Vec::new(),
                dynamic_wind: None,
                env: None,
            })
        } else {
            Ok(LambdaReturn::Return(vec![ctx.stack[0]]))
        }
    }

    fn error(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
        err: SchemeErrorPtr<'gc>,
    ) -> LambdaResult<'gc> {
        // eprintln!("Calling as weh err {err:?}");
        let [handler, _thunk] = Self::arguments(args)?;
        // TODO An error must capture its continuation for support of raise-continuation (?)
        if ctx.stack.is_empty() {
            let arg = err
                .error_type
                .value()
                .unwrap_or_else(|| Value::Error(err).into_ptr(&ctx));
            Ok(LambdaReturn::CallHandler {
                lambda: handler,
                exception: err,
                args: vec![arg],
                dynamic_wind: None,
            })
        } else {
            let Some(exception) = ctx.frames.last().unwrap().exception() else {
                // side effect of CallHandler should be storing the *original* error in frame exception
                unreachable!()
            };

            if Gc::ptr_eq(err, exception) {
                // This should only be hit in non-continuable exceptions? but we arent checking here unless necessary
                Ok(LambdaReturn::ReturnHandler)
            } else {
                Ok(LambdaReturn::Propagate(err))
            }
        }
    }
}
