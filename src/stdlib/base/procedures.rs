use either::Either;
use gc_arena::{Collect, Gc};

// TODO Split, if this file gets too large, into concerns

use crate::{
    Value,
    runtime::{
        convert::IntoValue as _,
        lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    },
};

#[derive(Collect, Debug)]
#[collect(require_static)]
pub struct CallCc;

impl NativeLambda for CallCc {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run<'gc>(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        // get the continuation of the stack frame right above us
        let cont = ctx.thread_ref.create_continuation(true);

        let arg = args.first();
        let Some(Value::Lambda(lambda)) = arg.map(|p| *p.borrow()) else {
            return Err(anyhow::anyhow!(
                "call-with-current-continuation must be given a lambda, was given {:?}",
                arg.map(|p| p.borrow().value_type())
            )
            .into());
        };

        if ctx.get_arity(self, lambda).is_satisfied(1) {
            return Err(anyhow::anyhow!(
                "call-with-current-continuation must be given a 1-arity lambda"
            )
            .into());
        }

        Ok(LambdaReturn::TailCall {
            lambda,
            args: vec![Gc::new(&ctx, cont).into_value(&ctx).into_ptr(&ctx)],
            dynamic_wind: None,
        })
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Add;

impl NativeLambda for Add {
    fn arity(&self) -> Arity {
        Arity::AtLeast(0)
    }

    fn run<'gc>(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let mut result = Either::Left(0);
        for arg in args {
            result = if let Value::Number(n) = *arg.borrow() {
                match result {
                    Either::Left(l) => Either::Left(l + n),
                    Either::Right(f) => Either::Right(f + n as f64),
                }
            } else if let Value::Inexact(f) = *arg.borrow() {
                match result {
                    Either::Left(l) => Either::Right(l as f64 + f),
                    Either::Right(l) => Either::Right(l + f),
                }
            } else {
                Err(anyhow::anyhow!("cannot add something that is not a number"))?
            };
        }
        Ok(LambdaReturn::Return(vec![
            match result {
                Either::Left(num) => Value::Number(num),
                Either::Right(flt) => Value::Inexact(flt),
            }
            .into_ptr(&ctx),
        ]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Subtract;

impl NativeLambda for Subtract {
    fn arity(&self) -> Arity {
        Arity::AtLeast(1)
    }

    fn run<'gc>(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        if args.len() == 1 {
            let value = match *args[0].borrow() {
                Value::Number(num) => Value::Number(-num),
                Value::Inexact(flt) => Value::Inexact(-flt),
                _ => Err(anyhow::anyhow!(
                    "cannot negate something that is not a number"
                ))?,
            };
            return Ok(LambdaReturn::Return(vec![value.into_ptr(&ctx)]));
        }

        let mut result = match *args[0].borrow() {
            Value::Number(num) => Either::Left(num),
            Value::Inexact(flt) => Either::Right(flt),
            _ => Err(anyhow::anyhow!(
                "cannot subtract something that is not a number"
            ))?,
        };

        for arg in args.iter().skip(1) {
            result = if let Value::Number(n) = *arg.borrow() {
                match result {
                    Either::Left(l) => Either::Left(l - n),
                    Either::Right(f) => Either::Right(f - n as f64),
                }
            } else if let Value::Inexact(f) = *arg.borrow() {
                match result {
                    Either::Left(l) => Either::Right(l as f64 - f),
                    Either::Right(l) => Either::Right(l - f),
                }
            } else {
                Err(anyhow::anyhow!("cannot add something that is not a number"))?
            };
        }

        Ok(LambdaReturn::Return(vec![
            match result {
                Either::Left(num) => Value::Number(num),
                Either::Right(flt) => Value::Inexact(flt),
            }
            .into_ptr(&ctx),
        ]))
    }
}
