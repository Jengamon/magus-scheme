// TODO Split, if this file gets too large, into separate files
pub use comparison::{Ascending, Descending, Equal, MonotonicAscending, MonotonicDescending};
pub use control::CallCc;
pub use math::{Add, Subtract};

mod control {
    use crate::{
        Value,
        runtime::{
            convert::IntoValue as _,
            lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        },
    };
    use gc_arena::{Collect, Gc};

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
}

mod comparison {
    use either::Either;
    use gc_arena::Collect;

    use crate::{
        Value, ValuePtr,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    };

    // From R7RS Report:
    // If any of the arguments
    // are +nan.0, all the predicates return #f. They do not dis-
    // tinguish between inexact zero and inexact negative zero.
    // These predicates are required to be transitive.

    // Helper datatypes and functions
    type Num = Either<i64, f64>;
    fn convert(p: ValuePtr<'_>) -> Result<Num, anyhow::Error> {
        Ok(match *p.borrow() {
            Value::Number(i) => Either::Left(i),
            Value::Inexact(f) => Either::Right(f),
            _ => Err(anyhow::anyhow!("cannot compare non-numeric values"))?,
        })
    }

    // If `None`, one of the numbers is a NaN
    fn compare(lhs: &Num, rhs: &Num) -> Option<std::cmp::Ordering> {
        lhs.partial_cmp(rhs)
    }

    macro_rules! comparison_impl {
        ($cmp:expr => $tp:ty) => {
            impl NativeLambda for $tp {
                fn arity(&self) -> Arity {
                    Arity::AtLeast(2)
                }

                fn run<'gc>(
                    &mut self,
                    ctx: NativeLambdaContext<'_, 'gc>,
                    args: &[ValuePtr<'gc>],
                ) -> Result<LambdaReturn<'gc>, LambdaError> {
                    let args_converted = args
                        .iter()
                        .copied()
                        .map(convert)
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut is_valid = match compare(&args_converted[0], &args_converted[1]) {
                        Some(ord) => ($cmp)(ord),
                        None => {
                            return Ok(LambdaReturn::Return(vec![ctx.ctx.false_value]));
                        }
                    };

                    let mut last_elem = args_converted[1];
                    for rhs in args_converted.into_iter().skip(2) {
                        is_valid &= match compare(&last_elem, &rhs) {
                            Some(ord) => ($cmp)(ord),
                            None => {
                                return Ok(LambdaReturn::Return(vec![ctx.ctx.false_value]));
                            }
                        };
                        // We can shortcut and return here b/c we know that once we are false we are *always* false
                        if !is_valid {
                            return Ok(LambdaReturn::Return(vec![ctx.ctx.false_value]));
                        }
                        last_elem = rhs;
                    }

                    // there could be not addition elements beyond the initial 2, so this should still use the variable we
                    // set up
                    Ok(LambdaReturn::Return(vec![
                        Value::Bool(is_valid).into_ptr(&ctx),
                    ]))
                }
            }
        };
    }

    // =
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Equal;
    comparison_impl!(|ord| ord == std::cmp::Ordering::Equal => Equal);

    // >= ("monotonically non-ascending")
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Descending;
    comparison_impl!(|ord| matches!(ord, std::cmp::Ordering::Greater | std::cmp::Ordering::Equal) => Descending);

    // <= ("monotonically non-descending")
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Ascending;
    comparison_impl!(|ord| matches!(ord, std::cmp::Ordering::Less | std::cmp::Ordering::Equal) => Ascending);

    // >
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct MonotonicDescending;
    comparison_impl!(|ord| ord == std::cmp::Ordering::Greater => MonotonicDescending);

    // <
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct MonotonicAscending;
    comparison_impl!(|ord| ord == std::cmp::Ordering::Less => MonotonicAscending);
}

mod math {
    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    };
    use either::Either;
    use gc_arena::Collect;

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
            let mut result = Either::Left(0i64);
            for arg in args {
                result = if let Value::Number(n) = *arg.borrow() {
                    match result {
                        Either::Left(l) => l
                            .checked_add(n)
                            .map(Either::Left)
                            .ok_or(anyhow::anyhow!("integer overflow"))?,
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
                        Either::Left(l) => l
                            .checked_sub(n)
                            .map(Either::Left)
                            .ok_or(anyhow::anyhow!("integer overflow"))?,
                        Either::Right(f) => Either::Right(f - n as f64),
                    }
                } else if let Value::Inexact(f) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Right(l as f64 - f),
                        Either::Right(l) => Either::Right(l - f),
                    }
                } else {
                    Err(anyhow::anyhow!(
                        "cannot subtract something that is not a number"
                    ))?
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
}
