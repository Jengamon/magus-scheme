// TODO Split, if this file gets too large, into separate files
pub use comparison::{Ascending, Descending, Equal, MonotonicAscending, MonotonicDescending};
pub use control::{Apply, CallCc};
pub use equality::{IsEq, IsEqv};
pub use list::{Caar, Cadr, Car, Cdar, Cddr, Cdr};
pub use math::{Add, Mul, Subtract};
pub use predicates::{IsNull, IsPair};
pub use structure::{Cons, Values};

mod equality {
    //! defines eq? and eqv?
    use gc_arena::{Collect, Gc};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    };

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct IsEq;

    impl NativeLambda for IsEq {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Return(vec![
                Value::Bool(match *args[0].borrow() {
                    // we have to override pointer equality for booleans and symbols
                    Value::Bool(b) => matches!(*args[1].borrow(), Value::Bool(ob) if b == ob),
                    Value::Symbol(sym) => {
                        matches!(*args[1].borrow(), Value::Symbol(osym) if sym == osym)
                    }
                    _ => Gc::ptr_eq(args[0], args[1]),
                })
                .into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct IsEqv;

    impl NativeLambda for IsEqv {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Return(vec![
                Value::Bool(args[0] == args[1]).into_ptr(&ctx),
            ]))
        }
    }
}

mod control {
    use crate::{
        Value,
        runtime::{
            convert::IntoValue as _,
            lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        },
    };
    use either::Either;
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
            let cont = ctx.thread_ref.create_continuation(&ctx, true);

            let arg = args.first();
            let Some(Value::Lambda(lambda)) = arg.map(|p| *p.borrow()) else {
                return Err(anyhow::anyhow!(
                    "call-with-current-continuation must be given a lambda, was given {:?}",
                    arg.map(|p| p.borrow().value_type())
                )
                .into());
            };

            if !ctx.get_arity(self, lambda).is_satisfied(1) {
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

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct Apply;

    impl NativeLambda for Apply {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let call = match *args[0].borrow() {
                Value::Lambda(l) => Either::Left(l),
                Value::Continuation(c) => Either::Right(c),
                _ => Err(anyhow::anyhow!(
                    "apply must be given a callable as the first argument"
                ))?,
            };

            // The last argument is a list that we have to unwrap
            let args_append = match args.last().map(|a| (a, *a.borrow())) {
                // the proc was the only argument
                Some((_, Value::Lambda(_))) => {
                    vec![]
                }
                Some((sptr, Value::Cons(cons)))
                    if cons.is_list(*sptr, ctx.thread_ctx.null_value) =>
                {
                    // we unwrap the cons (since it is non-cyclical)
                    let mut args = vec![];
                    let mut current = cons;

                    loop {
                        // push car, and set cons to tail *or* break on non-list cdr
                        // break on null cdr
                        args.push(current.car.unwrap_or(ctx.thread_ctx.null_value));
                        match current.cdr.map(|v| *v.borrow()) {
                            Some(Value::Cons(_))
                                if Gc::ptr_eq(current.cdr.unwrap(), ctx.thread_ctx.null_value) =>
                            {
                                break;
                            }
                            Some(Value::Cons(c)) => {
                                current = c;
                            }
                            Some(_) => {
                                unreachable!("must be a valid list")
                            }
                            None => {
                                // None is null_value lite
                                break;
                            }
                        }
                    }

                    args
                }
                // Non-list cons and other values are just treated as the last argument
                Some((ptr, _)) => {
                    vec![*ptr]
                }
                None => unreachable!("arity of 1"),
            };
            let args_len = args.len() - 1;

            let args = args
                .iter()
                .skip(1)
                .take(args_len.saturating_sub(1))
                .copied()
                .chain(args_append)
                .collect();
            Ok(match call {
                Either::Left(lambda) => LambdaReturn::TailCall {
                    lambda,
                    args,
                    dynamic_wind: None,
                },
                Either::Right(cont) => LambdaReturn::Continue { cont, args },
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
        value::NumberPtr,
    };

    // From R7RS Report:
    // If any of the arguments
    // are +nan.0, all the predicates return #f. They do not dis-
    // tinguish between inexact zero and inexact negative zero.
    // These predicates are required to be transitive.

    // Helper datatypes and functions
    // TODO Go from Number to ComplexNumber eventually
    type Num<'gc> = Either<NumberPtr<'gc>, f64>;
    fn convert(p: ValuePtr<'_>) -> Result<Num, anyhow::Error> {
        Ok(match *p.borrow() {
            Value::Number(i) => Either::Left(i),
            Value::Inexact(f) => Either::Right(f),
            _ => Err(anyhow::anyhow!("cannot compare non-numeric values"))?,
        })
    }

    // If `None`, one of the numbers is a NaN
    fn compare<'gc>(lhs: &Num<'gc>, rhs: &Num<'gc>) -> Option<std::cmp::Ordering> {
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
                            return Ok(LambdaReturn::Return(vec![ctx.thread_ctx.false_value]));
                        }
                    };

                    let mut last_elem = args_converted[1];
                    for rhs in args_converted.into_iter().skip(2) {
                        is_valid &= match compare(&last_elem, &rhs) {
                            Some(ord) => ($cmp)(ord),
                            None => {
                                return Ok(LambdaReturn::Return(vec![ctx.thread_ctx.false_value]));
                            }
                        };
                        // We can shortcut and return here b/c we know that once we are false we are *always* false
                        if !is_valid {
                            return Ok(LambdaReturn::Return(vec![ctx.thread_ctx.false_value]));
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
        value::Number,
    };
    use either::Either;
    use gc_arena::{Collect, Gc};

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
            let mut result = Either::Left(Number::ZERO);
            for arg in args {
                result = if let Value::Number(n) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Left(&l + &*n),
                        Either::Right(f) => Either::Right(f + n.to_inexact()),
                    }
                } else if let Value::Inexact(f) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Right(l.to_inexact() + f),
                        Either::Right(l) => Either::Right(l + f),
                    }
                } else {
                    Err(anyhow::anyhow!("cannot add something that is not a number"))?
                };
            }
            Ok(LambdaReturn::Return(vec![
                match result {
                    Either::Left(num) => Value::Number(num.into_ptr(&ctx)),
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
                    Value::Number(num) => Value::Number(Gc::new(&ctx, -&*num)),
                    Value::Inexact(flt) => Value::Inexact(-flt),
                    _ => Err(anyhow::anyhow!(
                        "cannot negate something that is not a number"
                    ))?,
                };
                return Ok(LambdaReturn::Return(vec![value.into_ptr(&ctx)]));
            }

            let mut result = match *args[0].borrow() {
                Value::Number(num) => Either::Left((*num).clone()),
                Value::Inexact(flt) => Either::Right(flt),
                _ => Err(anyhow::anyhow!(
                    "cannot subtract something that is not a number"
                ))?,
            };

            for arg in args.iter().skip(1) {
                result = if let Value::Number(n) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Left(&l - &*n),
                        Either::Right(f) => Either::Right(f - n.to_inexact()),
                    }
                } else if let Value::Inexact(f) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Right(l.to_inexact() - f),
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
                    Either::Left(num) => Value::Number(num.into_ptr(&ctx)),
                    Either::Right(flt) => Value::Inexact(flt),
                }
                .into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Mul;

    impl NativeLambda for Mul {
        fn arity(&self) -> Arity {
            Arity::AtLeast(0)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let mut result = Either::Left(Number::one());
            for arg in args {
                result = if let Value::Number(n) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Left(&l * &*n),
                        Either::Right(f) => Either::Right(f * n.to_inexact()),
                    }
                } else if let Value::Inexact(f) = *arg.borrow() {
                    match result {
                        Either::Left(l) => Either::Right(l.to_inexact() * f),
                        Either::Right(l) => Either::Right(l * f),
                    }
                } else {
                    Err(anyhow::anyhow!(
                        "cannot multiply something that is not a number"
                    ))?
                };
            }
            Ok(LambdaReturn::Return(vec![
                match result {
                    Either::Left(num) => Value::Number(num.into_ptr(&ctx)),
                    Either::Right(flt) => Value::Inexact(flt),
                }
                .into_ptr(&ctx),
            ]))
        }
    }
}

mod list {
    use gc_arena::{Collect, Gc};

    use crate::{
        Value, ValuePtr,
        runtime::lambda::{Arity, LambdaResult, LambdaReturn, NativeLambda, NativeLambdaContext},
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Car;
    impl NativeLambda for Car {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("car only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("car only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.car.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Caar;
    impl NativeLambda for Caar {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("caar only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("caar only operates on a pair"))?;
            };

            let Some(Value::Cons(c)) = c
                .car
                .filter(|p| !Gc::ptr_eq(*p, ctx.thread_ctx.null_value))
                .map(|p| *p.borrow())
            else {
                return Err(anyhow::anyhow!("caar only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.car.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cadr;
    impl NativeLambda for Cadr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("cadr only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("cadr only operates on a pair"))?;
            };

            let Some(Value::Cons(c)) = c
                .cdr
                .filter(|p| !Gc::ptr_eq(*p, ctx.thread_ctx.null_value))
                .map(|p| *p.borrow())
            else {
                return Err(anyhow::anyhow!("cadr only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.car.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cdar;
    impl NativeLambda for Cdar {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("cdar only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("cdar only operates on a pair"))?;
            };

            let Some(Value::Cons(c)) = c
                .car
                .filter(|p| !Gc::ptr_eq(*p, ctx.thread_ctx.null_value))
                .map(|p| *p.borrow())
            else {
                return Err(anyhow::anyhow!("cdar only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.cdr.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cddr;
    impl NativeLambda for Cddr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("cddr only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("cddr only operates on a pair"))?;
            };

            let Some(Value::Cons(c)) = c
                .cdr
                .filter(|p| !Gc::ptr_eq(*p, ctx.thread_ctx.null_value))
                .map(|p| *p.borrow())
            else {
                return Err(anyhow::anyhow!("cddr only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.cdr.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cdr;
    impl NativeLambda for Cdr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            if args[0] == ctx.thread_ctx.null_value {
                return Err(anyhow::anyhow!("cdr only operates on a pair"))?;
            }

            let Value::Cons(c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("cdr only operates on a pair"))?;
            };

            Ok(LambdaReturn::Return(vec![
                c.cdr.unwrap_or(ctx.thread_ctx.null_value),
            ]))
        }
    }
}

mod predicates {
    //! Scheme typechecking stuff
    use gc_arena::{Collect, Gc};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsPair;

    impl NativeLambda for IsPair {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let val = match *args[0].borrow() {
                Value::Cons(_) if Gc::ptr_eq(args[0], ctx.thread_ctx.null_value) => false,
                Value::Cons(_) => true,
                _ => false,
            };

            Ok(LambdaReturn::Return(vec![Value::Bool(val).into_ptr(&ctx)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsNull;

    impl NativeLambda for IsNull {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO Do we allow the degenerate case of a cons cell with (None None)?
            // It does mean that we can just do a pointer comparison...
            //
            // I think no, for the stdlib, the only pair considered to be null is the thread null value
            let val = matches!(*args[0].borrow(), Value::Cons(_) if Gc::ptr_eq(args[0], ctx.thread_ctx.null_value));

            Ok(LambdaReturn::Return(vec![Value::Bool(val).into_ptr(&ctx)]))
        }
    }
}

mod structure {
    use gc_arena::{Collect, Gc};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaReturn, NativeLambda},
    };

    // Stuff like cons and values

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Values;

    impl NativeLambda for Values {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            Ok(LambdaReturn::Return(vec![
                Value::Values(Gc::new(&ctx, args.to_vec())).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cons;

    impl NativeLambda for Cons {
        fn arity(&self) -> crate::runtime::lambda::Arity {
            Arity::Exact(2)
        }

        fn run<'gc>(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            Ok(crate::runtime::lambda::LambdaReturn::Return(vec![
                Value::Cons(crate::value::ConsCell {
                    car: Some(args[0]),
                    cdr: Some(args[1]),
                })
                .into_ptr(&ctx),
            ]))
        }
    }
}
