// TODO Split, if this file gets too large, into separate files
pub use comparison::{Ascending, Descending, Equal, MonotonicAscending, MonotonicDescending};
pub use control::{Apply, CallCc, Features};
pub use conversions::{
    CharToInteger, Exact, Inexact, IntegerToChar, ListToString, NumberToString, StringToList,
    StringToNumber, StringToSymbol, SymbolToString,
};
pub use equality::{IsEq, IsEqual, IsEqv};
pub use error::{Raise, RaiseContinuable};
pub use list::{Caar, Cadr, Car, Cdar, Cddr, Cdr, ListCopy, ListSetBang, Map};
pub use math::{
    Add, Denominator, Divide, ExactIntegerSqrt, Expt, Gcd, Lcm, Multiply, Numerator, Subtract,
};
pub use predicates::{
    IsEven, IsExact, IsExactInteger, IsInexact, IsInteger, IsList, IsNull, IsOdd, IsPair,
    IsProcedure, IsString, IsSymbol, IsVector,
};
pub use structure::{CallWithValues, Cons, Values};
pub use vector::VectorRef;

mod conversions;

mod error {
    use gc_arena::Collect;

    use crate::runtime::lambda::{
        Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Raise;

    impl<'gc> NativeLambda<'gc> for Raise {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            _ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Raise {
                error: args[0],
                is_continuable: false,
            })
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct RaiseContinuable;

    impl<'gc> NativeLambda<'gc> for RaiseContinuable {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            _ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Raise {
                error: args[0],
                is_continuable: true,
            })
        }
    }
}

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

    impl<'gc> NativeLambda<'gc> for IsEq {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
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

    impl<'gc> NativeLambda<'gc> for IsEqv {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Return(vec![
                Value::Bool(args[0] == args[1]).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct IsEqual;

    impl<'gc> NativeLambda<'gc> for IsEqual {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            Ok(LambdaReturn::Return(vec![
                Value::Bool(args[0].borrow().is_equal(*args[1].borrow())).into_ptr(&ctx),
            ]))
        }
    }
}

mod control {
    use std::sync::Arc;

    use crate::{
        Value, compiler,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::ConsCell,
    };
    use either::Either;
    use gc_arena::{Collect, Gc};

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct CallCc;

    impl<'gc> NativeLambda<'gc> for CallCc {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // get the continuation of the stack frame right above us
            let cont = ctx.thread_ref.create_continuation(&ctx, true);

            let cont_value = Value::Continuation(Gc::new(&ctx, cont)).into_ptr(&ctx);

            match *args[0].borrow() {
                Value::Lambda(lambda) => {
                    if !ctx.get_arity(self, lambda).is_satisfied(1) {
                        return Err(anyhow::anyhow!(
                    "call-with-current-continuation must be given a 1-arity lambda as its first argument"
                )
                .into());
                    }

                    Ok(LambdaReturn::TailCall {
                        lambda,
                        args: vec![cont_value],
                        dynamic_wind: None,
                    })
                }
                Value::Continuation(cont) => Ok(LambdaReturn::Continue {
                    cont,
                    args: vec![cont_value],
                }),
                _ => Err(anyhow::anyhow!(
                    "call-with-current-continuation expects a procedure as its first argument",
                ))?,
            }
        }
    }

    #[derive(Collect, Debug)]
    #[collect(require_static)]
    pub struct Apply;

    impl<'gc> NativeLambda<'gc> for Apply {
        fn arity(&self) -> Arity {
            Arity::AtLeast(2)
        }

        fn run(
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
                Some((sptr, Value::Cons(cons)))
                    if cons.is_list(*sptr, ctx.thread_ctx.null_value) =>
                {
                    cons.list_values(*sptr, ctx.thread_ctx.null_value)
                        .into_iter()
                        .collect::<Vec<_>>()
                }
                // Non-list cons and other values cause an error
                Some((_, _)) => Err(anyhow::anyhow!(
                    "apply expects a proper list as its last argument"
                ))?,
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

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Features {
        additional_features: Arc<[Arc<str>]>,
    }

    impl From<Arc<[Arc<str>]>> for Features {
        fn from(value: Arc<[Arc<str>]>) -> Self {
            Self {
                additional_features: value,
            }
        }
    }

    impl<'gc> NativeLambda<'gc> for Features {
        fn arity(&self) -> Arity {
            Arity::Exact(0)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            _args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let mut features = compiler::Compiler::base_features(ctx.interner);
            features.extend(
                self.additional_features
                    .iter()
                    .map(|s| ctx.interner.get_or_intern(s)),
            );

            let list = ConsCell::from_iter(
                &ctx,
                ctx.thread_ctx.null_value,
                features
                    .into_iter()
                    .map(|s| Value::Symbol(s.into()).into_ptr(&ctx))
                    .collect::<Vec<_>>(),
            );

            Ok(LambdaReturn::Return(vec![list]))
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
    // TODO Add Complex, InexactComplex support
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
        match (lhs, rhs) {
            (Either::Left(e), Either::Left(e2)) => Some(e.cmp(e2)),
            (Either::Right(i), Either::Right(i2)) => i.partial_cmp(i2),
            (Either::Left(e), Either::Right(i2)) => e.to_inexact().partial_cmp(i2),
            (Either::Right(i), Either::Left(e2)) => i.partial_cmp(&e2.to_inexact()),
        }
    }

    macro_rules! comparison_impl {
        ($cmp:expr => $tp:ty) => {
            impl<'gc> NativeLambda<'gc> for $tp {
                fn arity(&self) -> Arity {
                    Arity::AtLeast(2)
                }

                fn run(
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
    // TODO Add Complex, InexactComplex support
    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::Number,
    };
    use either::Either;
    use gc_arena::{Collect, Gc};
    use num::{BigInt, FromPrimitive, Zero};

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Expt;

    impl<'gc> NativeLambda<'gc> for Expt {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // So if our *second* argument is a non-negative exact integer, we preserve exactness,
            // otherwise we return an inexact b/c figuring out how to do roots is not what I wanna do.
            // (If the value is negative and the power even, we don't support it yet as we don't support complex numbers)
            todo!("expt")
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct ExactIntegerSqrt;

    impl<'gc> NativeLambda<'gc> for ExactIntegerSqrt {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // Does *not* need complex support as it *only* supports integers
            let Value::Number(n) = *args[0].borrow() else {
                return Err(anyhow::anyhow!(
                    "exact-integer-sqrt expects an integer as its argument"
                ))?;
            };

            let (num, root) = match &*n {
                n @ Number::Integer(i) if n.is_positive() || n.is_zero() => {
                    (i.clone(), num::integer::sqrt(i.clone()))
                }
                Number::Integer(_) => Err(anyhow::anyhow!(
                    "exact-integer-sqrt expects a positive integer as its argument"
                ))?,
                _ => Err(anyhow::anyhow!(
                    "exact-integer-sqrt expects an integer as its argument"
                ))?,
            };
            let remainder = num - &root * &root;

            Ok(LambdaReturn::Return(vec![
                Value::Number(Gc::new(&ctx, Number::Integer(root))).into_ptr(&ctx),
                Value::Number(Gc::new(&ctx, Number::Integer(remainder))).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Add;

    impl<'gc> NativeLambda<'gc> for Add {
        fn arity(&self) -> Arity {
            Arity::AtLeast(0)
        }

        fn run(
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

    impl<'gc> NativeLambda<'gc> for Subtract {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
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
    pub struct Multiply;

    impl<'gc> NativeLambda<'gc> for Multiply {
        fn arity(&self) -> Arity {
            Arity::AtLeast(0)
        }

        fn run(
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

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Divide;

    impl<'gc> NativeLambda<'gc> for Divide {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args.len() == 1 {
                let value = match *args[0].borrow() {
                    Value::Number(num) => {
                        if num.is_zero() {
                            return Err(anyhow::anyhow!("cannot divide by 0"))?;
                        }

                        Value::Number(Gc::new(&ctx, num.recip()))
                    }
                    Value::Inexact(flt) => Value::Inexact(flt.recip()),
                    _ => Err(anyhow::anyhow!(
                        "cannot reciprocate something that is not a number"
                    ))?,
                };
                return Ok(LambdaReturn::Return(vec![value.into_ptr(&ctx)]));
            }

            let mut result = match *args[0].borrow() {
                Value::Number(num) => Either::Left((*num).clone()),
                Value::Inexact(flt) => Either::Right(flt),
                _ => Err(anyhow::anyhow!(
                    "cannot divide something that is not a number"
                ))?,
            };

            for arg in args.iter().skip(1) {
                result = if let Value::Number(n) = *arg.borrow() {
                    if n.is_zero() {
                        return Err(anyhow::anyhow!("cannot divide by 0"))?;
                    }

                    match result {
                        Either::Left(l) => Either::Left(&l / &*n),
                        Either::Right(f) => Either::Right(f / n.to_inexact()),
                    }
                } else if let Value::Inexact(f) = *arg.borrow() {
                    if f == 0.0 {
                        return Err(anyhow::anyhow!("cannot divide by 0"))?;
                    }

                    match result {
                        Either::Left(l) => Either::Right(l.to_inexact() / f),
                        Either::Right(l) => Either::Right(l / f),
                    }
                } else {
                    Err(anyhow::anyhow!(
                        "cannot divide something that is not a number"
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

    /// Returns 2 numbers: n_q = floor(n1/n2) and it's remainder (n_r = n1 - n2n_q)
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct FloorSlash;
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct FloorQuotient;
    #[derive(Debug, Collect)]
    /// Also known as `modulo`
    #[collect(require_static)]
    pub struct FloorRemainder;
    /// Returns 2 numbers: n_q = trunc(n1/n2) and it's remainder (n_r = n1 - n2n_q)
    // trunc(x) = { if x < 0: ceil(x); else: floor(x)
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct TruncateSlash;
    /// Also known as `quotient`
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct TruncateQuotient;
    /// Also known as `remainder`
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct TruncateRemainder;

    /// Get the numerator of a rational number
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Numerator;

    impl<'gc> NativeLambda<'gc> for Numerator {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            use num::ToPrimitive;
            let denom = match *args[0].borrow() {
                Value::Number(n) => match &*n {
                    Number::Integer(n) => {
                        Value::Number(Gc::new(&ctx, Number::Integer(n.clone()))).into_ptr(&ctx)
                    }
                    Number::Rational(r) => {
                        Value::Number(Gc::new(&ctx, Number::Integer(r.numer().clone())))
                            .into_ptr(&ctx)
                    }
                },
                Value::Inexact(i) => Value::Inexact(
                    match Number::from_inexact(i)
                        .ok_or(anyhow::anyhow!("numerator expects a rational input"))?
                    {
                        Number::Integer(n) => n.to_f64().ok_or(anyhow::anyhow!(
                            "failed to convert numerator to inexact number"
                        ))?,
                        Number::Rational(r) => r.numer().to_f64().ok_or(anyhow::anyhow!(
                            "failed to convert numerator to inexact number"
                        ))?,
                    },
                )
                .into_ptr(&ctx),
                _ => Err(anyhow::anyhow!("numerator expects a rational input"))?,
            };

            Ok(LambdaReturn::Return(vec![denom]))
        }
    }

    /// Get the denominator of a rational number
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Denominator;

    impl<'gc> NativeLambda<'gc> for Denominator {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            use num::ToPrimitive;
            let denom = match *args[0].borrow() {
                Value::Number(n) => match &*n {
                    Number::Integer(_) => {
                        Value::Number(Gc::new(&ctx, Number::one())).into_ptr(&ctx)
                    }
                    Number::Rational(r) => {
                        Value::Number(Gc::new(&ctx, Number::Integer(r.denom().clone())))
                            .into_ptr(&ctx)
                    }
                },
                Value::Inexact(i) => Value::Inexact(
                    match Number::from_inexact(i)
                        .ok_or(anyhow::anyhow!("denominator expects a rational input"))?
                    {
                        Number::Integer(_) => 1.,
                        Number::Rational(r) => r.denom().to_f64().ok_or(anyhow::anyhow!(
                            "failed to convert denominator to inexact number"
                        ))?,
                    },
                )
                .into_ptr(&ctx),
                _ => Err(anyhow::anyhow!("denominator expects a rational input"))?,
            };

            Ok(LambdaReturn::Return(vec![denom]))
        }
    }

    /// Get the greatest common divisor of a set of numbers
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Gcd;

    impl<'gc> NativeLambda<'gc> for Gcd {
        fn arity(&self) -> Arity {
            Arity::AtLeast(0)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args
                .iter()
                .any(|a| !matches!(*a.borrow(), Value::Number(_) | Value::Inexact(_)))
            {
                Err(anyhow::anyhow!(
                    "gcd does not support non-integer arguments"
                ))?
            }

            if args.len() == 1 {
                // If there is only 1 argument, that is the result
                return Ok(LambdaReturn::Return(vec![args[0]]));
            } else if args.is_empty() {
                // handle 0 arguments
                return Ok(LambdaReturn::Return(vec![
                    Value::Number(Gc::new(&ctx, Number::ZERO)).into_ptr(&ctx),
                ]));
            }

            // handle the first 2 arguments
            let mut res = match (*args[0].borrow(), *args[1].borrow()) {
                (Value::Number(n1), Value::Number(n2)) => Either::Left(n1.gcd(&n2)),
                (Value::Number(n1), Value::Inexact(n2)) if n2.fract() == 0.0 => {
                    let n2 =
                        Number::from(BigInt::from_f64(n2).expect("failed to convert to integer"));
                    Either::Right(n1.gcd(&n2))
                }
                (Value::Inexact(n1), Value::Number(n2)) if n1.fract() == 0.0 => {
                    let n1 =
                        Number::from(BigInt::from_f64(n1).expect("failed to convert to integer"));
                    Either::Right(n1.gcd(&n2))
                }
                (Value::Inexact(n1), Value::Inexact(n2))
                    if n1.fract() == 0.0 && n2.fract() == 0.0 =>
                {
                    let n1 =
                        Number::from(BigInt::from_f64(n1).expect("failed to convert to integer"));
                    let n2 =
                        Number::from(BigInt::from_f64(n2).expect("failed to convert to integer"));
                    Either::Right(n1.gcd(&n2))
                }
                _ => Err(anyhow::anyhow!(
                    "gcd does not support non-integer inexact numbers"
                ))?,
            };

            for arg in args.iter().skip(2) {
                res = match *arg.borrow() {
                    Value::Number(n) => match res {
                        Either::Left(n1) => Either::Left(n1.gcd(&n)),
                        Either::Right(n1) => Either::Right(n1.gcd(&n)),
                    },
                    Value::Inexact(n) if n.fract() == 0.0 => {
                        let n = Number::from(
                            BigInt::from_f64(n).expect("failed to convert to integer"),
                        );
                        match res {
                            Either::Left(n1) => Either::Right(n1.gcd(&n)),
                            Either::Right(n1) => Either::Right(n1.gcd(&n)),
                        }
                    }
                    _ => Err(anyhow::anyhow!(
                        "gcd does not support non-integer inexact numbers"
                    ))?,
                }
            }

            Ok(LambdaReturn::Return(vec![match res {
                Either::Left(n) => Value::Number(Gc::new(&ctx, n)).into_ptr(&ctx),
                Either::Right(f) => Value::Inexact(f.to_inexact()).into_ptr(&ctx),
            }]))
        }
    }

    /// Get the least common multiple of a set of numbers
    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Lcm;

    impl<'gc> NativeLambda<'gc> for Lcm {
        fn arity(&self) -> Arity {
            Arity::AtLeast(0)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args
                .iter()
                .any(|a| !matches!(*a.borrow(), Value::Number(_) | Value::Inexact(_)))
            {
                Err(anyhow::anyhow!(
                    "lcm does not support non-integer arguments"
                ))?
            }

            if args.len() == 1 {
                // If there is only 1 argument, that is the result
                return Ok(LambdaReturn::Return(vec![args[0]]));
            } else if args.is_empty() {
                // handle 0 arguments
                return Ok(LambdaReturn::Return(vec![
                    Value::Number(Gc::new(&ctx, Number::one())).into_ptr(&ctx),
                ]));
            }

            // handle the first 2 arguments
            let mut res = match (*args[0].borrow(), *args[1].borrow()) {
                (Value::Number(n1), Value::Number(n2)) => Either::Left(n1.lcm(&n2)),
                (Value::Number(n1), Value::Inexact(n2)) if n2.fract() == 0.0 => {
                    let n2 =
                        Number::from(BigInt::from_f64(n2).expect("failed to convert to integer"));
                    Either::Right(n1.lcm(&n2))
                }
                (Value::Inexact(n1), Value::Number(n2)) if n1.fract() == 0.0 => {
                    let n1 =
                        Number::from(BigInt::from_f64(n1).expect("failed to convert to integer"));
                    Either::Right(n1.lcm(&n2))
                }
                (Value::Inexact(n1), Value::Inexact(n2))
                    if n1.fract() == 0.0 && n2.fract() == 0.0 =>
                {
                    let n1 =
                        Number::from(BigInt::from_f64(n1).expect("failed to convert to integer"));
                    let n2 =
                        Number::from(BigInt::from_f64(n2).expect("failed to convert to integer"));
                    Either::Right(n1.lcm(&n2))
                }
                _ => Err(anyhow::anyhow!(
                    "lcm does not support non-integer inexact numbers"
                ))?,
            };

            for arg in args.iter().skip(2) {
                res = match *arg.borrow() {
                    Value::Number(n) => match res {
                        Either::Left(n1) => Either::Left(n1.lcm(&n)),
                        Either::Right(n1) => Either::Right(n1.lcm(&n)),
                    },
                    Value::Inexact(n) if n.fract() == 0.0 => {
                        let n = Number::from(
                            BigInt::from_f64(n).expect("failed to convert to integer"),
                        );
                        match res {
                            Either::Left(n1) => Either::Right(n1.lcm(&n)),
                            Either::Right(n1) => Either::Right(n1.lcm(&n)),
                        }
                    }
                    _ => Err(anyhow::anyhow!(
                        "lcm does not support non-integer inexact numbers"
                    ))?,
                }
            }

            Ok(LambdaReturn::Return(vec![match res {
                Either::Left(n) => Value::Number(Gc::new(&ctx, n)).into_ptr(&ctx),
                Either::Right(f) => Value::Inexact(f.to_inexact()).into_ptr(&ctx),
            }]))
        }
    }
}

mod list {
    use either::Either;
    use gc_arena::{Collect, Gc, RefLock, unsize};
    use num::BigInt;

    use crate::{
        Value, ValuePtr, ValueType,
        runtime::lambda::{
            Arity, Lambda, LambdaResult, LambdaReturn, NativeLambda, NativeLambdaContext,
            NativeLambdaPtr,
        },
        value::{ConsCell, ContinuationPtr, Number},
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Car;
    impl<'gc> NativeLambda<'gc> for Car {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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
    impl<'gc> NativeLambda<'gc> for Caar {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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
    impl<'gc> NativeLambda<'gc> for Cadr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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
    impl<'gc> NativeLambda<'gc> for Cdar {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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
    impl<'gc> NativeLambda<'gc> for Cddr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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
    impl<'gc> NativeLambda<'gc> for Cdr {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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

    #[derive(Debug, Collect, Default)]
    #[collect(no_drop)]
    pub struct Map<'gc> {
        state: Option<MapState<'gc>>,
    }

    #[derive(Collect, Debug, Clone)]
    #[collect(no_drop)]
    enum Procedure<'gc> {
        Lambda(Lambda<'gc>),
        Continuation(ContinuationPtr<'gc>),
    }

    impl<'gc> From<Either<Lambda<'gc>, ContinuationPtr<'gc>>> for Procedure<'gc> {
        fn from(value: Either<Lambda<'gc>, ContinuationPtr<'gc>>) -> Self {
            match value {
                Either::Left(lam) => Procedure::Lambda(lam),
                Either::Right(cont) => Procedure::Continuation(cont),
            }
        }
    }

    #[derive(Collect, Debug, Clone)]
    #[collect(no_drop)]
    struct MapState<'gc> {
        index: usize,
        max: usize,
        lists: Vec<Vec<ValuePtr<'gc>>>,
        proc: Procedure<'gc>,
        results: Vec<ValuePtr<'gc>>,
    }

    impl<'gc> NativeLambda<'gc> for Map<'gc> {
        fn arity(&self) -> Arity {
            Arity::AtLeast(2)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            if ctx.stack.is_empty() {
                // We are in a fresh call, reset state and start work
                let proc = match *args[0].borrow() {
                    Value::Lambda(l) => Either::Left(l),
                    Value::Continuation(c) => Either::Right(c),
                    _ => {
                        return Err(anyhow::anyhow!(
                            "map expects a procedure as its first argument"
                        ))?;
                    }
                };

                let lists = args
                    .iter()
                    .skip(1)
                    .map(|v| match *v.borrow() {
                        Value::Cons(c) if c.is_list(*v, ctx.thread_ctx.null_value) => Ok((*v, c)),
                        Value::Cons(_) => Err(anyhow::anyhow!(
                            "map expects a proper list for the rest of its arguments"
                        )),
                        _ => Err(anyhow::anyhow!(
                            "map expects lists for the rest of its arguments"
                        )),
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if let Either::Left(lam) = &proc {
                    if !lam.arity().is_satisfied(lists.len()) {
                        return Err(anyhow::anyhow!(
                            "map expects a procedure that can accept {} arguments as its first argument",
                            lists.len()
                        ))?;
                    }
                }

                let lists: Vec<_> = lists
                    .into_iter()
                    .map(|(v, l)| {
                        l.list_values(v, ctx.thread_ctx.null_value)
                            .into_iter()
                            .collect::<Vec<_>>()
                    })
                    .collect();

                let max = lists.iter().map(|l| l.len()).min().unwrap_or_default();

                if max == 0 {
                    // If the longest list is the empty list, then we know the result is an empty list!
                    return Ok(LambdaReturn::Return(vec![ctx.thread_ctx.null_value]));
                }

                // Get the first value of each list to call the proc!
                let first_values: Vec<_> = lists.iter().map(|l| l[0]).collect();

                let state = MapState {
                    // We evaluate 0 *right now*
                    index: 1,
                    max,
                    lists,
                    proc: proc.into(),
                    results: vec![],
                };

                self.state = Some(state);

                match proc {
                    Either::Left(lambda) => Ok(LambdaReturn::Call {
                        lambda,
                        args: first_values,
                        dynamic_wind: None,
                    }),
                    Either::Right(cont) => Ok(LambdaReturn::Continue {
                        cont,
                        args: first_values,
                    }),
                }
            } else {
                // Resume where we left off
                let Some(MapState {
                    index,
                    max,
                    lists,
                    proc,
                    mut results,
                }) = self.state.take()
                else {
                    // if the stack is *not* empty, we should have some state
                    unreachable!()
                };

                let result = ctx.stack.last().copied().unwrap();
                results.push(result);

                if index == max {
                    Ok(LambdaReturn::Return(vec![ConsCell::from_iter(
                        &ctx,
                        ctx.thread_ctx.null_value,
                        results,
                    )]))
                } else {
                    // Still more results to evaluate
                    let values: Vec<_> = lists.iter().map(|l| l[index]).collect();
                    self.state = Some(MapState {
                        index: index + 1,
                        max,
                        lists,
                        proc: proc.clone(),
                        results,
                    });

                    match proc {
                        Procedure::Lambda(lambda) => Ok(LambdaReturn::Call {
                            lambda,
                            args: values,
                            dynamic_wind: None,
                        }),
                        Procedure::Continuation(cont) => {
                            Ok(LambdaReturn::Continue { cont, args: values })
                        }
                    }
                }
            }
        }

        fn continuation(&self, mc: &gc_arena::Mutation<'gc>) -> Option<NativeLambdaPtr<'gc>> {
            Some(unsize! [
                Gc::new(mc, RefLock::new(Self {
                    state: self.state.clone()
                })) => RefLock<dyn NativeLambda<'gc> + 'gc>
            ])
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct ListSetBang;

    impl<'gc> NativeLambda<'gc> for ListSetBang {
        fn arity(&self) -> Arity {
            Arity::Exact(3)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            let mut ptr = args[0];
            let Value::Cons(mut c) = *args[0].borrow() else {
                return Err(anyhow::anyhow!(
                    "list-set! expects a list as its first argument"
                ))?;
            };
            let Value::Number(n) = *args[1].borrow() else {
                return Err(anyhow::anyhow!(
                    "list-set! expects an exact integer as its second argument"
                ))?;
            };
            let val = args[2];

            let Number::Integer(mut i) = (*n).clone() else {
                return Err(anyhow::anyhow!(
                    "list-set! expects an exact integer as its second argument"
                ))?;
            };
            let orig_i = i.clone();

            // Find the correct cons cell
            while i > BigInt::ZERO {
                let Some(Value::Cons(cdr)) = c.cdr.map(|c| *c.borrow()) else {
                    return Err(anyhow::anyhow!("list-set!: index {orig_i} out of range"))?;
                };
                i -= 1;
                ptr = c.cdr.unwrap();
                c = cdr;
            }

            // mutate and set
            c.car = Some(val);
            *ptr.borrow_mut(&ctx) = Value::Cons(c);

            Ok(LambdaReturn::Return(vec![Value::Void.into_ptr(&ctx)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct ListCopy;

    impl<'gc> NativeLambda<'gc> for ListCopy {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            fn copy_list<'gc>(
                mc: &gc_arena::Mutation<'gc>,
                cell_ptr: ValuePtr<'gc>,
                encountered: &mut Vec<ValuePtr<'gc>>,
                null_ptr: ValuePtr<'gc>,
            ) -> anyhow::Result<ConsCell<'gc>> {
                if encountered.iter().any(|oc| Gc::ptr_eq(cell_ptr, *oc)) {
                    return Err(anyhow::anyhow!(
                        "list-copy expects a non-circular list as its argument"
                    ));
                }

                encountered.push(cell_ptr);

                let Value::Cons(mut c) = *cell_ptr.borrow() else {
                    return Err(anyhow::anyhow!("list-copy expects a list as its argument"))?;
                };

                let copied_cdr = if let Some(c) = c.cdr {
                    if Gc::ptr_eq(c, null_ptr) {
                        Some(c)
                    } else {
                        Some(if c.borrow().value_type() == ValueType::Cons {
                            Value::Cons(copy_list(mc, c, encountered, null_ptr)?).into_ptr(mc)
                        } else {
                            c
                        })
                    }
                } else {
                    None
                };

                c.cdr = copied_cdr;
                Ok(c)
            }

            let mut encountered = vec![];
            Ok(LambdaReturn::Return(vec![
                Value::Cons(copy_list(
                    &ctx,
                    args[0],
                    &mut encountered,
                    ctx.thread_ctx.null_value,
                )?)
                .into_ptr(&ctx),
            ]))
        }
    }
}

mod vector {
    //! Scheme Vector stuff
    use gc_arena::Collect;

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::Number,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct VectorRef;

    impl<'gc> NativeLambda<'gc> for VectorRef {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &mut self,
            _ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            use num::ToPrimitive;
            let Value::Vector(v) = *args[0].borrow() else {
                return Err(anyhow::anyhow!(
                    "vector-ref expects a vector as its first argument"
                ))?;
            };
            let Value::Number(n) = *args[1].borrow() else {
                return Err(anyhow::anyhow!(
                    "vector-ref expects an exact integer as its second argument"
                ))?;
            };
            let Number::Integer(i) = &*n else {
                return Err(anyhow::anyhow!(
                    "vector-ref expects an exact integer as its second argument"
                ))?;
            };
            let Some(i) = i.to_usize() else {
                return Err(anyhow::anyhow!("vector-ref: index {i} too large"))?;
            };

            Ok(LambdaReturn::Return(vec![v.vec.get(i).copied().ok_or(
                anyhow::anyhow!("vector-ref: index {i} out of range"),
            )?]))
        }
    }
}

mod predicates {
    //! Scheme typechecking stuff
    use gc_arena::{Collect, Gc};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::Number,
    };

    macro_rules! bool_ctx {
        ($ctx:expr, $bool:expr) => {
            if $bool {
                $ctx.thread_ctx.true_value
            } else {
                $ctx.thread_ctx.false_value
            }
        };
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsPair;

    impl<'gc> NativeLambda<'gc> for IsPair {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_pair = match *args[0].borrow() {
                Value::Cons(_) if Gc::ptr_eq(args[0], ctx.thread_ctx.null_value) => false,
                Value::Cons(_) => true,
                _ => false,
            };

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_pair)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsNull;

    impl<'gc> NativeLambda<'gc> for IsNull {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_null = matches!(*args[0].borrow(), Value::Cons(_) if Gc::ptr_eq(args[0], ctx.thread_ctx.null_value));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_null)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsList;

    impl<'gc> NativeLambda<'gc> for IsList {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_list = matches!(*args[0].borrow(), Value::Cons(c) if c.is_list(args[0], ctx.thread_ctx.null_value));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_list)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsExact;

    impl<'gc> NativeLambda<'gc> for IsExact {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO add Complex
            let is_exact = matches!(*args[0].borrow(), Value::Number(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_exact)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsInexact;

    impl<'gc> NativeLambda<'gc> for IsInexact {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO add InexactComplex
            let is_inexact = matches!(*args[0].borrow(), Value::Inexact(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_inexact)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsSymbol;

    impl<'gc> NativeLambda<'gc> for IsSymbol {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_symbol = matches!(*args[0].borrow(), Value::Symbol(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_symbol)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsString;

    impl<'gc> NativeLambda<'gc> for IsString {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_string = matches!(*args[0].borrow(), Value::String(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_string)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsProcedure;

    impl<'gc> NativeLambda<'gc> for IsProcedure {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO parameters (which are considered procedures with arity 0)
            let is_proc = matches!(*args[0].borrow(), Value::Lambda(_) | Value::Continuation(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_proc)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsEven;

    impl<'gc> NativeLambda<'gc> for IsEven {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let two = Number::from_integer(2).unwrap();
            let ret = match *args[0].borrow() {
                Value::Number(n) => &*n % &two == Number::ZERO,
                Value::Inexact(f) if f.fract() == 0. => f % 2. == 0.,
                _ => {
                    return Err(anyhow::anyhow!(
                        "even? expects an integer as its first argument"
                    ))?;
                }
            };

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, ret)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsOdd;

    impl<'gc> NativeLambda<'gc> for IsOdd {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let two = Number::from_integer(2).unwrap();
            let ret = match *args[0].borrow() {
                Value::Number(n) => &*n % &two == Number::one(),
                Value::Inexact(f) if f.fract() == 0. => f % 2. == 1.,
                _ => {
                    return Err(anyhow::anyhow!(
                        "odd? expects an integer as its first argument"
                    ))?;
                }
            };

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, ret)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsInteger;

    impl<'gc> NativeLambda<'gc> for IsInteger {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_integer = match *args[0].borrow() {
                Value::Number(n) => matches!(*n, Number::Integer(_)),
                Value::Inexact(i) => i.fract() == 0.0,
                _ => false,
            };

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_integer)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsExactInteger;

    impl<'gc> NativeLambda<'gc> for IsExactInteger {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_exact_integer = match *args[0].borrow() {
                Value::Number(n) => matches!(*n, Number::Integer(_)),
                _ => false,
            };

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_exact_integer)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsVector;

    impl<'gc> NativeLambda<'gc> for IsVector {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let is_vector = matches!(*args[0].borrow(), Value::Vector(_));

            Ok(LambdaReturn::Return(vec![bool_ctx!(ctx, is_vector)]))
        }
    }
}

mod structure {
    use gc_arena::{Collect, Gc};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaReturn, NativeLambda},
    };

    // Stuff like cons and values (and dealing with values)

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Values;

    impl<'gc> NativeLambda<'gc> for Values {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            if args.len() > 1 {
                Ok(LambdaReturn::Return(vec![
                    Value::Values(Gc::new(&ctx, args.to_vec())).into_ptr(&ctx),
                ]))
            } else {
                Ok(LambdaReturn::Return(Vec::from_iter(args.first().copied())))
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct CallWithValues;

    impl<'gc> NativeLambda<'gc> for CallWithValues {
        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            if ctx.stack.is_empty() {
                // We haven't produced anything, work on that
                let Value::Lambda(producer) = *args[0].borrow() else {
                    return Err(anyhow::anyhow!(
                        "call-with-values expects a lambda as its first argument"
                    ))?;
                };

                Ok(LambdaReturn::Call {
                    lambda: producer,
                    args: vec![],
                    dynamic_wind: None,
                })
            } else {
                let Value::Lambda(consumer) = *args[1].borrow() else {
                    return Err(anyhow::anyhow!(
                        "call-with-values expects a lambda as its second argument"
                    ))?;
                };

                // We've produced something, so get args to tail-call the consumer
                let args = if let Some(produced) = ctx.stack.last().copied() {
                    match *produced.borrow() {
                        Value::Values(v) => (*v).clone(),
                        _ => vec![produced],
                    }
                } else {
                    unreachable!("all lambdas must produce a value, even if it is #<void>")
                };

                Ok(LambdaReturn::TailCall {
                    lambda: consumer,
                    args,
                    dynamic_wind: None,
                })
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cons;

    impl<'gc> NativeLambda<'gc> for Cons {
        fn arity(&self) -> crate::runtime::lambda::Arity {
            Arity::Exact(2)
        }

        fn run(
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
