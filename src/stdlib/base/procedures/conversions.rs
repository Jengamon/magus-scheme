use either::Either;
use gc_arena::{Collect, Gc, RefLock};
use num::{bigint::Sign, BigInt, BigRational, BigUint, One};

use crate::{
    runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    value::{self, ConsCell, Number},
    Value,
};

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Exact;

impl<'gc> NativeLambda<'gc> for Exact {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let ptr = match *args[0].borrow() {
            Value::Number(_) => args[0],
            Value::Inexact(i) => Number::from_inexact(i)
                .map(|n| Value::Number(Gc::new(&ctx, n)).into_ptr(&ctx))
                .ok_or(anyhow::anyhow!(
                    "non-real numbers (infinities / NaN) have no exact representation"
                ))?,
            _ => Err(anyhow::anyhow!("argument is not a number"))?,
        };

        Ok(LambdaReturn::Return(vec![ptr]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Inexact;

impl<'gc> NativeLambda<'gc> for Inexact {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let ptr = match *args[0].borrow() {
            Value::Inexact(_) => args[0],
            Value::Number(i) => Value::Inexact(i.to_inexact()).into_ptr(&ctx),
            _ => Err(anyhow::anyhow!("argument is not a number"))?,
        };

        Ok(LambdaReturn::Return(vec![ptr]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct NumberToString;

impl<'gc> NativeLambda<'gc> for NumberToString {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 2 }
    }
    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        // TODO Support complex numbers
        let num = match *args[0].borrow() {
            Value::Number(n) => Either::Left(n),
            Value::Inexact(i) => Either::Right(i),
            _ => {
                return Err(anyhow::anyhow!(
                    "number->string expects a number as its first argument"
                ))?;
            }
        };

        let radix = if args.len() == 2 {
            let Value::Number(radix) = *args[1].borrow() else {
                return Err(anyhow::anyhow!(
                    "number->string expects an exact number as its second argument"
                ))?;
            };
            Some(radix)
        } else {
            None
        };

        let radix = radix
            .map(|r| match &*r {
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![2]) => Ok(2),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![8]) => Ok(8),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![10]) => Ok(10),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![16]) => Ok(16),
                _ => Err(anyhow::anyhow!(
                    "number->string expects its second argument to be exactly {{2, 8, 10, 16}}"
                )),
            })
            .unwrap_or(Ok(10))?;

        let output_string = match num {
            Either::Right(n) if radix == 10 => {
                // decimal-point repr
                match n {
                    f64::INFINITY => "+inf.0".to_string(),
                    f64::NEG_INFINITY => "-inf.0".to_string(),
                    n if n.is_nan() => {
                        if n.is_sign_negative() {
                            "-nan.0".to_string()
                        } else {
                            "+nan.0".to_string()
                        }
                    }
                    n => format!("{n}"),
                }
            }
            Either::Right(_) => Err(anyhow::anyhow!(
                "number->string: unsupported inexact radix {radix}"
            ))?,
            Either::Left(i) => match &*i {
                Number::Integer(i) => i.to_str_radix(radix),
                Number::Rational(r) => {
                    let mut string = r.numer().to_str_radix(radix);
                    string.push('/');
                    string.push_str(&r.denom().to_str_radix(radix));

                    string
                }
            },
        };

        Ok(LambdaReturn::Return(vec![Value::String(
            Gc::new(&ctx, RefLock::new(output_string)).into(),
        )
        .into_ptr(&ctx)]))
    }
}

fn exact_decimal_stn(
    // is our overall output negative
    is_neg: bool,
    base: BigUint,
    // if non-empty, this is in base-10
    post_digits: Vec<u8>,
    exponent_sign: Option<Sign>,
    // if non-empty, base-10 MSB order
    exponent_digits: Vec<u8>,
) -> BigRational {
    // an exact decimal is a ratio!
    use num::FromPrimitive;

    // we need a ten around for lots of stuff
    let ten = BigInt::from_biguint(Sign::Plus, BigUint::from_u64(10).unwrap());

    let mut num = BigRational::new(BigInt::from_biguint(Sign::Plus, base), BigInt::one());

    for (i, digit) in post_digits.into_iter().enumerate() {
        let ratio = BigRational::new(
            BigInt::new(Sign::Plus, vec![digit as u32]),
            num::pow::Pow::pow(&ten, &(BigUint::from_usize(i).unwrap() + BigUint::one())),
        );
        num += ratio;
    }

    let mut exponent = BigInt::ZERO;

    for (i, digit) in exponent_digits.into_iter().rev().enumerate() {
        let num = BigInt::new(Sign::Plus, vec![digit as u32]);
        exponent += num * num::pow::Pow::pow(&ten, &BigUint::from_usize(i).unwrap());
    }

    if let Some(Sign::Minus) = exponent_sign {
        exponent = -exponent;
    }

    let (exp_sign, exp_mag) = exponent.into_parts();

    let unsigned_decimal_ratio = match exp_sign {
        Sign::NoSign => {
            // exponent part is 0, so we can just return the ratio
            num
        }
        Sign::Plus => num * num::pow::Pow::pow(&ten, &exp_mag),
        Sign::Minus => num / num::pow::Pow::pow(&ten, &exp_mag),
    };

    if is_neg {
        unsigned_decimal_ratio * -BigInt::one()
    } else {
        unsigned_decimal_ratio
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToNumber;

impl<'gc> NativeLambda<'gc> for StringToNumber {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 2 }
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use logos::Logos;
        use num::ToPrimitive;

        let Value::String(string) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "string->number expects a string as its first argument"
            ))?;
        };

        let radix = if args.len() == 2 {
            let Value::Number(radix) = *args[1].borrow() else {
                return Err(anyhow::anyhow!(
                    "string->number expects an exact number as its second argument"
                ))?;
            };
            Some(radix)
        } else {
            None
        };

        let radix = radix
            .map(|r| match &*r {
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![2]) => Ok(2),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![8]) => Ok(8),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![10]) => Ok(10),
                Number::Integer(i) if i == &BigInt::new(Sign::Plus, vec![16]) => Ok(16),
                _ => Err(anyhow::anyhow!(
                    "string->number expects its second argument to be exactly {{2, 8, 10, 16}}"
                )),
            })
            .unwrap_or(Ok(10))?;

        let input = string.borrow().clone();

        // Manually reimplement lexerish behavior (None means failed to read radix flag)

        fn read_flags(mut input: &str) -> Option<(bool, Option<u32>, &str)> {
            let mut is_exact = true;
            let mut radix = None;

            let mut has_exactness = false;
            while input.starts_with('#') {
                match input.chars().nth(1) {
                    Some('e') if !has_exactness => {
                        has_exactness = true;
                        input = &input[2..];
                    }
                    Some('i') if !has_exactness => {
                        is_exact = false;
                        has_exactness = true;
                        input = &input[2..];
                    }
                    Some('b') if radix.is_none() => {
                        radix = Some(2);
                        input = &input[2..];
                    }
                    Some('o') if radix.is_none() => {
                        radix = Some(8);
                        input = &input[2..];
                    }
                    Some('x') if radix.is_none() => {
                        radix = Some(16);
                        input = &input[2..];
                    }
                    Some('d') if radix.is_none() => {
                        radix = Some(10);
                        input = &input[2..];
                    }
                    _ => {
                        return None;
                    }
                }
            }

            Some((is_exact, radix, input))
        }

        let Some((is_exact, maybe_radix, input)) = read_flags(&input) else {
            return Ok(LambdaReturn::Return(vec![ctx.thread_ctx.false_value]));
        };

        let radix = maybe_radix.unwrap_or(radix);

        // TODO support complex
        #[derive(logos::Logos, Debug)]
        enum StnComponent {
            #[token("/")]
            Slash,
            #[token(".")]
            Dot,
            #[token("+")]
            Plus,
            #[token("-")]
            Minus,

            #[token("0", |_| 0)]
            #[token("1", |_| 1)]
            #[token("2", |_| 2)]
            #[token("3", |_| 3)]
            #[token("4", |_| 4)]
            #[token("5", |_| 5)]
            #[token("6", |_| 6)]
            #[token("7", |_| 7)]
            #[token("8", |_| 8)]
            #[token("9", |_| 9)]
            #[token("a", |_| 10)]
            #[token("A", |_| 10)]
            #[token("b", |_| 11)]
            #[token("B", |_| 11)]
            #[token("c", |_| 12)]
            #[token("C", |_| 12)]
            #[token("d", |_| 13)]
            #[token("D", |_| 13)]
            #[token("e", |_| 14)]
            #[token("E", |_| 14)]
            #[token("f", |_| 15)]
            #[token("F", |_| 15)]
            Digit(u32),

            #[token("+inf.0")]
            PosInf,
            #[token("-inf.0")]
            NegInf,
            #[token("+nan.0")]
            PosNan,
            #[token("-nan.0")]
            NegNan,
        }

        let mut lexer = StnComponent::lexer(input);

        #[derive(Debug, Default)]
        enum ParsingState {
            #[default]
            Init,
            ReadSign {
                is_neg: bool,
            },
            ReadDigit {
                is_neg: bool,
                prefix: BigUint,
            },
            Decimal {
                is_neg: bool,
                value: BigUint,
                post_digits: Vec<u8>,
            },
            DecimalExponent {
                is_neg: bool,
                value: BigUint,
                post_digits: Vec<u8>,
                exponent_sign: Option<Sign>,
                exponent_digits: Vec<u8>,
            },
            Rational {
                is_neg: bool,
                numer: BigUint,
                denom: Option<BigUint>,
            },
        }

        let mut state = ParsingState::default();

        loop {
            state = match state {
                ParsingState::Init => match lexer.next() {
                    Some(Ok(StnComponent::Plus)) => ParsingState::ReadSign { is_neg: false },
                    Some(Ok(StnComponent::Minus)) => ParsingState::ReadSign { is_neg: true },
                    Some(Ok(StnComponent::Digit(0))) => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::ZERO,
                    },
                    Some(Ok(StnComponent::Digit(d))) if radix > d => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![d]),
                    },
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg: false,
                        value: BigUint::ZERO,
                        post_digits: vec![],
                    },
                    Some(Ok(StnComponent::PosInf)) if lexer.next().is_none() => {
                        return Ok(LambdaReturn::Return(vec![
                            Value::Inexact(f64::INFINITY).into_ptr(&ctx)
                        ]));
                    }
                    Some(Ok(StnComponent::NegInf)) if lexer.next().is_none() => {
                        return Ok(LambdaReturn::Return(vec![Value::Inexact(
                            f64::NEG_INFINITY,
                        )
                        .into_ptr(&ctx)]));
                    }
                    Some(Ok(StnComponent::PosNan | StnComponent::NegNan))
                        if lexer.next().is_none() =>
                    {
                        return Ok(LambdaReturn::Return(vec![
                            Value::Inexact(f64::NAN).into_ptr(&ctx)
                        ]));
                    }
                    _ => break,
                },
                ParsingState::ReadSign { is_neg } => match lexer.next() {
                    Some(Ok(StnComponent::Digit(0))) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::ZERO,
                    },
                    Some(Ok(StnComponent::Digit(d))) if radix > d => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![d]),
                    },
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg,
                        value: BigUint::ZERO,
                        post_digits: vec![],
                    },
                    _ => break,
                },
                ParsingState::ReadDigit { is_neg, prefix } => match lexer.next() {
                    Some(Ok(StnComponent::Digit(0))) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]),
                    },
                    Some(Ok(StnComponent::Digit(d))) if radix > d => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![d]),
                    },
                    Some(Ok(StnComponent::Digit(0xe))) if radix == 10 => {
                        ParsingState::DecimalExponent {
                            is_neg,
                            value: prefix,
                            post_digits: vec![],
                            exponent_sign: None,
                            exponent_digits: vec![],
                        }
                    }
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg,
                        value: prefix,
                        post_digits: vec![],
                    },
                    Some(Ok(StnComponent::Slash)) => ParsingState::Rational {
                        is_neg,
                        numer: prefix,
                        denom: None,
                    },
                    None => {
                        let num = Number::Integer(BigInt::from_biguint(
                            if is_neg { Sign::Minus } else { Sign::Plus },
                            prefix,
                        ));
                        return Ok(LambdaReturn::Return(vec![if is_exact {
                            Value::Number(Gc::new(&ctx, num)).into_ptr(&ctx)
                        } else {
                            Value::Inexact(num.to_inexact()).into_ptr(&ctx)
                        }]));
                    }
                    _ => break,
                },
                ParsingState::Decimal {
                    is_neg,
                    value,
                    mut post_digits,
                } => match lexer.next() {
                    Some(Ok(StnComponent::Digit(d))) if d < 10 => {
                        post_digits.push(d as u8);
                        ParsingState::Decimal {
                            is_neg,
                            value,
                            post_digits,
                        }
                    }
                    Some(Ok(StnComponent::Digit(0xe))) => ParsingState::DecimalExponent {
                        is_neg,
                        value,
                        post_digits,
                        exponent_sign: None,
                        exponent_digits: vec![],
                    },
                    None => {
                        if !post_digits.is_empty() {
                            return Ok(LambdaReturn::Return(vec![if is_exact {
                                Value::Number(Gc::new(
                                    &ctx,
                                    Number::from_rational(exact_decimal_stn(
                                        is_neg,
                                        value,
                                        post_digits,
                                        None,
                                        vec![],
                                    )),
                                ))
                                .into_ptr(&ctx)
                            } else {
                                Value::Inexact(
                                value
                                    .to_f64()
                                    .ok_or_else(|| {
                                        anyhow::anyhow!("string->number: number too big")
                                    })?
                                    + post_digits
                                        .into_iter()
                                        .enumerate()
                                        .map(|(idx, n)| {
                                            Ok::<_, anyhow::Error>(n as f64
                                                / 10.0f64.powi(
                                                    TryInto::<i32>::try_into(idx)
                                                    .map_err(|_| {
                                                        anyhow::anyhow!("string->number: number too big")
                                                    })? + 1,
                                                ))
                                        })
                                        .collect::<Result<Vec<_>, _>>()?.into_iter().sum::<f64>()
                            )
                            .into_ptr(&ctx)
                            }]));
                        } else {
                            break;
                        }
                    }
                    _ => break,
                },
                ParsingState::DecimalExponent {
                    is_neg,
                    value,
                    post_digits,
                    exponent_sign,
                    mut exponent_digits,
                } => match lexer.next() {
                    Some(Ok(StnComponent::Plus))
                        if exponent_sign.is_none() && exponent_digits.is_empty() =>
                    {
                        ParsingState::DecimalExponent {
                            is_neg,
                            value,
                            post_digits,
                            exponent_sign: Some(Sign::Plus),
                            exponent_digits,
                        }
                    }
                    Some(Ok(StnComponent::Minus))
                        if exponent_sign.is_none() && exponent_digits.is_empty() =>
                    {
                        ParsingState::DecimalExponent {
                            is_neg,
                            value,
                            post_digits,
                            exponent_sign: Some(Sign::Minus),
                            exponent_digits,
                        }
                    }
                    Some(Ok(StnComponent::Digit(d))) if d < 10 => {
                        exponent_digits.push(d as u8);
                        ParsingState::DecimalExponent {
                            is_neg,
                            value,
                            post_digits,
                            exponent_sign,
                            exponent_digits,
                        }
                    }
                    None => {
                        if !exponent_digits.is_empty() {
                            return Ok(LambdaReturn::Return(vec![if is_exact {
                                Value::Number(Gc::new(
                                    &ctx,
                                    Number::from_rational(exact_decimal_stn(
                                        is_neg,
                                        value,
                                        post_digits,
                                        exponent_sign,
                                        exponent_digits,
                                    )),
                                ))
                                .into_ptr(&ctx)
                            } else {
                                Value::Inexact(
                                    (value
                                        .to_f64()
                                        .ok_or_else(|| {
                                            anyhow::anyhow!("string->number: number too big")
                                        })?
                                        + post_digits
                                            .into_iter()
                                            .enumerate()
                                            .map(|(idx, n)| {
                                                Ok::<_, anyhow::Error>(n as f64
                                                    / 10.0f64.powi(
                                                        TryInto::<i32>::try_into(idx)
                                                        .map_err(|_| {
                                                            anyhow::anyhow!("string->number: number too big")
                                                        })? + 1,
                                                    ))
                                            })
                                            .collect::<Result<Vec<_>, _>>()?.into_iter().sum::<f64>())
                                            * 10.0f64.powi(
                                                if let Some(Sign::Minus) = exponent_sign {
                                                    -1
                                                } else {
                                                    1
                                                } * exponent_digits
                                                    .into_iter()
                                                    .rev()
                                                    .enumerate()
                                                    .map(|(idx, n)| Ok::<_, anyhow::Error>(n as i32 *
                                                        10i32.pow(TryInto::<u32>::try_into(idx)
                                                        .map_err(|_| {
                                                            anyhow::anyhow!("string->number: number too big")
                                                        })?)))
                                                    .collect::<Result<Vec<_>, _>>()?.into_iter().sum::<i32>(),
                                            ),
                                )
                                .into_ptr(&ctx)
                            }]));
                        } else {
                            break;
                        }
                    }
                    _ => break,
                },
                ParsingState::Rational {
                    is_neg,
                    numer,
                    denom,
                } => match lexer.next() {
                    Some(Ok(StnComponent::Digit(0))) => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: denom.map(|denom| denom * BigUint::new(vec![radix])),
                    },
                    Some(Ok(StnComponent::Digit(d))) if radix > d => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![d]))
                        } else {
                            Some(BigUint::new(vec![d]))
                        },
                    },
                    None => {
                        if let Some(denom) = denom {
                            let num = Number::from_rational(BigRational::new(
                                BigInt::from_biguint(
                                    if is_neg { Sign::Minus } else { Sign::Plus },
                                    numer,
                                ),
                                BigInt::from_biguint(Sign::Plus, denom),
                            ));
                            return Ok(LambdaReturn::Return(vec![if is_exact {
                                Value::Number(Gc::new(&ctx, num)).into_ptr(&ctx)
                            } else {
                                Value::Inexact(num.to_inexact()).into_ptr(&ctx)
                            }]));
                        } else {
                            // invalid or absent denominator
                            break;
                        }
                    }
                    _ => break,
                },
            };
        }

        Ok(LambdaReturn::Return(vec![ctx.thread_ctx.false_value]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToSymbol;

impl<'gc> NativeLambda<'gc> for StringToSymbol {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let Value::String(s) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "string->symbol expects a string as its first argument"
            ))?;
        };

        let sym = ctx.interner.get_or_intern(s.borrow().as_str());

        Ok(LambdaReturn::Return(vec![
            Value::Symbol(sym.into()).into_ptr(&ctx)
        ]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct SymbolToString;

impl<'gc> NativeLambda<'gc> for SymbolToString {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let Value::Symbol(s) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "symbol->string expects a symbol as its first argument"
            ))?;
        };

        let str = ctx.interner.resolve(&s.0);

        Ok(LambdaReturn::Return(vec![Value::String(
            value::String::new_frozen(Gc::new(&ctx, RefLock::new(str.to_string()))),
        )
        .into_ptr(&ctx)]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct ListToString;

impl<'gc> NativeLambda<'gc> for ListToString {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let Value::Cons(c) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "list->string expects a proper list as its argument"
            ))?;
        };

        if !c.is_list(args[0], ctx.thread_ctx.null_value) {
            return Err(anyhow::anyhow!(
                "list->string expects a proper list as its argument"
            ))?;
        }

        let values: Vec<_> = c
            .list_values(args[0], ctx.thread_ctx.null_value)
            .into_iter()
            .map(|v| match *v.borrow() {
                Value::Char(c) => Ok(c),
                _ => Err(anyhow::anyhow!(
                    "list->string expects a proper list of chars as its argument"
                )),
            })
            .collect::<Result<_, _>>()?;
        let s = String::from_iter(values);

        Ok(LambdaReturn::Return(vec![Value::String(
            Gc::new(&ctx, RefLock::new(s)).into(),
        )
        .into_ptr(&ctx)]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToList;

impl<'gc> NativeLambda<'gc> for StringToList {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 3 }
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use num::ToPrimitive;
        let Value::String(s) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "string->list expects a string as its first argument"
            ))?;
        };

        let maybe_start = args.get(1);
        let start = if let Some(start) = maybe_start {
            match *start.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(start) = &*n else {
                        unreachable!()
                    };

                    start
                        .to_usize()
                        .ok_or(anyhow::anyhow!("string->list: start is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "string->list expects an integer as its second argument"
                ))?,
            }
        } else {
            0usize
        };

        let maybe_end = args.get(2);
        let end = if let Some(end) = maybe_end {
            match *end.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(end) = &*n else {
                        unreachable!()
                    };

                    end.to_usize()
                        .ok_or(anyhow::anyhow!("string->list: end is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "string->list expects an integer as its third argument"
                ))?,
            }
        } else {
            s.borrow().chars().count()
        };

        if end < start {
            return Err(anyhow::anyhow!("string->list: start must be <= end"))?;
        } else if end > s.borrow().chars().count() {
            return Err(anyhow::anyhow!("string->list: end out of range"))?;
        }

        let chars: Vec<_> = s
            .borrow()
            .chars()
            .skip(start)
            .take(end - start)
            .map(|c| Value::Char(c).into_ptr(&ctx))
            .collect();

        Ok(LambdaReturn::Return(vec![ConsCell::from_iter(
            &ctx,
            ctx.thread_ctx.null_value,
            chars,
        )]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Utf8ToString;

impl<'gc> NativeLambda<'gc> for Utf8ToString {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 3 }
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use num::ToPrimitive;
        let Value::Bytevector(b) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "utf8->string expects a bytevector as its first argument"
            ))?;
        };

        let maybe_start = args.get(1);
        let start = if let Some(start) = maybe_start {
            match *start.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(start) = &*n else {
                        unreachable!()
                    };

                    start
                        .to_usize()
                        .ok_or(anyhow::anyhow!("utf8->string: start is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "utf8->string expects an integer as its second argument"
                ))?,
            }
        } else {
            0usize
        };

        let maybe_end = args.get(2);
        let end = if let Some(end) = maybe_end {
            match *end.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(end) = &*n else {
                        unreachable!()
                    };

                    end.to_usize()
                        .ok_or(anyhow::anyhow!("utf8->string: end is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "utf8->string expects an integer as its third argument"
                ))?,
            }
        } else {
            b.vec.len()
        };

        if end < start {
            return Err(anyhow::anyhow!("utf8->string: start must be <= end"))?;
        } else if end > b.vec.len() {
            return Err(anyhow::anyhow!("utf8->string: end out of range"))?;
        }

        let b = b
            .vec
            .iter()
            .skip(start)
            .take(end - start)
            .copied()
            .collect::<Vec<_>>();
        let s = String::from_utf8(b)
            .map_err(|_| anyhow::anyhow!("utf8->string: bytes were not valid UTF8"))?;

        Ok(LambdaReturn::Return(vec![Value::String(
            Gc::new(&ctx, RefLock::new(s)).into(),
        )
        .into_ptr(&ctx)]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToUtf8;

impl<'gc> NativeLambda<'gc> for StringToUtf8 {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 3 }
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use num::ToPrimitive;
        let Value::String(s) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "string->utf8 expects a string as its first argument"
            ))?;
        };

        let maybe_start = args.get(1);
        let start = if let Some(start) = maybe_start {
            match *start.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(start) = &*n else {
                        unreachable!()
                    };

                    start
                        .to_usize()
                        .ok_or(anyhow::anyhow!("string->utf8: start is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "string->utf8 expects an integer as its second argument"
                ))?,
            }
        } else {
            0usize
        };

        let maybe_end = args.get(2);
        let end = if let Some(end) = maybe_end {
            match *end.borrow() {
                Value::Number(n) if matches!(*n, Number::Integer(_)) => {
                    let Number::Integer(end) = &*n else {
                        unreachable!()
                    };

                    end.to_usize()
                        .ok_or(anyhow::anyhow!("string->utf8: end is too big"))?
                }
                _ => Err(anyhow::anyhow!(
                    "string->utf8 expects an integer as its third argument"
                ))?,
            }
        } else {
            s.borrow().chars().count()
        };

        if end < start {
            return Err(anyhow::anyhow!("string->utf8: start must be <= end"))?;
        } else if end > s.borrow().chars().count() {
            return Err(anyhow::anyhow!("string->utf8: end out of range"))?;
        }

        let s = s
            .borrow()
            .chars()
            .skip(start)
            .take(end - start)
            .collect::<String>();
        let bytes = im_rc::Vector::from_iter(s.as_bytes().iter().copied());

        Ok(LambdaReturn::Return(vec![Value::Bytevector(
            Gc::new(&ctx, gc_arena::Static(bytes)).into(),
        )
        .into_ptr(&ctx)]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct CharToInteger;

impl<'gc> NativeLambda<'gc> for CharToInteger {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let Value::Char(c) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "char->integer expects a char as its argument"
            ))?;
        };

        Ok(LambdaReturn::Return(vec![
            // u32 -> i64 is always valid, so we can unwrap
            Value::Number(Gc::new(&ctx, Number::from_integer(c as u32).unwrap())).into_ptr(&ctx),
        ]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct IntegerToChar;

impl<'gc> NativeLambda<'gc> for IntegerToChar {
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use num::ToPrimitive;

        let Value::Number(n) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "integer->char expects an exact integer as its argument"
            ))?;
        };

        let Number::Integer(i) = &*n else {
            return Err(anyhow::anyhow!(
                "integer->char expects an exact integer as its argument"
            ))?;
        };

        let Some(c) = i.to_u32().and_then(char::from_u32) else {
            return Err(anyhow::anyhow!(
                "integer->char expects a valid Unicode codepoint"
            ))?;
        };

        Ok(LambdaReturn::Return(vec![Value::Char(c).into_ptr(&ctx)]))
    }
}
