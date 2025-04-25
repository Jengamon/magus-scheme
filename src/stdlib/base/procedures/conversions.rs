use either::Either;
use gc_arena::{Collect, Gc, RefLock};
use num::{BigInt, BigRational, BigUint, bigint::Sign};

use crate::{
    Value,
    runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
    value::{self, ConsCell, Number},
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
        Arity::AtLeast(1)
    }
    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        if args.len() > 2 {
            return Err(anyhow::anyhow!(
                "number->string expects either 1 or 2 arguments"
            ))?;
        }

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

        fn to_string_radix(mut num: u32, radix: u32) -> String {
            let mut digits = Vec::new();
            while num > 0 {
                let digit = num % radix;
                num /= radix;
                digits.push(char::from_digit(digit, radix).unwrap());
            }

            if digits.is_empty() {
                "0".to_string()
            } else {
                digits.into_iter().rev().collect()
            }
        }

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
                Number::Integer(i) => {
                    let (sign, components) = i.to_u32_digits();
                    let mut string = String::new();
                    for i in components.into_iter().rev() {
                        string.push_str(&to_string_radix(i, radix));
                    }

                    if sign == Sign::Minus {
                        format!("-{string}")
                    } else {
                        string
                    }
                }
                Number::Rational(r) => {
                    let (sign_num, component_num) = r.numer().to_u32_digits();
                    let (sign_den, component_den) = r.denom().to_u32_digits();

                    let mut string = String::new();

                    for i in component_num.into_iter().rev() {
                        string.push_str(&to_string_radix(i, radix));
                    }
                    string.push('/');
                    for i in component_den.into_iter().rev() {
                        string.push_str(&to_string_radix(i, radix));
                    }

                    if (sign_num == Sign::Minus) ^ (sign_den == Sign::Minus) {
                        format!("-{string}")
                    } else {
                        string
                    }
                }
            },
        };

        Ok(LambdaReturn::Return(vec![
            Value::String(Gc::new(&ctx, RefLock::new(output_string)).into()).into_ptr(&ctx),
        ]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToNumber;

impl<'gc> NativeLambda<'gc> for StringToNumber {
    fn arity(&self) -> Arity {
        Arity::AtLeast(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        use logos::Logos;
        use num::ToPrimitive;
        if args.len() > 2 {
            return Err(anyhow::anyhow!(
                "string->number expects either 1 or 2 arguments"
            ))?;
        }

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

            #[token("0")]
            Zero,
            #[token("1")]
            One,

            #[token("2")]
            Two,
            #[token("3")]
            Three,
            #[token("4")]
            Four,
            #[token("5")]
            Five,
            #[token("6")]
            Six,
            #[token("7")]
            Seven,

            #[token("8")]
            Eight,
            #[token("9")]
            Nine,

            #[token("a")]
            #[token("A")]
            Ten,
            #[token("b")]
            #[token("B")]
            Eleven,
            #[token("c")]
            #[token("C")]
            Twelve,
            #[token("d")]
            #[token("D")]
            Thirteen,
            #[token("e")]
            #[token("E")]
            Fourteen,
            #[token("f")]
            #[token("F")]
            Fifteen,

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
                value: f64,
                post_digits: Vec<u8>,
                exponent_sign: Option<Sign>,
                exponent: BigUint,
                is_valid: bool,
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
                    Some(Ok(StnComponent::Zero)) => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::ZERO,
                    },
                    Some(Ok(StnComponent::One)) => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![1]),
                    },
                    Some(Ok(StnComponent::Two)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![2]),
                    },
                    Some(Ok(StnComponent::Three)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![3]),
                    },
                    Some(Ok(StnComponent::Four)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![4]),
                    },
                    Some(Ok(StnComponent::Five)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![5]),
                    },
                    Some(Ok(StnComponent::Six)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![6]),
                    },
                    Some(Ok(StnComponent::Seven)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![7]),
                    },
                    Some(Ok(StnComponent::Eight)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![8]),
                    },
                    Some(Ok(StnComponent::Nine)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![9]),
                    },
                    Some(Ok(StnComponent::Ten)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![10]),
                    },
                    Some(Ok(StnComponent::Eleven)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![11]),
                    },
                    Some(Ok(StnComponent::Twelve)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![12]),
                    },
                    Some(Ok(StnComponent::Thirteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![13]),
                    },
                    Some(Ok(StnComponent::Fourteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![14]),
                    },
                    Some(Ok(StnComponent::Fifteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg: false,
                        prefix: BigUint::new(vec![15]),
                    },
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg: false,
                        value: 0.0,
                        post_digits: vec![],
                        exponent_sign: None,
                        exponent: BigUint::ZERO,
                        is_valid: false,
                    },
                    Some(Ok(StnComponent::PosInf)) if lexer.next().is_none() => {
                        return Ok(LambdaReturn::Return(vec![
                            Value::Inexact(f64::INFINITY).into_ptr(&ctx),
                        ]));
                    }
                    Some(Ok(StnComponent::NegInf)) if lexer.next().is_none() => {
                        return Ok(LambdaReturn::Return(vec![
                            Value::Inexact(f64::NEG_INFINITY).into_ptr(&ctx),
                        ]));
                    }
                    Some(Ok(StnComponent::PosNan | StnComponent::NegNan))
                        if lexer.next().is_none() =>
                    {
                        return Ok(LambdaReturn::Return(vec![
                            Value::Inexact(f64::NAN).into_ptr(&ctx),
                        ]));
                    }
                    _ => break,
                },
                ParsingState::ReadSign { is_neg } => match lexer.next() {
                    Some(Ok(StnComponent::Zero)) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::ZERO,
                    },
                    Some(Ok(StnComponent::One)) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![1]),
                    },
                    Some(Ok(StnComponent::Two)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![2]),
                    },
                    Some(Ok(StnComponent::Three)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![3]),
                    },
                    Some(Ok(StnComponent::Four)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![4]),
                    },
                    Some(Ok(StnComponent::Five)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![5]),
                    },
                    Some(Ok(StnComponent::Six)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![6]),
                    },
                    Some(Ok(StnComponent::Seven)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![7]),
                    },
                    Some(Ok(StnComponent::Eight)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![8]),
                    },
                    Some(Ok(StnComponent::Nine)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![9]),
                    },
                    Some(Ok(StnComponent::Ten)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![10]),
                    },
                    Some(Ok(StnComponent::Eleven)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![11]),
                    },
                    Some(Ok(StnComponent::Twelve)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![12]),
                    },
                    Some(Ok(StnComponent::Thirteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![13]),
                    },
                    Some(Ok(StnComponent::Fourteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![14]),
                    },
                    Some(Ok(StnComponent::Fifteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: BigUint::new(vec![15]),
                    },
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg,
                        value: 0.0,
                        post_digits: vec![],
                        exponent_sign: None,
                        exponent: BigUint::ZERO,
                        is_valid: false,
                    },
                    _ => break,
                },
                ParsingState::ReadDigit { is_neg, prefix } => match lexer.next() {
                    Some(Ok(StnComponent::Zero)) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]),
                    },
                    Some(Ok(StnComponent::One)) => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![1]),
                    },
                    Some(Ok(StnComponent::Two)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![2]),
                    },
                    Some(Ok(StnComponent::Three)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![3]),
                    },
                    Some(Ok(StnComponent::Four)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![4]),
                    },
                    Some(Ok(StnComponent::Five)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![5]),
                    },
                    Some(Ok(StnComponent::Six)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![6]),
                    },
                    Some(Ok(StnComponent::Seven)) if radix >= 8 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![7]),
                    },
                    Some(Ok(StnComponent::Eight)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![8]),
                    },
                    Some(Ok(StnComponent::Nine)) if radix >= 10 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![9]),
                    },
                    Some(Ok(StnComponent::Ten)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![10]),
                    },
                    Some(Ok(StnComponent::Eleven)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![11]),
                    },
                    Some(Ok(StnComponent::Twelve)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![12]),
                    },
                    Some(Ok(StnComponent::Thirteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![13]),
                    },
                    Some(Ok(StnComponent::Fourteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![14]),
                    },
                    Some(Ok(StnComponent::Fifteen)) if radix == 16 => ParsingState::ReadDigit {
                        is_neg,
                        prefix: prefix * BigUint::new(vec![radix]) + BigUint::new(vec![15]),
                    },
                    Some(Ok(StnComponent::Dot)) if radix == 10 => ParsingState::Decimal {
                        is_neg,
                        value: prefix
                            .to_f64()
                            .ok_or(anyhow::anyhow!("string->number: number too big"))?,
                        post_digits: vec![],
                        exponent_sign: None,
                        exponent: BigUint::ZERO,
                        is_valid: false,
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
                    exponent_sign,
                    exponent,
                    is_valid,
                } => match lexer.next() {
                    Some(Ok(StnComponent::Zero)) => {
                        post_digits.push(0);
                        ParsingState::Decimal {
                            is_neg,
                            value,
                            post_digits,
                            exponent_sign,
                            exponent,
                            is_valid,
                        }
                    }
                    _ => {
                        if is_valid {
                            return Ok(LambdaReturn::Return(vec![
                                Value::Inexact(
                                    value
                                        * (10.0f64.powf(
                                            BigInt::from_biguint(
                                                exponent_sign.unwrap_or(Sign::Plus),
                                                exponent,
                                            )
                                            .to_f64()
                                            .ok_or(
                                                anyhow::anyhow!("string->number: number too big"),
                                            )?,
                                        )),
                                )
                                .into_ptr(&ctx),
                            ]));
                        } else {
                            break;
                        }
                    }
                },
                ParsingState::Rational {
                    is_neg,
                    numer,
                    denom,
                } => match lexer.next() {
                    Some(Ok(StnComponent::Zero)) => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: denom.map(|denom| denom * BigUint::new(vec![radix])),
                    },
                    Some(Ok(StnComponent::One)) => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![1]))
                        } else {
                            Some(BigUint::new(vec![1]))
                        },
                    },
                    Some(Ok(StnComponent::Two)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![2]))
                        } else {
                            Some(BigUint::new(vec![2]))
                        },
                    },
                    Some(Ok(StnComponent::Three)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![3]))
                        } else {
                            Some(BigUint::new(vec![3]))
                        },
                    },
                    Some(Ok(StnComponent::Four)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![4]))
                        } else {
                            Some(BigUint::new(vec![4]))
                        },
                    },
                    Some(Ok(StnComponent::Five)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![5]))
                        } else {
                            Some(BigUint::new(vec![5]))
                        },
                    },
                    Some(Ok(StnComponent::Six)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![6]))
                        } else {
                            Some(BigUint::new(vec![6]))
                        },
                    },
                    Some(Ok(StnComponent::Seven)) if radix >= 8 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![7]))
                        } else {
                            Some(BigUint::new(vec![7]))
                        },
                    },
                    Some(Ok(StnComponent::Eight)) if radix >= 10 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![8]))
                        } else {
                            Some(BigUint::new(vec![8]))
                        },
                    },
                    Some(Ok(StnComponent::Nine)) if radix >= 10 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![9]))
                        } else {
                            Some(BigUint::new(vec![9]))
                        },
                    },
                    Some(Ok(StnComponent::Ten)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![10]))
                        } else {
                            Some(BigUint::new(vec![10]))
                        },
                    },
                    Some(Ok(StnComponent::Eleven)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![11]))
                        } else {
                            Some(BigUint::new(vec![11]))
                        },
                    },
                    Some(Ok(StnComponent::Twelve)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![12]))
                        } else {
                            Some(BigUint::new(vec![12]))
                        },
                    },
                    Some(Ok(StnComponent::Thirteen)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![13]))
                        } else {
                            Some(BigUint::new(vec![13]))
                        },
                    },
                    Some(Ok(StnComponent::Fourteen)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![14]))
                        } else {
                            Some(BigUint::new(vec![14]))
                        },
                    },
                    Some(Ok(StnComponent::Fifteen)) if radix == 16 => ParsingState::Rational {
                        is_neg,
                        numer,
                        denom: if let Some(denom) = denom {
                            Some(denom * BigUint::new(vec![radix]) + BigUint::new(vec![15]))
                        } else {
                            Some(BigUint::new(vec![15]))
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
            Value::Symbol(sym.into()).into_ptr(&ctx),
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

        Ok(LambdaReturn::Return(vec![
            Value::String(value::String::new_frozen(Gc::new(
                &ctx,
                RefLock::new(str.to_string()),
            )))
            .into_ptr(&ctx),
        ]))
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

        Ok(LambdaReturn::Return(vec![
            Value::String(Gc::new(&ctx, RefLock::new(s)).into()).into_ptr(&ctx),
        ]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct StringToList;

impl<'gc> NativeLambda<'gc> for StringToList {
    fn arity(&self) -> Arity {
        Arity::AtLeast(1)
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
            s.borrow().len()
        };

        if end < start {
            return Err(anyhow::anyhow!("string->list: start must be less than end"))?;
        }

        let s = &s.borrow()[start..end];
        let chars: Vec<_> = s.chars().map(|c| Value::Char(c).into_ptr(&ctx)).collect();

        Ok(LambdaReturn::Return(vec![ConsCell::from_iter(
            &ctx,
            ctx.thread_ctx.null_value,
            chars,
        )]))
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
