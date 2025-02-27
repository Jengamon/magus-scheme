//! Because exact number handling is quite important to Scheme

use core::fmt;
use std::num::{NonZeroU16, NonZeroU64};

use arbitrary::Arbitrary;

#[derive(Debug, PartialEq, Clone, Copy, Arbitrary)]
pub enum SchemeNumber {
    Exact(ExactReal),
    ExactComplex {
        real: ExactReal,
        imaginary: ExactReal,
    },
    ExactPolar {
        modulus: ExactReal,
        argument: ExactReal, // radians
    },
    Inexact(f64),
    InexactComplex {
        real: f64,
        imaginary: f64,
    },
}

impl fmt::Display for SchemeNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemeNumber::Exact(e) => match e {
                deci @ ExactReal::Decimal { .. } => write!(f, "#e{deci}"),
                e => write!(f, "{e}"),
            },
            SchemeNumber::ExactComplex { real, imaginary } => {
                if real.is_decimal() || imaginary.is_decimal() {
                    write!(f, "#e{real}{imaginary:#}i")
                } else {
                    write!(f, "{real}{imaginary:#}i")
                }
            }
            SchemeNumber::ExactPolar { modulus, argument } => {
                write!(f, "{modulus}@{argument}")
            }
            SchemeNumber::Inexact(inex) => write!(f, "#i{inex}"),
            SchemeNumber::InexactComplex { real, imaginary } => {
                write!(f, "#i{real}{imaginary:+#}i")
            }
        }
    }
}

macro_rules! impl_into_sn {
    (into $($ty:ty),+) => {
        $(
            impl From<$ty> for SchemeNumber {
                fn from(value: $ty) -> Self {
                    Self::Exact(value.into())
                }
            }
        )+
    };
}
impl_into_sn!(into u8, u16, u32, u64, i8, i16, i32, i64);
impl From<ExactReal> for SchemeNumber {
    fn from(value: ExactReal) -> Self {
        Self::Exact(value)
    }
}
impl From<f32> for SchemeNumber {
    fn from(value: f32) -> Self {
        Self::Inexact(value.into())
    }
}
impl From<f64> for SchemeNumber {
    fn from(value: f64) -> Self {
        Self::Inexact(value)
    }
}

impl SchemeNumber {
    pub fn integer(is_neg: bool, value: u64) -> Self {
        Self::Exact(ExactReal::Integer { is_neg, value })
    }

    pub fn real_decimal(
        is_neg: bool,
        base: u64,
        leading_zeros: u16,
        post_dot: u64,
        exponent_neg: bool,
        exponent: u64,
    ) -> Self {
        Self::Exact(ExactReal::Decimal {
            base,
            leading_zeros: NonZeroU16::new(leading_zeros),
            post_dot,
            exponent,
            exponent_neg,
            is_neg,
        })
    }

    pub fn is_inexact(self) -> bool {
        matches! { self, Self::Inexact(_) | Self::InexactComplex { .. }}
    }
}

/// Possible values to exactly represent real numbers
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExactReal {
    Integer {
        value: u64,
        is_neg: bool,
    },
    Rational {
        numer: u64,
        denom: u64,
        is_neg: bool,
    },
    Inf {
        is_neg: bool,
    },
    Nan {
        is_neg: bool,
    },
    Decimal {
        base: u64,
        /// The *number* of leading zeroes in the source of the number after the decimal point
        leading_zeros: Option<NonZeroU16>,
        post_dot: u64,
        exponent: u64,
        exponent_neg: bool,
        is_neg: bool,
    },
}

impl ExactReal {
    /// Can this exact number be determined to be 0?
    pub fn is_zero(self) -> bool {
        matches!(
            self,
            ExactReal::Integer { value: 0, .. }
                | ExactReal::Decimal {
                    base: 0,
                    post_dot: 0,
                    ..
                }
        ) || matches!(self, ExactReal::Rational { numer: 0, denom, ..} if denom != 0)
    }
}

impl<'a> Arbitrary<'a> for ExactReal {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(match u.int_in_range(0..=4)? {
            0 => Self::Integer {
                value: u64::arbitrary(u)?,
                is_neg: bool::arbitrary(u)?,
            },
            1 => Self::Rational {
                numer: u64::arbitrary(u)?,
                denom: u64::arbitrary(u)?,
                is_neg: bool::arbitrary(u)?,
            },
            2 => Self::Inf {
                is_neg: bool::arbitrary(u)?,
            },
            3 => Self::Nan {
                is_neg: bool::arbitrary(u)?,
            },
            4 => {
                // cap the number of leading zeros to a "reasonable" amount.
                let leading_zeros = NonZeroU16::new(u16::arbitrary(u)?.min(32));
                let post_dot = if leading_zeros.is_some() {
                    // if leading zeros is some, then the post dot will cannot be just 0
                    // if it was zero, leading zeros would be 1 bigger.
                    NonZeroU64::arbitrary(u)?.get()
                } else {
                    u64::arbitrary(u)?
                };
                Self::Decimal {
                    base: u64::arbitrary(u)?,
                    leading_zeros,
                    post_dot,
                    exponent: u64::arbitrary(u)?,
                    exponent_neg: bool::arbitrary(u)?,
                    is_neg: bool::arbitrary(u)?,
                }
            }
            _ => unreachable!(),
        })
    }
}

// display is useful and is used for external representation
impl fmt::Display for ExactReal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let calc_sign = |is_neg: bool| {
            if is_neg {
                "-"
            } else if f.alternate() {
                "+"
            } else {
                ""
            }
        };

        match self {
            ExactReal::Integer { value, is_neg } => write!(f, "{}{value}", calc_sign(*is_neg)),
            ExactReal::Rational {
                numer,
                denom,
                is_neg,
            } => write!(f, "{}{numer}/{denom}", calc_sign(*is_neg)),
            ExactReal::Inf { is_neg } => write!(f, "{}inf.0", if *is_neg { "-" } else { "+" }),
            ExactReal::Nan { is_neg } => write!(f, "{}nan.0", if *is_neg { "-" } else { "+" }),
            ExactReal::Decimal { is_neg, .. } => write!(
                f,
                "{}{}",
                if !*is_neg && f.alternate() { "+" } else { "" },
                self.display(10).unwrap()
            ),
        }
    }
}

macro_rules! exact_integer_impl {
    ($ty:ty) => {
        impl From<$ty> for ExactReal {
            fn from(value: $ty) -> Self {
                Self::Integer {
                    value: value.into(),
                    is_neg: false,
                }
            }
        }
    };
    (signed $ty:ty) => {
        impl From<$ty> for ExactReal {
            fn from(value: $ty) -> Self {
                Self::Integer {
                    value: value.unsigned_abs().into(),
                    is_neg: value < 0,
                }
            }
        }
    };
}

exact_integer_impl!(u8);
exact_integer_impl!(u16);
exact_integer_impl!(u32);
exact_integer_impl!(u64);
impl TryFrom<usize> for ExactReal {
    type Error = std::num::TryFromIntError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Self::Integer {
            value: TryInto::<u64>::try_into(value)?,
            is_neg: false,
        })
    }
}
exact_integer_impl!(signed i8);
exact_integer_impl!(signed i16);
exact_integer_impl!(signed i32);
exact_integer_impl!(signed i64);
impl TryFrom<isize> for ExactReal {
    type Error = std::num::TryFromIntError;
    fn try_from(value: isize) -> Result<Self, Self::Error> {
        Ok(Self::Integer {
            value: TryInto::<i64>::try_into(value)?.unsigned_abs(),
            is_neg: value < 0,
        })
    }
}

impl ExactReal {
    pub fn is_neg(self) -> bool {
        match self {
            Self::Integer { is_neg, .. } => is_neg,
            Self::Rational { is_neg, .. } => is_neg,
            Self::Inf { is_neg } => is_neg,
            Self::Nan { is_neg } => is_neg,
            Self::Decimal { is_neg, .. } => is_neg,
        }
    }

    // Used internally for inexact calculation
    // TODO Make this a no-op by storing in a u16
    fn calc_leading_zeros(leading_zeros: Option<NonZeroU16>) -> u16 {
        leading_zeros.map(|nz| nz.get()).unwrap_or(0)
    }

    pub fn is_numeric(self) -> bool {
        !matches!(self, Self::Inf { .. } | Self::Nan { .. })
    }

    pub fn is_decimal(self) -> bool {
        matches!(self, Self::Decimal { .. })
    }

    // makes a version of this number that can be stored inexactly
    pub fn inexact(self) -> f64 {
        match self {
            ExactReal::Integer { value, is_neg } => {
                (if is_neg { -1.0 } else { 1.0 }) * value as f64
            }
            ExactReal::Rational {
                numer,
                denom,
                is_neg,
            } => (if is_neg { -1.0 } else { 1.0 }) * (numer as f64 / denom as f64),
            ExactReal::Inf { is_neg } => {
                if is_neg {
                    f64::NEG_INFINITY
                } else {
                    f64::INFINITY
                }
            }
            ExactReal::Nan { is_neg } => (if is_neg { -1.0 } else { 1.0 }) * f64::NAN,
            ExactReal::Decimal {
                base,
                leading_zeros,
                post_dot,
                exponent,
                exponent_neg,
                is_neg,
            } => {
                let post_dot_10_power = (post_dot as f64).log10().max(1.).ceil()
                    + Self::calc_leading_zeros(leading_zeros) as f64;
                (if is_neg { -1.0 } else { 1.0 })
                    * (base as f64
                        + (post_dot as f64
                            / if post_dot_10_power == f64::NEG_INFINITY {
                                // post_dot is 0, so this should act like 10^0 == 1
                                1.0
                            } else {
                                10f64.powf(post_dot_10_power)
                            }))
                    * 10f64.powf((if exponent_neg { -1.0 } else { 1.0 }) * exponent as f64)
            }
        }
    }

    fn to_string_radix(mut num: u64, radix: u32) -> Option<Box<str>> {
        let mut string_rev = Vec::new();
        while num > 0 {
            string_rev.push(char::from_digit((num % (radix as u64)) as u32, radix)?);
            num /= radix as u64;
        }
        if !string_rev.is_empty() {
            Some(string_rev.into_iter().rev().collect())
        } else {
            Some(Box::from("0"))
        }
    }

    pub fn display(self, radix: u32) -> Option<Box<str>> {
        if ![2, 8, 10, 16].contains(&radix) {
            return None;
        }

        let mut output = String::new();
        match self {
            ExactReal::Integer { value, is_neg } => {
                if is_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(value, radix)?);
            }
            ExactReal::Rational {
                numer,
                denom,
                is_neg,
            } => {
                if is_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(numer, radix)?);
                output.push('/');
                output.push_str(&Self::to_string_radix(denom, radix)?);
            }
            ExactReal::Inf { is_neg } => {
                output.push(if is_neg { '-' } else { '+' });
                output.push_str("inf.0");
            }
            ExactReal::Nan { is_neg } => {
                output.push(if is_neg { '-' } else { '+' });
                output.push_str("nan.0");
            }
            ExactReal::Decimal {
                base,
                leading_zeros,
                post_dot,
                exponent,
                exponent_neg,
                is_neg,
            } if radix == 10 => {
                if is_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(base, radix)?);
                if post_dot != 0 {
                    output.push('.');
                    output.push_str(
                        "0".repeat(leading_zeros.map(|nz| nz.get()).unwrap_or_default() as usize)
                            .as_str(),
                    );
                    output.push_str(&Self::to_string_radix(post_dot, radix)?);
                }
                if exponent_neg || exponent != 0 {
                    output.push('e');
                    if exponent_neg {
                        output.push('-');
                    }
                    output.push_str(&Self::to_string_radix(exponent, radix)?);
                }
            }
            ExactReal::Decimal { .. } => return None,
        }
        Some(Box::from(output.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU16;

    use super::ExactReal;
    use assert2::check;

    #[test]
    fn exact_to_inexact() {
        let exact = ExactReal::Decimal {
            base: 3,
            leading_zeros: None,
            post_dot: 5,
            exponent: 2,
            exponent_neg: true,
            is_neg: false,
        };
        check!(exact.inexact() == 0.035);
        let exact = ExactReal::Decimal {
            base: 42,
            leading_zeros: None,
            post_dot: 5020,
            exponent: 2,
            exponent_neg: false,
            is_neg: true,
        };
        check!(exact.inexact() == -4250.20);
        // Found by arbtest
        let exact = ExactReal::Decimal {
            base: 0,
            leading_zeros: None,
            post_dot: 0,
            exponent: 0,
            exponent_neg: false,
            is_neg: false,
        };
        check!(exact.inexact() == 0.0);
        // Found by trying shit
        let leading_zeros = ExactReal::Decimal {
            base: 3,
            leading_zeros: NonZeroU16::new(1),
            post_dot: 1,
            exponent: 0,
            exponent_neg: false,
            is_neg: false,
        };
        check!(leading_zeros.inexact() == 3.01);
    }
}
