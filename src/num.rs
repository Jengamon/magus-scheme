//! Because exact number handling is quite important to Scheme

use arbitrary::Arbitrary;

// Does *not* implement Display b/c these structures are parse/lex structures,
// not runtime ones.
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

impl SchemeNumber {
    pub fn integer(is_neg: bool, value: u64) -> Self {
        Self::Exact(ExactReal::Integer { is_neg, value })
    }

    pub fn real_decimal(
        is_neg: bool,
        base: u64,
        post_dot: u64,
        exponent_neg: bool,
        exponent: u64,
    ) -> Self {
        Self::Exact(ExactReal::Decimal {
            base,
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
#[derive(Debug, PartialEq, Clone, Copy, Arbitrary)]
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
        post_dot: u64,
        exponent: u64,
        exponent_neg: bool,
        is_neg: bool,
    },
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

    pub fn is_numeric(self) -> bool {
        !matches!(self, Self::Inf { .. } | Self::Nan { .. })
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
                post_dot,
                exponent,
                exponent_neg,
                is_neg,
            } => {
                let post_dot_10_power = (post_dot as f64).log10().ceil();
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
                post_dot,
                exponent,
                exponent_neg,
                is_neg,
            } if radix == 10 => {
                if is_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(base, radix)?);
                output.push('.');
                output.push_str(&Self::to_string_radix(post_dot, radix)?);
                output.push('e');
                if exponent_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(exponent, radix)?);
            }
            ExactReal::Decimal { .. } => return None,
        }
        Some(Box::from(output.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::ExactReal;
    use assert2::check;

    #[test]
    fn exact_to_inexact() {
        let exact = ExactReal::Decimal {
            base: 3,
            post_dot: 5,
            exponent: 2,
            exponent_neg: true,
            is_neg: false,
        };
        check!(exact.inexact() == 0.035);
        let exact = ExactReal::Decimal {
            base: 42,
            post_dot: 5020,
            exponent: 2,
            exponent_neg: false,
            is_neg: true,
        };
        check!(exact.inexact() == -4250.20);
        // Found by arbtest
        let exact = ExactReal::Decimal {
            base: 0,
            post_dot: 0,
            exponent: 0,
            exponent_neg: false,
            is_neg: false,
        };
        check!(exact.inexact() == 0.0);
    }
}
