//! Because exact number handling is quite important to Scheme

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
    InexactComplex(f64, f64),
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
        matches! { self, Self::Inexact(_) | Self::InexactComplex(_, _)}
    }

    pub fn display(self, radix: u32) -> Option<String> {
        if ![2, 8, 10, 16].contains(&radix) {
            return None;
        }

        let mut output = String::new();
        if !self.is_inexact() {
            output.push_str(match radix {
                2 => "#b",
                8 => "#o",
                10 => "",
                16 => "#x",
                _ => unreachable!(),
            });
        }

        match self {
            SchemeNumber::Exact(real) => output.push_str(&real.display(radix)?),
            SchemeNumber::ExactComplex { real, imaginary } => {
                output.push_str(&real.display(radix)?);
                if !imaginary.is_neg() {
                    output.push('+');
                }
                output.push_str(&imaginary.display(radix)?);
                output.push('i');
            }
            SchemeNumber::ExactPolar { modulus, argument } => {
                output.push_str(&modulus.display(radix)?);
                output.push('@');
                output.push_str(&argument.display(radix)?);
            }
            SchemeNumber::Inexact(v) => {
                output.push_str("#i");
                output.push_str(&v.to_string());
            }
            SchemeNumber::InexactComplex(re, im) => {
                output.push_str("#i");
                output.push_str(&re.to_string());
                if im >= 0.0 {
                    output.push('+');
                }
                output.push_str(&im.to_string());
                output.push('i');
            }
        }

        Some(output)
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
                (if is_neg { -1.0 } else { 1.0 })
                    * (base as f64
                        + (post_dot as f64 / 10f64.powf((post_dot as f64).log10().ceil())))
                    * 10f64.powf((if exponent_neg { -1.0 } else { 1.0 }) * exponent as f64)
            }
        }
    }

    fn to_string_radix(mut num: u64, radix: u32) -> Option<String> {
        let mut string_rev = Vec::new();
        while num > 0 {
            string_rev.push(char::from_digit((num % (radix as u64)) as u32, radix)?);
            num /= radix as u64;
        }
        Some(string_rev.into_iter().rev().collect())
    }

    pub fn display(self, radix: u32) -> Option<String> {
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
            } => {
                if is_neg {
                    output.push('-');
                }
                output.push_str(&Self::to_string_radix(base, radix)?);
                if post_dot != 0 {
                    output.push('.');
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
        }
        Some(output)
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
    }
}
