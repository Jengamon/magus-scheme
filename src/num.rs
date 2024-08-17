//! Because exact number handling is quite important to Scheme

#[derive(Debug, PartialEq, Clone, Copy)]
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
        post_dot: u64,
        exponent: u64,
        exponent_neg: bool,
        is_neg: bool,
    },
}

impl ExactReal {
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
