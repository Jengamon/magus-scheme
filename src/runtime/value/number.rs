use core::fmt;
use std::sync::LazyLock;

use gc_arena::{Collect, Gc};
use num::bigint::Sign;
use num::{BigInt, BigRational, Complex, Integer, ToPrimitive};

pub type ComplexNumberPtr<'gc> = Gc<'gc, ComplexNumber>;
/// A ComplexNumber handles any form of exact number needed
#[derive(Debug, Clone, Collect, PartialEq, Eq, Hash)]
#[collect(require_static)]
pub struct ComplexNumber(Complex<Number>);

// Act as a wrapper around a Complex<Number> that happens to be Collect
impl std::ops::Deref for ComplexNumber {
    type Target = Complex<Number>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type NumberPtr<'gc> = Gc<'gc, Number>;
/// A Number is an immutable value that can either be an integer or rational
#[derive(Debug, Clone, Collect, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[collect(require_static)]
pub enum Number {
    Integer(BigInt),
    Rational(BigRational),
}

// TODO Implement num::Num on Number<'gc> (along with ops::Neg and num::Signed)
// So that Number can simply be the internal type used for ComplexNumber

// TODO We will want helpers out the wazoo to interact with
// TODO Implement Add, Sub, Mul, Div, Rem (basically math operations)
// (so complex gets Num for free ish)
// We get comparisons for free (thanks Rust derive macros)
// TODO Provide gcd and lcm between 2 numbers (as Scheme will want those)
// num provides these functions for integers, and there is an analytic solution
// for GCD and LCM of ratios (where integers are treated as their value over 1)
// Ideally, we forget that the underlying type is not an i64, but is actually a BigInt
// (which is *also* why we allocate the numbers in the GC, so that they can be as big as they
// want, but remain copy)
impl Number {
    pub const ZERO: Number = Number::Integer(BigInt::ZERO);

    pub fn from_integer(i: impl ToPrimitive + Integer) -> Option<Self> {
        Some(Number::Integer(Self::i64_to_bigint(i.to_i64()?)))
    }

    pub fn from_inexact(f: f64) -> Option<Self> {
        Some(Number::Rational(BigRational::from_float(f)?))
    }

    fn i64_to_bigint(i: i64) -> BigInt {
        match i {
            0 => BigInt::ZERO,
            i if i.is_negative() => BigInt::from_bytes_be(Sign::Minus, &i.to_be_bytes()),
            i => BigInt::from_bytes_be(Sign::Plus, &i.to_be_bytes()),
        }
    }

    // `None` if denominator is 0
    pub fn from_ratio(
        numer: impl ToPrimitive + Integer,
        denom: impl ToPrimitive + Integer,
    ) -> Option<Self> {
        let denom = Self::i64_to_bigint(denom.to_i64()?);
        if denom == BigInt::ZERO {
            return None;
        }

        let numer = Self::i64_to_bigint(numer.to_i64()?);

        Some(Number::Rational(BigRational::new(numer, denom)))
    }

    // Rounds rationals to the nearest integer
    pub fn to_integer_rounded(&self) -> Option<i64> {
        use num::ToPrimitive;

        match self {
            Self::Integer(i) => i.to_i64(),
            Self::Rational(r) => r.to_integer().to_i64(),
        }
    }

    pub fn to_inexact(&self) -> f64 {
        fn bigint_to_float(b: &BigInt) -> f64 {
            use num::ToPrimitive;
            b.to_f64().unwrap_or_else(|| match b.sign() {
                Sign::NoSign => 0.,
                Sign::Plus => f64::INFINITY,
                Sign::Minus => f64::NEG_INFINITY,
            })
        }

        match self {
            Self::Integer(b) => bigint_to_float(b),
            Self::Rational(r) => bigint_to_float(r.numer()) / bigint_to_float(r.denom()),
        }
    }

    // helper to convert down to Integer variant if Rational is over 1
    fn simplify(r: BigRational) -> Number {
        static ONE: LazyLock<BigInt> = LazyLock::new(|| BigInt::new(Sign::Plus, vec![1]));
        static NEG_ONE: LazyLock<BigInt> = LazyLock::new(|| BigInt::new(Sign::Minus, vec![1]));

        if r.denom() == &*ONE {
            Number::Integer(r.numer().clone())
        } else if r.denom() == &*NEG_ONE {
            let (_, num) = r.numer().clone().into_parts();
            Number::Integer(BigInt::from_biguint(Sign::Minus, num))
        } else {
            Number::Rational(r)
        }
    }

    pub fn gcd(self, rhs: &Number) -> Number {
        todo!()
    }

    pub fn lcm(self, rhs: &Number) -> Number {
        todo!()
    }
}

// impl partial_cmp for comparisons to inexact numbers
impl PartialEq<f64> for Number {
    fn eq(&self, other: &f64) -> bool {
        self.to_inexact() == *other
    }
}
impl PartialOrd<f64> for Number {
    fn partial_cmp(&self, other: &f64) -> Option<std::cmp::Ordering> {
        self.to_inexact().partial_cmp(other)
    }
}
impl PartialEq<Number> for f64 {
    fn eq(&self, other: &Number) -> bool {
        *self == other.to_inexact()
    }
}
impl PartialOrd<Number> for f64 {
    fn partial_cmp(&self, other: &Number) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.to_inexact())
    }
}

// Operation wrappers to make the rest of the codebase happy
impl std::ops::Neg for Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        match self {
            Number::Integer(i) => Number::Integer(i.neg()),
            Number::Rational(r) => Number::Rational(r.neg()),
        }
    }
}
impl std::ops::Neg for &Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        match self {
            Number::Integer(i) => Number::Integer(i.neg()),
            Number::Rational(r) => Number::Rational(r.neg()),
        }
    }
}
impl std::ops::Add for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(i), Number::Integer(i2)) => Number::Integer(i + i2),
            (Number::Rational(r), Number::Integer(i))
            | (Number::Integer(i), Number::Rational(r)) => {
                // addition is reflexive, so we can do
                Self::simplify(r + BigRational::from_integer(i))
            }
            (Number::Rational(r), Number::Rational(r2)) => Self::simplify(r + r2),
        }
    }
}
impl std::ops::Add<f64> for Number {
    type Output = f64;
    fn add(self, rhs: f64) -> Self::Output {
        self.to_inexact() + rhs
    }
}
impl std::ops::Add<Number> for f64 {
    type Output = f64;
    fn add(self, rhs: Number) -> Self::Output {
        self + rhs.to_inexact()
    }
}
impl std::ops::Add for &Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(i), Number::Integer(i2)) => Number::Integer(i + i2),
            (Number::Rational(r), Number::Integer(i))
            | (Number::Integer(i), Number::Rational(r)) => {
                // addition is reflexive, so we can do
                Number::simplify(r + BigRational::from_integer(i.clone()))
            }
            (Number::Rational(r), Number::Rational(r2)) => Number::simplify(r + r2),
        }
    }
}
impl std::ops::Add<f64> for &Number {
    type Output = f64;
    fn add(self, rhs: f64) -> Self::Output {
        self.to_inexact() + rhs
    }
}
impl std::ops::Add<&Number> for f64 {
    type Output = f64;
    fn add(self, rhs: &Number) -> Self::Output {
        self + rhs.to_inexact()
    }
}
impl std::ops::Sub for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(r), Number::Integer(r2)) => Number::Integer(r - r2),
            (Number::Rational(r), Number::Integer(i)) => {
                Self::simplify(r - BigRational::from_integer(i))
            }
            (Number::Integer(i), Number::Rational(r)) => {
                Self::simplify(BigRational::from_integer(i) - r)
            }
            (Number::Rational(r), Number::Rational(r2)) => Self::simplify(r - r2),
        }
    }
}
impl std::ops::Sub<f64> for Number {
    type Output = f64;
    fn sub(self, rhs: f64) -> Self::Output {
        self.to_inexact() - rhs
    }
}
impl std::ops::Sub<Number> for f64 {
    type Output = f64;
    fn sub(self, rhs: Number) -> Self::Output {
        self - rhs.to_inexact()
    }
}
impl std::ops::Sub for &Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(r), Number::Integer(r2)) => Number::Integer(r - r2),
            (Number::Rational(r), Number::Integer(i)) => {
                Number::simplify(r - BigRational::from_integer(i.clone()))
            }
            (Number::Integer(i), Number::Rational(r)) => {
                Number::simplify(BigRational::from_integer(i.clone()) - r)
            }
            (Number::Rational(r), Number::Rational(r2)) => Number::simplify(r - r2),
        }
    }
}
impl std::ops::Sub<f64> for &Number {
    type Output = f64;
    fn sub(self, rhs: f64) -> Self::Output {
        self.to_inexact() - rhs
    }
}
impl std::ops::Sub<&Number> for f64 {
    type Output = f64;
    fn sub(self, rhs: &Number) -> Self::Output {
        self - rhs.to_inexact()
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // we might want to customize later
        match self {
            Number::Integer(i) => write!(f, "{i}"),
            Number::Rational(r) => write!(f, "{r}"),
        }
    }
}
