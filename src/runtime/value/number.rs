use core::fmt;
use std::sync::LazyLock;

use gc_arena::{Collect, Gc, Mutation};
use num::bigint::{ParseBigIntError, Sign};
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

// Ideally, we forget that the underlying type is not an i64, but is actually a BigInt
// (which is *also* why we allocate the numbers in the GC, so that they can be as big as they
// want, but remain copy)
impl Number {
    pub const ZERO: Number = Number::Integer(BigInt::ZERO);

    pub fn into_ptr<'gc>(self, mc: &Mutation<'gc>) -> NumberPtr<'gc> {
        Gc::new(mc, self)
    }

    pub fn from_integer(i: impl ToPrimitive + Integer) -> Option<Self> {
        Some(Number::Integer(Self::i64_to_bigint(i.to_i64()?)))
    }

    pub fn from_inexact(f: f64) -> Option<Self> {
        Some(Self::simplify(BigRational::from_float(f)?))
    }

    pub fn from_rational(r: BigRational) -> Self {
        Self::simplify(r)
    }

    fn i64_to_bigint(i: i64) -> BigInt {
        match i {
            0 => BigInt::ZERO,
            i if i.is_negative() => BigInt::from_bytes_be(Sign::Minus, &i.abs().to_be_bytes()),
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

    pub fn one() -> Number {
        Number::Integer(BigInt::new(Sign::Plus, vec![1]))
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
        } else if r.numer() == &BigInt::ZERO {
            Number::Integer(BigInt::ZERO)
        } else {
            Number::Rational(r)
        }
    }

    pub fn recip(&self) -> Number {
        static ONE: LazyLock<BigInt> = LazyLock::new(|| BigInt::new(Sign::Plus, vec![1]));
        Self::simplify(match self {
            Self::Integer(i) => BigRational::new(ONE.clone(), i.clone()),
            Self::Rational(r) => r.recip(),
        })
    }

    pub fn gcd(&self, rhs: &Number) -> Number {
        static ONE: LazyLock<BigInt> = LazyLock::new(|| BigInt::new(Sign::Plus, vec![1]));
        // luckily ratios are stored in reduced form, so computing these are simple!
        match (self, rhs) {
            (Self::Integer(i), Self::Integer(i2)) => Self::Integer(i.gcd(i2)),
            (Self::Integer(i), Self::Rational(r)) | (Self::Rational(r), Self::Integer(i)) => {
                Self::simplify(BigRational::new(i.gcd(r.numer()), ONE.lcm(r.denom())))
            }
            (Self::Rational(r), Self::Rational(r2)) => Self::simplify(BigRational::new(
                r.numer().gcd(r2.numer()),
                r.denom().lcm(r2.denom()),
            )),
        }
    }

    pub fn lcm(&self, rhs: &Number) -> Number {
        static ONE: LazyLock<BigInt> = LazyLock::new(|| BigInt::new(Sign::Plus, vec![1]));
        // luckily ratios are stored in reduced form, so computing these are simple!
        match (self, rhs) {
            (Self::Integer(i), Self::Integer(i2)) => Self::Integer(i.lcm(i2)),
            (Self::Integer(i), Self::Rational(r)) | (Self::Rational(r), Self::Integer(i)) => {
                Self::simplify(BigRational::new(i.lcm(r.numer()), ONE.gcd(r.denom())))
            }
            (Self::Rational(r), Self::Rational(r2)) => Self::simplify(BigRational::new(
                r.numer().lcm(r2.numer()),
                r.denom().gcd(r2.denom()),
            )),
        }
    }

    pub fn is_positive(&self) -> bool {
        match self {
            Self::Integer(i) => i.sign() == Sign::Plus,
            Self::Rational(r) => {
                let nsign = r.numer().sign();
                let dsign = r.denom().sign();
                // the signs being the same means we represent a positive number!
                // (-3/-2) ~ (3/2)
                if nsign != Sign::NoSign {
                    nsign == dsign
                } else {
                    // numerator is 0, so only check the denominator sign
                    // which cannot also be 0
                    dsign == Sign::Plus
                }
            }
        }
    }

    pub fn is_negative(&self) -> bool {
        match self {
            Self::Integer(i) => i.sign() == Sign::Minus,
            Self::Rational(r) => {
                let nsign = r.numer().sign();
                let dsign = r.denom().sign();
                // the signs being different means we represent a negative number!
                // (-3/2) ~ (3/-2)
                if nsign != Sign::NoSign {
                    nsign != dsign
                } else {
                    // numerator is 0, so only check the denominator sign
                    // which cannot also be 0
                    dsign == Sign::Minus
                }
            }
        }
    }

    // TODO For Scheme `expt` we want support for complex numbers
    // (so that even roots of negative numbers can be caluclated), and we
    // use the `BigRational::pow for BigInt` and simplify the rational.
}

impl From<BigInt> for Number {
    fn from(value: BigInt) -> Self {
        Self::Integer(value)
    }
}

impl From<BigRational> for Number {
    fn from(value: BigRational) -> Self {
        Self::Rational(value)
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
        -&self
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
        &self + &rhs
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
        &self - &rhs
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

impl std::ops::Mul<f64> for Number {
    type Output = f64;
    fn mul(self, rhs: f64) -> Self::Output {
        self.to_inexact() * rhs
    }
}
impl std::ops::Mul<Number> for f64 {
    type Output = f64;
    fn mul(self, rhs: Number) -> Self::Output {
        self * rhs.to_inexact()
    }
}
impl std::ops::Mul for &Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(i), Number::Integer(i2)) => Number::Integer(i * i2),
            (Number::Rational(r), Number::Integer(i))
            | (Number::Integer(i), Number::Rational(r)) => {
                // mulition is reflexive, so we can do
                Number::simplify(r * BigRational::from_integer(i.clone()))
            }
            (Number::Rational(r), Number::Rational(r2)) => Number::simplify(r * r2),
        }
    }
}
impl std::ops::Mul<f64> for &Number {
    type Output = f64;
    fn mul(self, rhs: f64) -> Self::Output {
        self.to_inexact() * rhs
    }
}
impl std::ops::Mul<&Number> for f64 {
    type Output = f64;
    fn mul(self, rhs: &Number) -> Self::Output {
        self * rhs.to_inexact()
    }
}
impl std::ops::Mul<Number> for Number {
    type Output = Number;
    fn mul(self, rhs: Number) -> Self::Output {
        &self * &rhs
    }
}

impl std::ops::Div<f64> for Number {
    type Output = f64;
    fn div(self, rhs: f64) -> Self::Output {
        self.to_inexact() / rhs
    }
}
impl std::ops::Div<Number> for f64 {
    type Output = f64;
    fn div(self, rhs: Number) -> Self::Output {
        self / rhs.to_inexact()
    }
}
impl std::ops::Div for &Number {
    type Output = Number;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(i), Number::Integer(i2)) => {
                Number::simplify(BigRational::new(i.clone(), i2.clone()))
            }
            (Number::Rational(r), Number::Integer(i)) => {
                Number::simplify(r / BigRational::from_integer(i.clone()))
            }
            (Number::Integer(i), Number::Rational(r)) => {
                Number::simplify(BigRational::from_integer(i.clone()) / r)
            }
            (Number::Rational(r), Number::Rational(r2)) => Number::simplify(r / r2),
        }
    }
}
impl std::ops::Div<f64> for &Number {
    type Output = f64;
    fn div(self, rhs: f64) -> Self::Output {
        self.to_inexact() / rhs
    }
}
impl std::ops::Div<&Number> for f64 {
    type Output = f64;
    fn div(self, rhs: &Number) -> Self::Output {
        self / rhs.to_inexact()
    }
}
impl std::ops::Div<Number> for Number {
    type Output = Number;
    fn div(self, rhs: Number) -> Self::Output {
        &self / &rhs
    }
}

impl std::ops::Rem<f64> for Number {
    type Output = f64;
    fn rem(self, rhs: f64) -> Self::Output {
        self.to_inexact() % rhs
    }
}
impl std::ops::Rem<Number> for f64 {
    type Output = f64;
    fn rem(self, rhs: Number) -> Self::Output {
        self % rhs.to_inexact()
    }
}
impl std::ops::Rem for &Number {
    type Output = Number;
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(i), Number::Integer(i2)) => Number::Integer(i % i2),
            (Number::Rational(r), Number::Integer(i)) => {
                Number::simplify(r % BigRational::from_integer(i.clone()))
            }
            (Number::Integer(i), Number::Rational(r)) => {
                Number::simplify(BigRational::from_integer(i.clone()) % r)
            }
            (Number::Rational(r), Number::Rational(r2)) => Number::simplify(r % r2),
        }
    }
}
impl std::ops::Rem<f64> for &Number {
    type Output = f64;
    fn rem(self, rhs: f64) -> Self::Output {
        self.to_inexact() % rhs
    }
}
impl std::ops::Rem<&Number> for f64 {
    type Output = f64;
    fn rem(self, rhs: &Number) -> Self::Output {
        self % rhs.to_inexact()
    }
}
impl std::ops::Rem<Number> for Number {
    type Output = Number;
    fn rem(self, rhs: Number) -> Self::Output {
        &self % &rhs
    }
}

impl num::Zero for Number {
    fn zero() -> Self {
        Self::ZERO
    }

    fn is_zero(&self) -> bool {
        match self {
            Number::Integer(i) => i.is_zero(),
            Number::Rational(r) => r.is_zero(),
        }
    }
}
impl num::One for Number {
    fn one() -> Self {
        Self::one()
    }

    fn is_one(&self) -> bool
    where
        Self: PartialEq,
    {
        match self {
            Number::Integer(i) => i.is_one(),
            // Just in-case something unsimplified comes by, we are still technically correct
            Number::Rational(r) => r.numer().is_one() && r.denom().is_one(),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum NumberFromStrError {
    #[error(transparent)]
    Integer(#[from] ParseBigIntError),
    #[error("cannot make rational over 0")]
    RatioDenominatorZero,
}

impl num::Num for Number {
    type FromStrRadixErr = NumberFromStrError;
    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        use num::Zero;

        if str.contains('/') {
            // Split at / and read both sides, and simplify the number
            let parts = str.splitn(2, '/').collect::<Vec<_>>();
            // From splitn, we know parts has length 2, and it *definitely* contains a / so just treat as fraction parts
            let numer = BigInt::from_str_radix(parts[0], radix)?;
            let denom = BigInt::from_str_radix(parts[1], radix)?;
            if denom.is_zero() {
                Err(NumberFromStrError::RatioDenominatorZero)
            } else {
                Ok(Number::simplify(BigRational::new(numer, denom)))
            }
        } else {
            Ok(Number::Integer(BigInt::from_str_radix(str, radix)?))
        }
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
