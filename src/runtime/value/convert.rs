use std::convert::Infallible;

use gc_arena::{Gc, Mutation, RefLock};

use crate::compiler::environment::EnvironmentPtr;

use super::{Lambda, Value};

pub trait FromValue<'gc> {
    fn from_value(value: Value<'gc>) -> Option<Self>
    where
        Self: Sized;
}
impl<'gc> FromValue<'gc> for i64 {
    fn from_value(value: Value<'gc>) -> Option<Self>
    where
        Self: Sized,
    {
        match value {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }
}

pub trait TryIntoValue<'gc> {
    type Error;
    fn try_into_value(self, mc: &Mutation<'gc>) -> Result<Value<'gc>, Self::Error>;
}
impl<'gc, T: IntoValue<'gc>> TryIntoValue<'gc> for T {
    type Error = Infallible;
    fn try_into_value(self, mc: &Mutation<'gc>) -> Result<Value<'gc>, Self::Error> {
        Ok(self.into_value(mc))
    }
}

pub trait IntoValue<'gc> {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc>;
}
macro_rules! impl_into_value {
    (simple $tp:ty => $lbl:ident) => {
        impl<'gc> IntoValue<'gc> for $tp {
            fn into_value(self, _mc: &Mutation<'gc>) -> Value<'gc> {
                Value::$lbl(self)
            }
        }
    };
}
impl<'gc> IntoValue<'gc> for Lambda<'gc> {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::Lambda(Gc::new(mc, RefLock::new(self)))
    }
}
impl<'gc> IntoValue<'gc> for u8 {
    fn into_value(self, _mc: &Mutation<'gc>) -> Value<'gc> {
        Value::Number(self as i64)
    }
}
impl_into_value!(simple i64 => Number);
impl_into_value!(simple f64 => Inexact);
impl_into_value!(simple bool => Bool);
impl_into_value!(simple char => Char);
impl_into_value!(simple EnvironmentPtr<'gc> => Environment);
impl<'gc> IntoValue<'gc> for String {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::String(Gc::new(mc, RefLock::new(self)))
    }
}
