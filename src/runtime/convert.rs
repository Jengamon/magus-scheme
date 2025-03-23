use std::convert::Infallible;

use gc_arena::{Gc, Mutation, RefLock};

use crate::{bytecode, environment::StackEnvironmentPtr, runtime::userstruct::UserStruct};

use super::{
    lambda::Lambda,
    value::{self, ConsCell, ContinuationPtr, Symbol, Value},
};

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
            Value::Number(n) => n.to_integer_rounded(),
            _ => None,
        }
    }
}
/// Marker trait for things that are used as UserStructs
pub trait UserType {}
/// Mark a type as a user type
#[macro_export]
macro_rules! user_type {
    ($tp:ty) => {
        impl $crate::runtime::convert::UserType for $tp {}
    };
}
impl<'gc, T: UserType + 'static> FromValue<'gc> for &'gc T {
    fn from_value(value: Value<'gc>) -> Option<Self>
    where
        Self: Sized,
    {
        match value {
            Value::UserStruct(us) => us.downcast_static::<T>().ok(),
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
impl<'gc> IntoValue<'gc> for Value<'gc> {
    fn into_value(self, _mc: &Mutation<'gc>) -> Value<'gc> {
        self
    }
}
macro_rules! impl_into_value {
    (simple $tp:ty => $lbl:ident) => {
        impl<'gc> IntoValue<'gc> for $tp {
            fn into_value(self, _mc: &Mutation<'gc>) -> Value<'gc> {
                Value::$lbl(self)
            }
        }
    };

    (number $tp:ty) => {
        impl<'gc> IntoValue<'gc> for $tp {
            fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
                Value::Number(Gc::new(
                    mc,
                    $crate::value::Number::from_integer(self).unwrap(),
                ))
            }
        }
    };
}
// impl<'gc> IntoValue<'gc> for Lambda<'gc> {
//     fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
//         Value::Lambda(Gc::new(mc, RefLock::new(self)))
//     }
// }
impl<'gc, T: UserType + 'static> IntoValue<'gc> for T {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::UserStruct(UserStruct::new_static(mc, self))
    }
}
impl_into_value!(number u8);
impl_into_value!(number u16);
impl_into_value!(number u32);
impl_into_value!(number i8);
impl_into_value!(number i16);
impl_into_value!(number i32);
impl_into_value!(number isize);
impl_into_value!(number i64);
impl_into_value!(simple f64 => Inexact);
impl_into_value!(simple bool => Bool);
impl_into_value!(simple char => Char);
impl_into_value!(simple StackEnvironmentPtr<'gc> => Environment);
impl_into_value!(simple Lambda<'gc> => Lambda);
impl_into_value!(simple ContinuationPtr<'gc> => Continuation);
impl_into_value!(simple ConsCell<'gc> => Cons);
// impl_into_value!(simple LambdaPtr<'gc> => Lambda);
impl<'gc> IntoValue<'gc> for String {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::String(value::String::from(Gc::new(mc, RefLock::new(self))))
    }
}
impl<'gc> IntoValue<'gc> for std::sync::Arc<str> {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        Value::String(value::String::from(Gc::new(
            mc,
            RefLock::new(self.to_string()),
        )))
    }
}
impl<'gc> IntoValue<'gc> for bytecode::Constant {
    fn into_value(self, mc: &Mutation<'gc>) -> Value<'gc> {
        match self {
            Self::Symbol(s) => Value::Symbol(Symbol(s)),
            Self::Char(c) => c.into_value(mc),
            Self::Number(i) => i.into_value(mc),
            Self::Inexact(f) => f.into_value(mc),
            Self::String(s) => s.into_value(mc),
            Self::Bytevector(bv) => Value::Bytevector(
                Gc::new(
                    mc,
                    gc_arena::Static(im_rc::Vector::from_iter(bv.iter().copied())),
                )
                .into(),
            ),
        }
    }
}
