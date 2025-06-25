//! The Scheme "error" object
use gc_arena::{Collect, Gc, RefLock, Rootable};

use crate::{
    Value, ValuePtr,
    runtime::{
        lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        userstruct::UserStruct,
    },
    value::String,
};

/// The error object used for `error`
#[derive(Collect)]
#[collect(no_drop)]
pub struct ErrorObject<'gc> {
    pub(crate) message: String<'gc>,
    irritants: Vec<ValuePtr<'gc>>,
}

impl<'gc> ErrorObject<'gc> {
    pub fn new(message: String<'gc>, irritants: impl IntoIterator<Item = ValuePtr<'gc>>) -> Self {
        Self {
            message,
            irritants: irritants.into_iter().collect(),
        }
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct Error;

impl<'gc> NativeLambda<'gc> for Error {
    fn name(&self) -> &str {
        "error"
    }

    fn arity(&self) -> Arity {
        Arity::AtLeast(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let Value::String(s) = *args[0].borrow() else {
            return Err(anyhow::anyhow!(
                "error expects a string as its first argument"
            ))?;
        };

        let us = UserStruct::new_labeled::<Rootable![ErrorObject<'_>]>(
            &ctx,
            ErrorObject {
                message: s,
                irritants: args[1..].to_vec(),
            },
            "error",
        );

        Ok(LambdaReturn::Raise {
            error: Value::UserStruct(us).into_ptr(&ctx),
            is_continuable: false,
        })
    }
}

// Indicates support for error-object-message and error-object-irritants
#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct IsErrorObject;

impl<'gc> NativeLambda<'gc> for IsErrorObject {
    fn name(&self) -> &str {
        "is-error-object"
    }

    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let is_error_object = matches!(*args[0].borrow(), Value::Error(_))
            | matches!(*args[0].borrow(), Value::UserStruct(us) if us.is::<Rootable![ErrorObject<'_>]>());

        Ok(LambdaReturn::Return(vec![ctx.bool(is_error_object)]))
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct ErrorObjectMessage;

impl<'gc> NativeLambda<'gc> for ErrorObjectMessage {
    fn name(&self) -> &str {
        "error-object-message"
    }

    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let message = match *args[0].borrow() {
            Value::Error(e) => e.error_type.to_string(),
            Value::UserStruct(us) if us.is::<Rootable![ErrorObject<'_>]>() => us
                .downcast::<Rootable![ErrorObject<'_>]>()
                .unwrap()
                .message
                .borrow()
                .clone(),
            _ => {
                return Err(anyhow::anyhow!(
                    "error-object-message expects an error object as its first argument"
                ))?;
            }
        };

        let message = String::from(Gc::new(&ctx, RefLock::new(message)));

        Ok(LambdaReturn::Return(vec![
            Value::String(message).into_ptr(&ctx),
        ]))
    }
}
