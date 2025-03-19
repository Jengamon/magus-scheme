//! We provide 2 kinds of lambdas: native and compiled

use core::fmt;
use std::collections::HashMap;

use gc_arena::{Collect, Gc, Mutation, RefLock, Static};

use crate::{
    Fuel, ValuePtr,
    bytecode::ChunkPtr,
    compiler::World,
    environment::StackEnvironmentPtr,
    interpreter::{
        Context, Includer,
        thread::{Thread, ThreadFrame},
    },
};

use super::{error::SchemeErrorPtr, value::ContinuationPtr};

/// Possible errors
#[derive(thiserror::Error, Debug)]
pub enum LambdaError {
    #[error(transparent)]
    NonContinuable(anyhow::Error),
    #[error(transparent)]
    Continuable(#[from] anyhow::Error),
}

impl LambdaError {
    /// Create a non-continuable Rust lambda error
    pub fn non_continuable(err: impl Into<anyhow::Error>) -> Self {
        Self::NonContinuable(err.into())
    }
}

pub type DynamicWind<'gc> = Option<(Lambda<'gc>, Lambda<'gc>)>;

/// Possible things a lambda can return
/// If something marked `[call-end]` is returned, the lambda will not be called again.
#[derive(Debug)]
pub enum LambdaReturn<'gc> {
    /// Suspend the lambda, as it is waiting on an external value
    Waiting,
    /// Return the given values, pushing them to the stack
    ///
    /// `[call-end]`
    Return(Vec<ValuePtr<'gc>>),
    /// Call a continuation
    ///
    /// `[call-end]`
    Continue {
        cont: ContinuationPtr<'gc>,
        args: Vec<ValuePtr<'gc>>,
    },
    /// Raise the given value as an error
    ///
    /// `[call-end]`
    Raise {
        error: ValuePtr<'gc>,
        is_continuable: bool,
    },
    /// Propagate an error value
    ///
    /// `[call-end]`
    Propagate(SchemeErrorPtr<'gc>),

    /// Call a given lambda, and push the values it returns onto the stack
    Call {
        lambda: Lambda<'gc>,
        args: Vec<ValuePtr<'gc>>,
        dynamic_wind: DynamicWind<'gc>,
    },
    /// Call a given lambda, as a return value
    ///
    /// `[call-end]`
    TailCall {
        lambda: Lambda<'gc>,
        args: Vec<ValuePtr<'gc>>,
        dynamic_wind: DynamicWind<'gc>,
    },
    /// Set an exception handler for this frame
    ///
    /// If an error arises on a frame:
    /// - if a bytecode frame, the frame is popped unless a handler has been set, where execution takes and calls
    ///   the handler
    /// - if a native frame, the error run path is checked, then if the error is propagated (or a new error is
    ///   raised), the handler is checked if any (and the rest handles like a bytecode frame)
    SetExceptionHandler(Lambda<'gc>),
}

// Since Collect is not dyn-compatible, we create a "Collectable" trait implement
// for anything that implements Collect, and bound on this for NativeLambda
/// Trait used to ensure that native lambdas implement Collect
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not implement Collect",
    label = "doesn't implement Collect"
)]
trait Collectable {}
#[diagnostic::do_not_recommend]
impl<'gc, T: Collect<'gc>> Collectable for T {}

pub struct NativeLambdaContext<'a, 'gc> {
    pub self_ptr: NativeLambdaPtr<'gc>,
    /// Since we are in a borrow of the Thread, don't use ctx.thread, use this field instead
    pub thread_ref: &'a Thread<'gc>,
    pub thread_ctx: Context<'a, 'gc>,
    pub world: &'a World,
    pub stack: &'a [ValuePtr<'gc>],
    pub interner: &'a mut lasso::Rodeo,
    pub includer: &'a dyn Includer,
    pub fuel: &'a mut Fuel,
    pub env: StackEnvironmentPtr<'gc>,
    pub frames: &'a [ThreadFrame<'gc>],
}

impl<'gc> NativeLambdaContext<'_, 'gc> {
    /// Get the arity of a [`Lambda`]
    ///
    /// To prevent a double borrow, check the equality of lambda with the self pointer.
    /// If they refer to the same [`NativeLambda`], use the borrow to retrieve the lambda instead.
    pub fn get_arity(&self, native: &impl NativeLambda, lambda: Lambda<'gc>) -> Arity {
        if lambda == self.self_ptr {
            native.arity()
        } else {
            lambda.arity()
        }
    }
}

impl<'gc> std::ops::Deref for NativeLambdaContext<'_, 'gc> {
    type Target = Mutation<'gc>;
    fn deref(&self) -> &Self::Target {
        self.thread_ctx.mc
    }
}

/// A native lambda is a Rust-implemented lambda
///
/// # Notes
/// - implementations are defined by a [`World`] and are thus shared across all scripts
///   that use that `World`, depending on how that `World` defines them
#[expect(private_bounds)]
pub trait NativeLambda: std::fmt::Debug + Collectable {
    /// What is the arity of this lambda?
    fn arity(&self) -> Arity;

    /// Run in normal mode
    fn run<'gc>(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError>;
    /// Run when there is an error present
    fn error<'gc>(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
        err: SchemeErrorPtr<'gc>,
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let _ = (ctx, args);
        Ok(LambdaReturn::Propagate(err))
    }

    /// Create a version of `self` that will continue off where this function
    /// was called for this lambda (used in continuation impl)
    ///
    /// `None` signifies that this lambda can simply have its pointer
    /// copied as a continuation (it does not mutate `self`)
    fn continuation<'gc>(&self, mc: &Mutation<'gc>) -> Option<NativeLambdaPtr<'gc>> {
        let _ = mc;
        None
    }
}
pub type NativeLambdaPtr<'gc> = Gc<'gc, RefLock<dyn NativeLambda>>;
pub type LambdaResult<'gc> = Result<LambdaReturn<'gc>, LambdaError>;

/// A compiled lambda is a wrapper around a [`ChunkPtr`] with additional information about arity
pub type CompiledLambdaPtr<'gc> = Gc<'gc, CompiledLambda<'gc>>;
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct CompiledLambda<'gc> {
    #[collect(require_static)]
    pub(crate) arity: Arity,
    pub(crate) chunk: ChunkPtr<'gc>,
    pub(crate) upvalue_id: Option<usize>,
}

impl<'gc> CompiledLambda<'gc> {
    pub fn new(arity: Arity, chunk: ChunkPtr<'gc>) -> Self {
        Self {
            arity,
            chunk,
            upvalue_id: None,
        }
    }

    pub fn arity(&self) -> Arity {
        self.arity
    }

    pub fn chunk(&self) -> ChunkPtr<'gc> {
        self.chunk
    }

    pub fn label(&self, mc: &Mutation<'gc>, upvalue_id: usize) -> CompiledLambdaPtr<'gc> {
        Gc::new(
            mc,
            CompiledLambda {
                arity: self.arity,
                chunk: self.chunk,
                upvalue_id: Some(upvalue_id),
            },
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    /// Require an exact number of arguments
    Exact(usize),
    /// Require at least a certain number of arguments
    AtLeast(usize),
}

impl Arity {
    pub fn is_satisfied(self, len: usize) -> bool {
        match self {
            Self::Exact(exact) => exact == len,
            Self::AtLeast(minimum) => minimum <= len,
        }
    }

    pub fn minimum(self) -> usize {
        match self {
            Self::Exact(exact) => exact,
            Self::AtLeast(min) => min,
        }
    }
}

impl fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Exact(exact) => write!(f, "{exact}"),
            Arity::AtLeast(min) => write!(f, ">={min}"),
        }
    }
}

/// A wrapper to hide the type difference (for storage in values)
#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub enum Lambda<'gc> {
    Native(NativeLambdaPtr<'gc>),
    Compiled(CompiledLambdaPtr<'gc>),
}

impl Lambda<'_> {
    pub fn arity(self) -> Arity {
        match self {
            Self::Native(n) => n.borrow().arity(),
            Self::Compiled(c) => c.arity,
        }
    }

    /// Check if a lambda needs a label
    pub fn needs_label(self) -> bool {
        match self {
            Self::Compiled(c) => c.upvalue_id.is_none(),
            _ => false,
        }
    }

    /// Get a lambda's label (if any)
    pub fn get_label(self) -> Option<usize> {
        match self {
            Self::Compiled(c) => c.upvalue_id,
            _ => None,
        }
    }
}

impl<'gc> Lambda<'gc> {
    // convenience function to label compiled lambdas
    pub fn label(self, mc: &Mutation<'gc>, upvalue_id: usize) -> Self {
        match self {
            Self::Native(_) => self,
            Self::Compiled(c) => Self::Compiled(c.label(mc, upvalue_id)),
        }
    }
}

impl PartialEq for Lambda<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lambda::Native(sp), Lambda::Native(op)) => Gc::ptr_eq(*sp, *op),
            (Lambda::Compiled(sp), Lambda::Compiled(op)) => Gc::ptr_eq(*sp, *op),
            _ => false,
        }
    }
}
impl<'gc> PartialEq<NativeLambdaPtr<'gc>> for Lambda<'gc> {
    fn eq(&self, other: &NativeLambdaPtr<'gc>) -> bool {
        matches!(self, Lambda::Native(n) if Gc::ptr_eq(*n, *other))
    }
}
impl<'gc> PartialEq<CompiledLambdaPtr<'gc>> for Lambda<'gc> {
    fn eq(&self, other: &CompiledLambdaPtr<'gc>) -> bool {
        matches!(self, Lambda::Compiled(n) if Gc::ptr_eq(*n, *other))
    }
}
impl Eq for Lambda<'_> {}

impl std::fmt::Pointer for Lambda<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native(np) => write!(f, "{np:p}"),
            Self::Compiled(cp) => write!(f, "{cp:p}"),
        }
    }
}
