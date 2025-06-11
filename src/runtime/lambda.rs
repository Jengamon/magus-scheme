//! We provide 2 kinds of lambdas: native and compiled

use core::fmt;
use gc_arena::{Collect, Gc, Mutation, RefLock};
use std::rc::Rc;

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

use super::{
    error::SchemeErrorPtr,
    value::{self, ContinuationPtr, ParameterPtr},
};

/// Possible errors
#[derive(thiserror::Error, Debug)]
pub enum LambdaError {
    #[error(transparent)]
    NonContinuable(#[from] anyhow::Error),
    #[error(transparent)]
    Continuable(anyhow::Error),
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
    /// Return from an error handler
    ///
    /// `[call-end]`
    ReturnHandler,
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
        /// This gives up ownership of the environment. Only use for
        /// environments you created. (It will reparent it if its parent is None)
        env: Option<StackEnvironmentPtr<'gc>>,
    },
    /// Call a given lambda as an exception handler, and push the values it returns onto the stack
    CallHandler {
        lambda: Lambda<'gc>,
        exception: SchemeErrorPtr<'gc>,
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
    /// Get the value of the given parameter object, and push it to stack
    Parameter { parameter: ParameterPtr<'gc> },
}

impl std::fmt::Display for LambdaReturn<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LambdaReturn::Waiting => write!(f, "Waiting"),
            LambdaReturn::Return(_) => write!(f, "Return"),
            LambdaReturn::ReturnHandler => write!(f, "ReturnHandler"),
            LambdaReturn::Continue { .. } => write!(f, "Continue"),
            LambdaReturn::Raise { .. } => write!(f, "Raise"),
            LambdaReturn::Propagate(_) => write!(f, "Propagate"),
            LambdaReturn::Call { .. } => write!(f, "Call"),
            LambdaReturn::CallHandler { .. } => write!(f, "CallHandler"),
            LambdaReturn::TailCall { .. } => write!(f, "TailCall"),
            LambdaReturn::Parameter { .. } => write!(f, "Parameter"),
        }
    }
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
    pub fn get_arity(&self, native: &impl NativeLambda<'gc>, lambda: Lambda<'gc>) -> Arity {
        if lambda == self.self_ptr {
            native.arity()
        } else {
            lambda.arity()
        }
    }

    /// Convenience function to use the thread context true and false values
    pub fn bool(&self, v: bool) -> ValuePtr<'gc> {
        if v {
            self.thread_ctx.true_value
        } else {
            self.thread_ctx.false_value
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
pub trait NativeLambda<'gc>: std::fmt::Debug + Collectable {
    /// What is the arity of this lambda?
    fn arity(&self) -> Arity;

    /// Run in normal mode
    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError>;
    /// Run when there is an error present
    fn error(
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
    fn continuation(&self, mc: &Mutation<'gc>) -> Option<NativeLambdaPtr<'gc>> {
        let _ = mc;
        None
    }

    /// Provide a documentation string
    fn doc_string(&self) -> Option<&str> {
        None
    }
}
pub type NativeLambdaPtr<'gc> = Gc<'gc, RefLock<dyn NativeLambda<'gc> + 'gc>>;
pub type LambdaResult<'gc> = Result<LambdaReturn<'gc>, LambdaError>;

/// A compiled lambda is a wrapper around a [`ChunkPtr`] with additional information about arity
pub type CompiledLambdaPtr<'gc> = Gc<'gc, CompiledLambda<'gc>>;
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct CompiledLambda<'gc> {
    #[collect(require_static)]
    pub(crate) arity: Arity,
    pub(crate) chunk: ChunkPtr<'gc>,
    #[collect(require_static)]
    pub(crate) arg_names: Rc<[lasso::Spur]>,
    #[collect(require_static)]
    pub(crate) rest_name: Option<lasso::Spur>,
    pub(crate) upvalue_id: Option<usize>,
    pub(crate) doc_string: Gc<'gc, RefLock<Option<value::String<'gc>>>>,
}

impl<'gc> CompiledLambda<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        arity: Arity,
        chunk: ChunkPtr<'gc>,
        arg_names: impl IntoIterator<Item = lasso::Spur>,
        rest_name: Option<lasso::Spur>,
    ) -> Self {
        Self {
            arity,
            chunk,
            upvalue_id: None,
            arg_names: arg_names.into_iter().collect(),
            rest_name,
            doc_string: Gc::new(mc, RefLock::new(None)),
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
                arg_names: Rc::clone(&self.arg_names),
                rest_name: self.rest_name,
                doc_string: self.doc_string,
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
    /// Require a bounded number of arguments [min, max]
    Bounded { min: usize, max: usize },
}

impl Arity {
    pub fn is_satisfied(self, len: usize) -> bool {
        match self {
            Self::Exact(exact) => exact == len,
            Self::AtLeast(minimum) => minimum <= len,
            Self::Bounded { min, max } => min <= len && max >= len,
        }
    }

    pub fn minimum(self) -> usize {
        match self {
            Self::Exact(exact) => exact,
            Self::AtLeast(min) => min,
            Self::Bounded { min, .. } => min,
        }
    }
}

impl fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Exact(exact) => write!(f, "{exact}"),
            Arity::AtLeast(min) => write!(f, ">={min}"),
            Arity::Bounded { min, max } => write!(f, "[{min}..{max}]"),
        }
    }
}

/// A wrapper to hide the type difference (for storage in values)
#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub enum Lambda<'gc> {
    Native(NativeLambdaPtr<'gc>),
    Compiled(CompiledLambdaPtr<'gc>),
    // these are variants that only show up at runtime
    ClosureCompiled {
        compiled: CompiledLambdaPtr<'gc>,
        env: StackEnvironmentPtr<'gc>,
    },
}

impl Lambda<'_> {
    pub fn arity(self) -> Arity {
        match self {
            Self::Native(n) => n.borrow().arity(),
            Self::Compiled(c) => c.arity,
            Self::ClosureCompiled { compiled, .. } => compiled.arity,
        }
    }

    /// Check if a lambda needs a label
    pub fn needs_label(self) -> bool {
        match self {
            Self::Compiled(c) => c.upvalue_id.is_none(),
            Self::ClosureCompiled { compiled, .. } => compiled.upvalue_id.is_none(),
            _ => false,
        }
    }

    /// Get a lambda's label (if any)
    pub fn get_label(self) -> Option<usize> {
        match self {
            Self::Compiled(c) => c.upvalue_id,
            Self::ClosureCompiled { compiled, .. } => compiled.upvalue_id,
            _ => None,
        }
    }

    /// Get a lambda's doc string
    pub fn doc_string(&self, native_lam: Option<&dyn NativeLambda>) -> Option<String> {
        match self {
            Self::Compiled(c) => c.doc_string.borrow().map(|i| i.borrow().clone()),
            Self::ClosureCompiled { compiled, .. } => {
                compiled.doc_string.borrow().map(|i| i.borrow().clone())
            }
            Self::Native(n) => {
                if let Some(native) = native_lam {
                    // compare addresses, if same, we are the same pointer, just use us
                    let n_addr = n.as_ptr().addr();
                    let our_addr = (native as *const dyn NativeLambda).addr();
                    if n_addr == our_addr {
                        native.doc_string().map(str::to_string)
                    } else {
                        n.borrow().doc_string().map(str::to_string)
                    }
                } else {
                    n.borrow().doc_string().map(str::to_string)
                }
            }
        }
    }
}

impl<'gc> Lambda<'gc> {
    // convenience function to label compiled lambdas
    pub fn label(self, mc: &Mutation<'gc>, upvalue_id: usize) -> Self {
        match self {
            Self::Native(_) => self,
            Self::Compiled(c) => Self::Compiled(c.label(mc, upvalue_id)),
            Self::ClosureCompiled { compiled, env } => Self::ClosureCompiled {
                compiled: compiled.label(mc, upvalue_id),
                env,
            },
        }
    }

    /// (Try to) set a lambda's doc string
    pub fn set_doc_string(&self, mc: &Mutation<'gc>, to: Option<value::String<'gc>>) -> bool {
        match self {
            Self::Native(_) => false,
            Self::Compiled(c) => {
                *c.doc_string.borrow_mut(mc) = to;
                true
            }
            Self::ClosureCompiled { compiled, .. } => {
                *compiled.doc_string.borrow_mut(mc) = to;
                true
            }
        }
    }
}

impl PartialEq for Lambda<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lambda::Native(sp), Lambda::Native(op)) => Gc::ptr_eq(*sp, *op),
            (Lambda::Compiled(sp), Lambda::Compiled(op)) => Gc::ptr_eq(*sp, *op),
            (
                Lambda::ClosureCompiled { compiled, env },
                Lambda::ClosureCompiled {
                    compiled: ocompiled,
                    env: oenv,
                },
            ) => Gc::ptr_eq(*compiled, *ocompiled) && Gc::ptr_eq(*env, *oenv),
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
            Self::Native(np) => write!(f, "0x{:x}", (&raw const *np.borrow()).addr()),
            Self::Compiled(cp) | Self::ClosureCompiled { compiled: cp, .. } => {
                write!(f, "0x{:x}", (&raw const *cp.as_ref()).addr())
            }
        }
    }
}
