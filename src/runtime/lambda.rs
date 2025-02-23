//! We provide 2 kinds of lambdas: native and compiled

use gc_arena::{Collect, Gc};

use crate::{
    Fuel, ValuePtr,
    bytecode::ChunkPtr,
    compiler::Program,
    environment::StackEnvironmentPtr,
    interpreter::{Context, syntax::MacroEnvironmentPtr, thread::ThreadFrame},
};

use super::error::SchemeErrorPtr;

/// Possible things a lambda can return
/// If something marked `[call-end]` is returned, the lambda will not be called again.
pub enum LambdaReturn<'gc> {
    /// Return the given values, push them to stack
    ///
    /// `[call-end]`
    Return(Vec<ValuePtr<'gc>>),
    /// Raise the given value
    ///
    /// `[call-end]`
    Raise(ValuePtr<'gc>),
    /// Propagate an error value
    ///
    /// `[call-end]`
    Propagate(SchemeErrorPtr<'gc>),

    /// Evaluate a given program in the current environment
    Eval(Program<'gc>),
    /// Evaluate a given program in the current environment
    ///
    /// `[call-end]`
    TailEval(Program<'gc>),

    /// Call a given lambda
    Call {
        lambda: Lambda<'gc>,
        args: Vec<ValuePtr<'gc>>,
    },
    /// Call a given lambda
    ///
    /// `[call-end]`
    TailCall {
        lambda: Lambda<'gc>,
        args: Vec<ValuePtr<'gc>>,
    },
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

/// A native lambda is a Rust-implemented lambda
pub trait NativeLambda: std::fmt::Debug + Collectable {
    /// What is the arity of this lambda?
    fn arity(&self) -> Arity;

    /// Run in normal mode
    fn run<'gc>(
        &mut self,
        ctx: Context<'gc>,
        interner: &mut lasso::Rodeo,
        fuel: &mut Fuel,
        env: StackEnvironmentPtr<'gc>,
        macro_env: MacroEnvironmentPtr<'gc>,
        frames: &[ThreadFrame<'gc>],
        args: &[ValuePtr<'gc>],
    ) -> LambdaReturn<'gc>;
    /// Run when there is an error present
    fn error<'gc>(
        &mut self,
        ctx: Context<'gc>,
        interner: &mut lasso::Rodeo,
        fuel: &mut Fuel,
        env: StackEnvironmentPtr<'gc>,
        macro_env: MacroEnvironmentPtr<'gc>,
        frames: &[ThreadFrame<'gc>],
        args: &[ValuePtr<'gc>],
        err: SchemeErrorPtr<'gc>,
    ) -> LambdaReturn<'gc> {
        let _ = (ctx, interner, env, macro_env);
        LambdaReturn::Propagate(err)
    }
}
pub type NativeLambdaPtr<'gc> = Gc<'gc, dyn NativeLambda>;

/// A compiled lambda is a wrapper around a [`ChunkPtr`](crate::bytecode::ChunkPtr) with additional information about arity
pub type CompiledLambdaPtr<'gc> = Gc<'gc, CompiledLambda<'gc>>;
#[derive(Debug)]
pub struct CompiledLambda<'gc> {
    arity: Arity,
    chunk: ChunkPtr<'gc>,
}

#[derive(Debug, Clone, Copy)]
pub enum Arity {
    /// Require an exact number of arguments
    Exact(usize),
    /// Require at least a certain number of arguments
    AtLeast(usize),
}

impl Arity {
    fn satisfies(self, len: usize) -> bool {
        match self {
            Self::Exact(exact) => exact == len,
            Self::AtLeast(minimum) => minimum <= len,
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

impl PartialEq for Lambda<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lambda::Native(sp), Lambda::Native(op)) => Gc::ptr_eq(*sp, *op),
            (Lambda::Compiled(sp), Lambda::Compiled(op)) => Gc::ptr_eq(*sp, *op),
            _ => false,
        }
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
