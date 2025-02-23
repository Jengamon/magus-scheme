//! Defines "macros" which are bits of Rust-code that can interact with the interpreter
//! as it interprets code.
//! This means:
//! - interact with the current environment
//! - evaluate snippets of code in the current environment (but suspending on [`Program`]s)

use gc_arena::Gc;

use crate::{
    bytecode::SourceData, compiler::Program, create_environment_pair,
    environment::StackEnvironmentPtr,
};

use super::Context;

pub enum MacroReturn<'gc> {
    /// Evaluate a given `Program` in the current scope, then
    /// call back into this macro.
    Evaluate(Program<'gc>),

    /// Finish this macro
    Finish,
}

create_environment_pair!(
    pub Macro => MacroPtr<'gc>
);
pub type MacroPtr<'gc> = Gc<'gc, dyn Macro>;
pub trait Macro: std::fmt::Debug {
    /// Evaluate a macro, until it finishes
    ///
    /// # Parameters
    /// - `source`: [`SourceData`] of this invocation
    /// - `mc`: [`gc_arena`] mutation context
    /// - `env`: [`StackEnvironmentPtr`] to the current value environment
    /// - `macro_env`: [`MacroEnvironmentPtr`] to the current value environment
    /// - `args`: external representation corresponding to passed in syntax
    fn evaluate<'gc>(
        &mut self,
        source: SourceData,
        mc: Context<'gc>,
        env: StackEnvironmentPtr<'gc>,
        macro_env: MacroEnvironmentPtr<'gc>,
        args: &[Program<'gc>],
    ) -> anyhow::Result<MacroReturn<'gc>>;
}
