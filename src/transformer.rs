use gc_arena::{unsize, Collect, Gc, Mutation, RefLock};

use crate::{
    environment::EnvironmentPtr,
    treewalk::{virtual_inst::VirtualInstruction, Context, StackValue, TreewalkExecutor},
    value::ValuePtr,
    Fuel,
};

/// An implementation of a macro
pub trait Macro<'gc>: Collect + core::fmt::Debug {
    /// Is this macro's output only determined by its inputs given these arguments
    ///
    /// If this returns true, any call to this macro might be replaced with the call's output
    fn is_pure(&self, _args: &[StackValue<'gc>]) -> bool {
        false
    }

    // TODO Add function that can look at a virtual instruction
    // and either mark it as a "reserved form" and error if malformed
    // or mark it to ignore and read it
    fn is_form(
        &self,
        _exec: &TreewalkExecutor<'gc>,
        _inst: &VirtualInstruction<'gc>,
    ) -> Option<Result<(), anyhow::Error>> {
        Some(Ok(()))
    }

    /// Produce the output of the macro
    ///
    /// Note: pure macros are allowed to mutate themselves, so long as the output is *entirely* determined
    /// by the inputs
    fn rewrite(
        &mut self,
        ctx: &Context<'gc>,
        executor: &mut TreewalkExecutor<'gc>,
        fuel: &mut Fuel,
    ) -> anyhow::Result<MacroReturn<'gc>>;
}
pub type MacroPtr<'gc> = Gc<'gc, RefLock<dyn Macro<'gc> + 'gc>>;

pub fn macro_to_ptr<'gc, M>(mc: &'gc Mutation<'gc>, mcr: M) -> MacroPtr<'gc>
where
    M: Macro<'gc> + 'gc,
{
    unsize!(Gc::new(mc, RefLock::new(mcr)) => RefLock<dyn Macro<'gc>>)
}

#[derive(Debug, Clone)]
pub enum MacroReturn<'gc> {
    Return {
        inst: Vec<MacroInstruction<'gc>>,
        ret: ValuePtr<'gc>,
    },
    Suspend,
}

/// The value type of `syntax-rules`
#[expect(dead_code)]
pub struct SyntaxRules {}

// Have stuff to tell the Treewalk to do the things here
// Fundamental forms: define, lambda, quote, if, define-syntax, let-syntax, letrec-syntax, syntax-rules, set!
// and that's it.
//
// macros are kept in a separate namespace in environments, so they are *not* values. (however, the output of `syntax-rules`
// is a transformer and *is* a value)
// We do have a similar pattern as lambdas.
//
// A macro optimization is if the macro considers itself "pure" or not. A "pure" macro might be JIT'ed out into
// the instructions that result from calling the macro.
//
// Macros are *contextually* pure, meaning that to determine the purity of a macro, it can look at the stack it
// will be called with (ofc the pure check is given with &self to make life easier)
/// Instructions that a macro can perform
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum MacroInstruction<'gc> {
    /// Execute code in the given environment, pushing the result to the top of stack
    // TODO Investigate if we need the separate environment ptr in-lieu of adding a "restore envirnment"
    // instruction (for let and friends)
    // ANSWER to implement hygenic syntax-rules macros, this is might be necessary, as any code it evaluates
    // has to be in the "original" environment
    // but that would be the same as changing the environment, evaluating them all followed by as restore
    // instruction, so we might be able to jure remove this
    // *but* I'd want to see it work first before doing
    Evaluate(StackValue<'gc>),
    /// Sets the environment
    SetEnvironment(EnvironmentPtr<'gc>),
    /// Pops the top of the stack, and `define`s the given name as that value
    Define {
        #[collect(require_static)]
        name: lasso::Spur,
    },
    /// Pops the top of the stack, and `set!`s the given name as that value
    ///
    /// Will set the error of a scope if the name is not found in it.
    SetBang {
        #[collect(require_static)]
        name: lasso::Spur,
    },
    /// Will call the function at stack[len - args - 1] with stack[len-args..] as arguments
    CallFunction { args: usize },
}
