use crate::treewalk::StackValue;

/// An implementation of a macro
pub trait Macro {}

/// The value type of `syntax-rules`
pub struct SyntaxRules {}

// Have stuff to tell the Treewalk to do the things here
// Fundamental forms: define, lambda, quote, if, define-syntax, let-syntax, letrec-syntax, syntax-rules, set!
// and that's it.
//
// macros are kept in a separate namespace in environments, so they are *not* values. (however, the output of `syntax-rules`
// is a transformer and *is* a value)
// We do have a similar pattern as lambdas, though:
// - macros are *not* Collect (so cannot hold references to the environment)
// - *cannot* suspend (these are not runtime values, but essentially "compile-time" values, so
// they do all their work in one go)
//
// A macro optimization is if the macro considers itself "pure" or not. A "pure" macro might be JIT'ed out into
// the instructions that result from calling the macro.
/// Instructions that a transformer can perform
#[expect(dead_code)]
pub enum MacroInstruction<'gc> {
    /// Execute code in the current environment, pushing the result to the top of stack
    Evaluate(StackValue<'gc>),
    /// Pops the top of the stack, and `define`s the given name as that value
    Define { name: lasso::Spur },
    /// Pops the top of the stack, and `set!`s the given name as that value
    ///
    /// Will set the error of a scope if the name is not found in it.
    SetBang { name: lasso::Spur },
    /// Will call the function at stack[len - args - 1] with stack[len-args..] as arguments
    CallFunction { args: usize },
}
