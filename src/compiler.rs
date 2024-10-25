//! Compile Scheme code into bytecode
//! to execute!
//! (maybe we should design the bytecode such that
//! it is reversible into Scheme code?)
//!
//! For example:
//!
//! (lambda (x) (+ x x 3))
//!
//! can compile to
//!
//! == (id: 0) Lambda Declaration: arity: 1 ==
//! LOADVAR 0
//! LOADVAR 0
//! LOAD_STATIC 3
//! OP_ADD
//!
//! or
//!
//! (if (>= 3 x -2) ((lambda (b) (+ b x)) 3) ((lambda () (- 3 x))))
//!
//! into
//!
//! == (id: 0) Lambda Declaration: arity: 1, upvars: 1 ==
//! LOADVAR 0
//! LOADUPVAR 0
//! OP_ADD
//! CONT ; this is b/c all of these are in CPS, so there are 2 instructions
//! ; CONT uses the continuation that was passed in with the values on the stack as arguments (tail call)
//! ; while CONTINUE uses the value at the top of the stack as continuation function,
//! ; passing any data below as arguments [arg0, arg1, arg2, ..., argN, func] <- how stack should look
//! == (id: 1) Lambda Declaration: arity: 0, upvars: 1 ==
//! LOAD_STATIC 3
//! LOADUPVAR 0
//! OP_SUB
//! CONT
//! == (id: 2) Lambda Declaration: arity: 0 ==
//! == STATIC DATA ==
//! 0: symbol "x"
//! == CODE ==
//! LOAD_STATIC 3
//! LOADENV 0
//! LOAD_STATIC -2
//! OP_GEQ
//! JUMPIF 3 ; jumps by the argument amount forward if top of stack is false
//! LOAD_STATIC 3
//! LAMBDA 0
//! CONTINUE
//! LAMBDA 1
//! CONTINUE

use core::fmt;
use std::{collections::HashMap, rc::Rc};

use environment::{Environment, EnvironmentPtr};

use crate::{value::ValuePtr, Module};

pub mod bytecode;
pub mod environment;

// for implementation order: we will use
// - Rust macro: to implement define, begin, import and others
// - Scheme macro: first to implement derived forms, and user-defined macros
// - Scheme functions (treewalk): to get an idea of how Scheme code runs
// - Rust functions (treewalk): as the end goal, to be able to integrate external Rust code with
// this system
// - VM functions: make em fast
//
// I say "Scheme macro" w/o any kind of runtime b/c I have realized that the brilliance of Scheme
// macros is that you don't *have* to execute any kind of code in the compiler, they're just
// pattern matching implementations, so this should work across implementation styles as
// there is *no* code to execute...
// Macro output (and thus input) should be:
// - what code to execute
// - what environment to use
//
// refer to https://en.wikipedia.org/wiki/Hygienic_macro#Hygienic_transformation for info on hygiene
// basically
// ```scheme
// (define-syntax my-unless
//   (syntax-rules ()
//     ((_ condition body ...)
//      (if (not condition)
//          (begin body ...)))))
//
// (let ((not (lambda (x) x)))
//   (my-unless #t
//     (display "This should not be printed!")
//     (newline)))
// ```
// shouldn't print anything.
//
// "import" should just be a Rust-side macro, so that the compiler can operate
// with or without it, and you can isolate from import by simply not
// inserting its definition into scope.
// All macros have access to anything that the Scheme macro does in addition to:
// - the current environment of the compiler
// - the World that the compiler was given
//
// also extend with support for "syntax parameters" (https://www.schemeworkshop.org/2011/papers/Barzilay2011.pdf)
// so that we can implement unhygienic stuff.

// Compiler specfies an IR: "frames", which takes something like
// ((lambda (x) (+ x 3)) 4)
// and turns it into:
// APPLICATION
//  APPLICATOR:
//    APPLICATION
//      APPLICATOR: <macro function "lambda">
//      APPLICANDS:
//        - list {"x"}
//        - list {"+", "x", 3}
//      ENV: 0
//   APPLICANDS:
//     - 4
//   ENV: 0
// which means that both expressions are interpreted in the same environment.
// the <macro function> is an Fn (or trait object) that takes the applicands, world, and execution environment,
// and can return 0 or more runtime values while changing the environment.
// macros are run, and then the IR is lowered into the desired backend: treewalk or VM.
// This should work b/c macros should be backend-agnostic, as at no point does arbitrary Scheme code run.
// This must not prevent the design from allowing arbitrary Scheme code though... (in case we want to
// support something like `syntax-case`)

// CPS workout
// the IR is in CPS, so we have to convert:
// (* (/ (+ 1 1 1 1) 1) 3)
// into
// (lambda (k)
//  (+* 1 1 1 1 (lambda (oa)
//      (/* oa 1 (lambda (dvo)
//          (** dvo 3 k)))))
// )
//
// and
// (lambda (a b) (+ a (/ b 3)))
// into
// (lambda (a b k)
//      (/* b 3 (lambda (bd3)
//          (+ a bd3 k))))
//
// (lambda (x) ((lambda (y) (+ x y)) (+ 3 4)))
// into
// (lambda (x k)
//  (+* 3 4 (lambda (tpf)
//      ((lambda (y k) (+* x y k)) tpf k)
//      )))
//

// call-with-current-continuation or call/cc can be represented by:
// (define (call/cc* f k) (f k))

// our compiler is environment aware, so that we can do a little typechecking
// here.
//
// the compiler takes in a base environment, which is definable in Rust, and
// a Source, which holds executable values, and defines a structure that can then be passed to
// the Source in order to execute that Source.

/// An implementation of a transformer or "macro"
pub trait Transformer {}

/// External value
#[derive(Clone)]
pub enum ExternValue {
    Integer(i64),
    String(Box<str>),
    Bool(bool),
    Transformer(Rc<dyn Transformer>),
}
impl fmt::Debug for ExternValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Transformer(trans) => write!(f, "<transformer {trans:p}>"),
        }
    }
}

/// An external, non-gc'ed description on an environment
pub type CompilerEnvironment = HashMap<Box<str>, ExternValue>;

/// Compiler output
#[derive(Clone, Debug)]
pub struct CompilerOutput {}

#[derive(thiserror::Error, Debug)]
#[error("TODO: write errors using codesnake?")]
pub struct CompilerErrors {
    errors: Vec<CompilerError>,
}

/// Describes the values *inside* an environment
enum ValueDescription {
    Number {
        is_integer: bool,
        unsigned: bool,
        within_u32: bool,
    },
    String,
    Transformer {},
}

/// Describes the types and values of an environment
struct EnvironmentDescription {
    values: HashMap<Box<str>, ValueDescription>,
}

/// Trait to query information from environments
trait EnvironmentQuery {
    fn declare(&self) -> EnvironmentDescription;
}

impl EnvironmentQuery for CompilerEnvironment {
    fn declare(&self) -> EnvironmentDescription {
        todo!()
    }
}

impl<'gc> EnvironmentQuery for Environment<'gc> {
    fn declare(&self) -> EnvironmentDescription {
        todo!()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum CompilerError {}

pub struct Compiler;

impl Compiler {
    pub fn compile(
        env: CompilerEnvironment,
        code: Module,
    ) -> Result<CompilerOutput, CompilerErrors> {
        todo!()
    }

    // used for `eval` to compile a value as code to run!
    pub fn compile_eval<'gc>(
        env: EnvironmentPtr<'gc>,
        value: ValuePtr<'gc>,
    ) -> Result<CompilerOutput, CompilerErrors> {
        todo!()
    }
}

// NOTE *every* IR Frame has an implicit continuation parameter
/// An IR Frame
pub struct Lambda {
    // Describes the number of parameters expected (excuding the continuation parameter)
    // can be an exact number of arguments, or a lower bound (for lambdas with a rest parameter)
    parameters: (),
    body: IrBody,
}

pub struct IrBody {
    // this can be an environment var, a lambda function literal, or reference to a frame (or parent frame)'s continuation
    operator: (),
    // Can either reference parameters of a frame, a parent frame, an environment var, a frame (or parent frame)'s continuation
    // or a literal
    arguments: (),
    // Reference to a continutation: can either be a reference to a frame (or parent frame) continuation, or
    // a lambda function literal
    continuation: (),
}
