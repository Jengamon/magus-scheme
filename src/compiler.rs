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
//! == (id: 1) Lambda Declaration: arity: 0, upvars: 1 ==
//! LOAD_STATIC 3
//! LOADUPVAR 0
//! OP_SUB
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
//! CALL
//! LAMBDA 1
//! CALL

pub mod bytecode;
pub mod environment;
