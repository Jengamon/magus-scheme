// This is the IR format that is compiled into Chunks.
//
// It is convertable to and from StackValues as well as Datum

/*
TODO Goals:
enough information stored to mark errors at the same time, lossless conversion
to/from StackValue.
*/
pub struct Program {}

impl Program {}

/*
TODO Prevent the definition of macros of any primitive form, meaning:
define
lambda
if
set!
(when/if supported) include, include-ci

then provide Rust-side impls for the rest of the standard library (and/or mix it with
Scheme-impls)
*/

// Compiles Programs into Chunks
pub struct Compiler {}
