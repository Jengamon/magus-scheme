# Magus - an R7RS-ish impl for Magicflute

Build status: [![builds.sr.ht status](https://builds.sr.ht/~jangermad/magus/commits/impl-redo.svg)](https://builds.sr.ht/~jangermad/magus/commits/impl-redo?)

So we are targeting R5RS and R7RS as we please. In general:
- R7RS has case-sensitivity. since we already wrote the code, might as well use it...
- ~~R7RS has a module system + `include`. we don't need that, so we won't~~ I actually love the module system
- ~~R7RS has a library system. this is nopen't we are not doing that.~~ Nah libs are great.
- R5RS has no error handling. we want error handling.

(So now we are basically R7RS)

So while it is correct to say we are *some* form of Scheme, we aren't going to be
hardlining any specific implementation. (But I do want to add the Racket syntax-parameter
extension, and also look around for any *non-syntactical* changes that would make life easier.)
(I'd want to mull more for any syntax changes, b/c there *is* value in saying that the lexer/parser
is *just* R7RS)

### Frontend

There are 2 parts of the frontend:
- Lexer
- General Parser

The formal syntax of Scheme is encoded in the lexer and general parser, where the lexer handles things that can be
recognized by regular expressions (identifiers, numbers, strings, etc.) while the general parser handles things
that can't be (nested comments, datum, datum comments, bytevectors, vectors) (anything that requires pairing).

(Currently the only number type of the tower that is unsupported by the lexer are polar numbers)

This forms the GAST which is just a representation of what a given file *literally* contains. (these should map to a backing CST from `rowan`).

No macro interpretation is done at this point (and special forms are handled by this layer).

A World defines *all* native modules that can possibly exist.
A script is only allowed to import libraries defined by modules from its World, or defined locally (by `define-library`)
in their script (or include) on the same `Compiler`.

### Runtime
The runtime is responsible for reading and executing on a GAst.

It consists of 2 parts:
- Compiler
- VM

GAst is wrapped with a filename to form a Program.

#### Compiler
It is the job of the compiler to:
- interpret macros
- produce runnable code from a world and source

It takes in a World, Includer, and a Program, and returns code that can be ran on an interpreter thread.

#### VM/Treewalk

VM is an interpreter that relies on the compiler converting code into bytecode before it can execute.

## Numbers

Numbers are of particular interest to Schemers, and I'm happy to say that we support *lexing*
all forms of numbers (and with how we store them, the Scheme `ieee-float` feature). However,
we currently do not have a runtime that supports numbers beyond exact integers (for my purposes,
this is fine).

TODO Work on allowing writing of rationals in a Program
TODO (future) Work on using BigIntegers in the Frontend

**UPDATE** (2025-03-01): We don't actually support polar numbers. We should, eventually.

### IMPL NOTES

## Hygenic macros

~~(why is having hyg*i*ene considered *hyge*nic??? anyways)~~
Hygenic macros have the property that "they mean the same thing everywhere".
To put this in more formal terms, this means that the evaluation of a macro *always*
takes place in the same environment, namely the one it was defined in.
If I were to define in `hygiene.scm`:

```scheme
(define-syntax x! (syntax-rules ()
    ((x! val)
        (set! x val))
))
(define-syntax define-x (syntax-rules ()
    ((define-x val)
        (begin
            (define x val)
            (x! (+ val 1))
            x
        ))
))

;; this should error!
(define-x 6)
;; but if *above* that line, you defined `x`, it would work and the
;; value would be overwritten by (the number you passed in + 1)
```

## Improve UX

Make code prettier. As a prerequisite (?) for this, spin out the lexer and general parser together into their own crate.
The below operations should be provided by a single executable (as they are pretty similar in goal).

### Formatter

Soooo to make code look nice we should do this, taking advantage of the CST we use.
We can model it after the one currently in REPL (which is modelled after https://justinethier.github.io/cyclone/docs/Scheme-code-conventions.html).

The idea is to have this and then have the repl use this to display code instead (so preserving comments)

(Possibly we can just write something for [`topiary`](https://topiary.tweag.io/), which would mean we'd only need the lexer)

### Linter

Due to technicallities of our stack, we try to error where we can, but invalid-*looking* code is allow through like:
```scheme
(define x (list #xah))
```
b/c it's *technically* not invalid. We should lint code like this (taking advantage of CST~) (the formatter simply makes
the code follow these lint rules) (essentially linter is suggestions, while formatter
changes code to match suggestions)

## Scheme Implemented Procedures
To make things easier on me (hopefully), some of a module's functionality can be implemented in Scheme, as native modules
are allowed to overlap in name with a local Scheme module. This means that there can be a difference in functionality between
directly registering a native module with the World, and the method a module might want you to use. In the case
of the Scheme standard library (located in "src/stdlib/base"), it provides a method `register_module` that will properly set up
a compiler to fully define the module.

For modules in this repository, there is the method above, but one can also use the public constant `MODULE_SRC` to get the
code compiled to form the compiler-local Scheme side of a module.

TODO Figure out if this if useful enough to provide an interface that would allow Interpreter to automate this registration step.
(It probably is useful enough, but check for an actual way to implement)

## Threat Model

I've been working with this mindset, but I want to record it here for posterity. The basic
gist is that Rust code is fully* trusted to know what it is doing, while Scheme code should not:
- be able to run forever, unless Rust code explicitly allows it to do so
- cause a panic (rn upvalue miscompilation causes a todo! to trigger, but this should
  be turned into an SchemeErrorKind as soon as possible)

There sre 3 interaction points with Rust:
- Syntax, defining ways to compile code forms (ProgramPtr) into code for
  the VM (Bytecode / Chunk). These are fully trusted to do their job (except for
  infinite recursion, which is handled by the Compiler).
- NativeLambda, a way to get code that Scheme code can call into. These are trusted
  in their operation, but the world attempts to be sanitized by providing (generally)
  immutable/shared access to internals (It *can* mutate things, but we try to stop it
  by doing things like freezeing the env pointer it accesses, or providing a shared ref to Thread,
  and encouraging that instead of using the ThreadPtr in the Context it has access to).
  Less trusted than Syntax, but still heavily trusted.
- external API, like any `&mut` method on Thread, and `Compiler::compile`. It should
  not be possible with a combination of API calls to cause a panic. We don't prevent broken
  execution with misused APIs, but panics should generally be considered errors that need fixing.

I will/should work on documentation once `syntax-rules` works (and thus the Hard Parts: libraries,
continuations, and Scheme macros all work) and some stable base to work on is created.

