# Magus - an R7RS-ish impl for Magicflute

Build status: [![builds.sr.ht status](https://builds.sr.ht/~jangermad/magus/commits/impl-redo.svg)](https://builds.sr.ht/~jangermad/magus/commits/impl-redo?)

So we are targeting R5RS and R7RS as we please. In general:
- R7RS has case-sensitivity. since we already wrote the code, might as well use it...
- R7RS has a module system + `include`. we don't need that, so we won't
- R7RS has a library system. this is nopen't we are not doing that.
- R5RS has no error handling. we want error handling.

So while it is correct to say we are *some* form of Scheme, we aren't going to be
hardlining any specific implementation. (But I do want to add the Racket syntax-parameter
extension, and also look around for any *non-syntactical* changes that would make life easier.)
(I'd want to mull more for any syntax changes, b/c there *is* value in saying that the lexer/parser
is *just* R7RS)

### Frontend

There are 2 parts of the frontend:
- Lexer
- General Parser

The formal syntax of R5RS is encoded in the lexer and general parser, where the lexer handles things that can be
recognized by regular expressions (identifiers, numbers, strings, etc.) while the general parser handles things
that can't be (nested comments, datum, datum comments, bytevectors, vectors) (anything that requires pairing).
(we are case-sensitive as influenced by R7RS)

This forms the GAST which is just a representation of what a given file *literally* contains. (these should map to a backing CST from `rowan`).

No macro interpretation is done at this point (and special forms are handled by this layer).

A World defines *all* modules that can possibly exist. A script is only allowed to import libraries defined by modules from its World.
(In fact, any value is able to be shared between runtimes if they share the same World. This is to
make implementing Scheme macros easier \[I hope...])

### Runtime

The runtime is responsible for reading and executing on a World's GAst.

It consists of 2 parts:
- Compiler
- VM/Treewalk

- [x] compiler can interpret Rust-side macros
- [ ] compiler can interpret Scheme-side macros
- [ ] compiler can produce VM bytecode
- [ ] vm can interpret code

Scheme-defined macros are have their results executed in the environment where the macros was originally declared,
so the result of the compiler must have a way to specifying different environments for code to be executed in.

#### Compiler
It is the job of the compiler to:
- interpret macros
- produce runnable code from a world and source

It takes in a World, and a filename, and returns code that can be ran on an interpreter.

#### VM/Treewalk

VM is an interpreter that relies on the compiler converting code into bytecode before it can execute.
Treewalk is one that only needs macros to be interpreted by the compiler to get the resulting code.

## Numbers

Numbers are of particular interest to Schemers, and I'm happy to say that we support *lexing*
all forms of numbers (and with how we store them, the Scheme `ieee-float` feature). However,
we currently do not have a runtime that supports numbers beyond exact integers (for my purposes,
this is fine).

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

Make code prettier. As a prerequisite for this, spin out the lexer and general parser together into their own crate.
The below operations should be provided by a single executable (as they are pretty similar in goal).

### Formatter

Soooo to make code look nice we should do this, taking advantage of the CST we use.
We can model it after the one currently in REPL (which is modelled after https://justinethier.github.io/cyclone/docs/Scheme-code-conventions.html).

The idea is to have this and then have the repl use this to display code instead (so preserving comments)

### Linter

Due to technicallities of our stack, we try to error where we can, but invalid-*looking* code is allow through like:
```scheme
(define x (list #xah))
```
b/c it's *technically* not invalid. We should lint code like this (taking advantage of CST~) (the formatter simply makes
the code follow these lint rules) (essentially linter is suggestions, while formatter
changes code to match suggestions)
