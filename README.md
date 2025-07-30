# Magus - an R7RS impl (originally for Magicflute)

Build status: [![builds.sr.ht status](https://builds.sr.ht/~jangermad/magus/commits/impl-redo.svg)](https://builds.sr.ht/~jangermad/magus/commits/impl-redo?)

R7RS Compatibility: [COMPATIBILITY.md](./COMPATIBILITY.md)

## Project Goals

In the vein of [piccolo](https://github.com/kyren/piccolo), we have pretty much the same goals in the same priority order (descending):
  - Be an arguably working, useful Scheme interpreter.
  - Be an easy way to *confidently* sandbox untrusted Scheme scripts.
  - Be resilient against DoS from untrusted scripts (scripts should not be able to cause the interpreter to panic or use an unbounded amount of memory and should be guaranteed to return control to the caller in some bounded amount of time).
  - Be an easy way to bind Rust APIs to Scheme safely, with a bindings system that is resilient against weirdness and edge cases, and with user types that can safely participate in runtime garbage collection. (this is more or less handled by [gc-arena](https://github.com/kyren/gc-arena), due to
    Scheme types being opaque pointers)
  - Be pragmatically compatible with Scheme as described by R7RS.
  - Don't be obnoxiously slow. (Scheme is a slow language, so we can be more lenient than Piccolo/Lua, but not *too* lenient.)

## Architecture

- Lexer
- General Parser
- Compiler
- VM (Thread)

<!-- TODO write about each layer -->

## Error reporting

TODO move repl from codesnake to codespan-reporting

## Numbers

Numbers are of particular interest to Schemers, and I'm happy to say that we support *lexing*
all forms of numbers (and with how we store them, the Scheme `ieee-float` feature). However,
we currently do not have a runtime that supports numbers beyond exact integers (for my purposes,
this is fine).

TODO (future) Work on using BigIntegers (Number) in the Frontend. We only have to actually *do* it, as `SyntaxToken` is not Copy!
The only real blocker is the need to split out and reorganize number lexing code for maintainability and avoid the "one function to
rule them all" pattern we currently have (following the pattern of having many different regexes, each targeted at a slightly different valid pattern).
The goal is to get most of everything else working, then circle back to this lexer part (desirably after complex numbers have landed more support in the runtime).

**UPDATE** (2025-03-01): We don't actually support polar numbers. We should, eventually.
**UPDATE** (2025-06-24): Polar lexer fix is planned for when we actually review number lexing and *hopefully* make it less of a cluster-bomb (hindsight:
using the whole *one function* pattern is not all that tenable esp. for post-authoring modification, we should take advantage of the fact that we technically pass a closure),
*and*
complex numbers start becoming more than just a lexer feature and the standard library starts handling them more.

## Example

See [readme.rs](./examples/readme.rs) for a very simple example of running a script.

## Optimization Work (TO BE REMOVED/SEPARATED)

Optimization work is cool (and expected but unexpected).
An example of a function that went from being 23s to do 1/3 of this work (in *release* mode), to 200ms in *debug* mode.
(Release mode is something like *30-ish ms*). (on my MBP M3)

<!-- i wish the URL was adjusted -->
<img src="./magus/blob/impl-redo/list-opt.gif"/>

## IMPL NOTES (TO BE REMOVED/SEPARATED)

### Hygienic macros

Hygienic macros have the property that "they mean the same thing everywhere".
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

### Threat Model

I've been working with this mindset, but I want to record it here for posterity. The basic
gist is that Rust code is fully* trusted to know what it is doing, while Scheme code should not:
- be able to run forever, unless Rust code explicitly allows it to do so
- cause a panic (rn upvalue miscompilation causes a todo! to trigger, but this should
  be turned into an SchemeErrorKind as soon as possible) (only reason it hasn't yet is that
  at this point, triggering the todo! indicates a serious problem with upvalue usage)

There are 3 interaction points with Rust:
- Syntax, defining ways to compile code forms (ProgramPtr) into code for
  the VM (Bytecode / Chunk). These are fully trusted to do their job (except for
  infinite recursion, which is handled by the Compiler).
- NativeLambda, a way to get code that Scheme code can call into. These are trusted
  in their operation, but the world attempts to be sanitized by providing (generally)
  immutable/shared access to internals (It *can* mutate things, but we try to stop it
  by doing things like freezing the env pointer it accesses, or providing a shared ref to Thread,
  and encouraging that instead of using the ThreadPtr in the Context it has access to).
  Less trusted than Syntax, but still heavily trusted.
- external API, like any `&mut` method on Thread, and `Compiler::compile`. It should
  not be possible with a combination of API calls to cause a panic. We don't prevent broken
  execution with misused APIs, but panics should generally be considered errors that need fixing.

I will/should work on documentation once `syntax-rules` works (and thus the Hard Parts: libraries,
continuations, and Scheme macros all work) and some stable base to work on is created.

