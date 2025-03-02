use std::{borrow::Cow, collections::HashSet};

use anyhow::Context;
use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    bytecode::{Bytecode, Constant},
    compiler::{LibraryName, ParseProgram, World},
    environment::StackEnvironment,
    gc_arena::{Gc, RefLock},
    general_parser::GeneralParserError,
    interpreter::{CompilerHandle, Interpreter, NullIncluder, ThreadHandle, ValueHandle},
    library_name, stdlib, ContainsDatum, Fuel, GAstNode, Module, Value,
};
use reedline::{
    FileBackedHistory, Prompt, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus,
    PromptViMode, Reedline, Signal, Validator,
};
use yansi::{Condition, Paint};

mod datum_printer;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to read (use `-` for stdin, and not present to use REPL mode)
    file: Option<String>,
}

fn main() -> anyhow::Result<()> {
    yansi::whenever(Condition::TTY_AND_COLOR);

    let args = Cli::parse();
    if let Some(file) = args.file {
        if file == "-" {
            // Find a good way to display compiled data (using the visitor)
            todo!("read from standard input")
        } else {
            execute_file(file)
        }
    } else {
        repl()
    }
}

struct SchemeValidator;
impl Validator for SchemeValidator {
    fn validate(&self, line: &str) -> reedline::ValidationResult {
        // check if parens are balanced
        let lparen_count = line.chars().filter(|c| *c == '(').count();
        let rparen_count = line.chars().filter(|c| *c == ')').count();

        if lparen_count != rparen_count || line.ends_with(';') {
            reedline::ValidationResult::Incomplete
        } else {
            reedline::ValidationResult::Complete
        }
    }
}

#[derive(Default)]
struct MagusPrompt {
    completed_lines: usize,
    // TODO change color depend on if the last completed line succeeded or failed
}

impl Prompt for MagusPrompt {
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::Owned(format!("[{}] ", self.completed_lines))
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::Borrowed("")
    }

    fn render_prompt_indicator(&self, prompt_mode: reedline::PromptEditMode) -> Cow<str> {
        match prompt_mode {
            PromptEditMode::Default | PromptEditMode::Emacs => Cow::Borrowed("> "),
            PromptEditMode::Vi(vi_mode) => match vi_mode {
                PromptViMode::Insert => Cow::Borrowed(": "),
                PromptViMode::Normal => Cow::Borrowed("> "),
            },
            PromptEditMode::Custom(str) => format!("({str})> ").into(),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        let dots: String = self
            .render_prompt_left()
            .chars()
            .map(|_| '.')
            .chain("  ".chars())
            .collect();
        Cow::Owned(dots)
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
        let prefix = match history_search.status {
            PromptHistorySearchStatus::Passing => "",
            PromptHistorySearchStatus::Failing => "failing ",
        };
        // NOTE: magic strings, given there is logic on how these compose I am not sure if it
        // is worth extracting in to static constant
        Cow::Owned(format!(
            "({}reverse-search: {}) ",
            prefix, history_search.term
        ))
    }
}

/// Compiles a given source into a module
fn compile(source: impl AsRef<str>) -> Result<Module, Vec<GeneralParserError>> {
    // General parse
    let gast = magus::general_parse(source.as_ref());

    if gast.errors().is_empty() {
        Ok(Module::cast(gast.syntax()).unwrap())
    } else {
        Err(gast.into_errors())
    }
}

/// Executes a given module
fn execute(
    source: impl AsRef<str>,
    module: &Module,
    interpreter: &mut Interpreter,
    compiler: &CompilerHandle,
    thread: &ThreadHandle,
    stashed_env: Option<&ValueHandle>,
    world: &World,
) {
    // TODO Return output (either () or the interpreter error)
    // Show what the parser sees
    println!("{:#?}", module.syntax());

    // Print the programs parsable external representation
    for datum in module.datum() {
        println!("{:#}", datum_printer::DisplayDatum(&datum));
    }

    // Run the code in through our compiler to get a chunk,
    // then execute that chunk on a new thread
    let chunk: Result<_, anyhow::Error> =
        interpreter.compiler_context(compiler, |mc, compiler, interner| {
            let programs = ("repl.scm", module).parse_program(mc, interner, false)?;
            Ok(compiler.compile(mc, interner, world, &NullIncluder, programs)?)
        });

    match chunk {
        Ok(chunk) => {
            let mut fuel = Fuel::with(1_000_000);
            interpreter.run(thread, |ctx, arena, interner| {
                let Some(chunk) = arena.get_chunk(&chunk) else {
                    unreachable!()
                };
                // TODO Make an actual debugger view?
                println!("==CONSTANTS TABLE==");
                for (idx, constant) in chunk.constants.iter().enumerate() {
                    println!("{idx}: {constant:?}");
                }
                println!("==END CONSTANTS==");
                // expose what each spur means
                let mut shown = HashSet::new();
                println!("==SYMBOLS REFERENCED==");
                for code in chunk.code.iter() {
                    match code {
                        Bytecode::Reference { symbol } if !shown.contains(symbol) => {
                            shown.insert(*symbol);
                            println!("{} -> `{}`", symbol.into_inner(), interner.resolve(symbol));
                        }
                        Bytecode::Define { symbol } if !shown.contains(symbol) => {
                            shown.insert(*symbol);
                            println!("{} -> `{}`", symbol.into_inner(), interner.resolve(symbol));
                        }
                        Bytecode::SetBang { symbol } if !shown.contains(symbol) => {
                            shown.insert(*symbol);
                            println!("{} -> `{}`", symbol.into_inner(), interner.resolve(symbol));
                        }
                        _ => {}
                    }
                }
                for constant in chunk.constants.iter() {
                    if let Constant::Symbol(symbol) = constant {
                        if !shown.contains(symbol) {
                            println!("{} -> `{}`", symbol.into_inner(), interner.resolve(symbol));
                        }
                    }
                }
                println!("==END SYMBOLS==");
                // nice mnemonic format??
                println!("==CHUNK CODE==");
                for code in chunk.code.iter() {
                    // Use display
                    println!("{code}");
                }
                println!("==END CHUNK==");
                let thread = ctx.thread;
                {
                    let mut thread = thread.borrow_mut(&ctx);
                    thread.include(ctx.mc, chunk, None, true);
                    // TODO Make an actual way to do this properly, and not so shenangian-y
                    // Shenanigans to share an environment
                    let Some(frame_env) = thread.env() else {
                        unreachable!()
                    };
                    if let Some(hnd) = stashed_env {
                        let Some(Value::Environment(env)) =
                            arena.get_value(hnd).map(|vp| *vp.borrow())
                        else {
                            unreachable!();
                        };
                        *frame_env.borrow_mut(&ctx) = *env.borrow();
                        // The shenanigan: id want to keep this private to the magus crate
                        frame_env.borrow_mut(&ctx).reparent(Some(chunk.import_env));
                    }
                    thread.step(ctx, interner, &NullIncluder, &mut fuel);
                    // dbg!(&thread);
                    let sources = [(interner.get_or_intern_static("repl.scm"), source.as_ref())];
                    if let Some(res) = thread.result() {
                        match res {
                            Ok(res) => {
                                for v in res {
                                    println!(
                                        "{}",
                                        v.borrow().resolve_into(interner.clone(), ctx.null_value)
                                    );
                                }
                            }
                            Err(e) => {
                                println!(
                                    "{}: {}",
                                    "THREAD ERROR".red(),
                                    e.display(interner, sources)
                                );
                            }
                        }
                    };
                    thread.reset_error();
                    thread.clear_stack();
                }
            });
        }
        Err(e) => {
            println!("{}: {e}", "COMPILE ERROR".red());
        }
    }
}

fn execute_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
    let path = path.as_ref();
    let source = std::fs::read_to_string(path).context("failed to read input file")?;

    match compile(&source) {
        Ok(module) => {
            let mut interpreter = Interpreter::default();
            let compiler = interpreter.new_compiler();
            let thread = interpreter.new_empty_thread();
            let world = World::default();
            execute(
                source,
                &module,
                &mut interpreter,
                &compiler,
                &thread,
                None,
                &world,
            );
        }
        Err(errors) => {
            let idx = LineIndex::new(&source);
            let blocks = errors.iter().flat_map(|err| {
                Block::new(
                    &idx,
                    [Label::new(err.span())
                        .with_text(err.to_string())
                        .with_style(|s| s.red().to_string())],
                )
            });

            for block in blocks.map(|blk| blk.map_code(|c| CodeWidth::new(c, c.len()))) {
                println!("{}[{path:?}]", block.prologue());
                print!("{block}");
                println!("{}", block.epilogue());
            }
        }
    }
    Ok(())
}

fn repl() -> anyhow::Result<()> {
    let mut readline = Reedline::create()
        .with_history(Box::new(
            FileBackedHistory::new(200).expect("failed to configure history file"),
        ))
        .with_validator(Box::new(SchemeValidator));
    let mut prompt = MagusPrompt::default();

    // compiler setup
    let mut interpreter = Interpreter::default();
    let compiler = interpreter.new_compiler();
    let thread = interpreter.new_empty_thread();
    let world = {
        let mut world = World::default();
        world
            .insert(
                LibraryName::from_iter(library_name!(interpreter.interner_mut() => scheme base)),
                stdlib::base::Base,
            )
            .expect("failed to define scheme base module");
        world
    };
    // import (scheme base)
    interpreter.enter(|mc, arena, interner| {
        let Some(compiler) = arena.compiler_mut(&compiler) else {
            unreachable!()
        };
        let import_set = magus::compiler::ImportSet::Name(LibraryName::from_iter(
            library_name!(interner => scheme base),
        ));
        compiler
            .import(mc, interner, &world, &import_set, false)
            .unwrap();
    });
    let stashed_env = interpreter.try_run(&thread, |ctx, arena, _| {
        // Create a shared environment between prompts (excluding macros for now)
        arena.stash_value(
            Value::Environment(Gc::new(
                ctx.mc,
                RefLock::new(StackEnvironment::new(ctx.mc, None)),
            ))
            .into_ptr(&ctx),
        )
    });

    loop {
        match readline.read_line(&prompt) {
            Ok(Signal::Success(input)) => {
                if input.is_empty() {
                    continue;
                }

                let src = input.as_str();

                // General parse
                match compile(src) {
                    Ok(module) => {
                        // consider this line successfully executed
                        prompt.completed_lines += 1;

                        execute(
                            src,
                            &module,
                            &mut interpreter,
                            &compiler,
                            &thread,
                            Some(&stashed_env),
                            &world,
                        );
                    }
                    Err(errors) => {
                        let idx = LineIndex::new(src);
                        let blocks = errors.iter().flat_map(|err| {
                            Block::new(
                                &idx,
                                [Label::new(err.span())
                                    .with_text(err.to_string())
                                    .with_style(|s| s.red().to_string())],
                            )
                        });

                        for block in blocks.map(|blk| blk.map_code(|c| CodeWidth::new(c, c.len())))
                        {
                            println!("{}[repl.scm]", block.prologue());
                            print!("{block}");
                            println!("{}", block.epilogue());
                        }
                    }
                }
            }
            Ok(Signal::CtrlC) | Ok(Signal::CtrlD) => break,
            Err(e) => {
                panic!("{e}");
            }
        }
    }

    Ok(())
}
