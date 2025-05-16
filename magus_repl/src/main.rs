use std::{
    borrow::Cow,
    collections::HashSet,
    sync::mpsc::{channel, Receiver},
    time::Instant,
};

use anyhow::Context;
use clap::{Parser, Subcommand};
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    bytecode::{Bytecode, Constant},
    compiler::{LibraryDefinitionContext, LibraryName, ParseProgram, World},
    environment::StackEnvironment,
    gc_arena::{Gc, RefLock},
    general_parse,
    general_parser::GeneralParserError,
    interpreter::{CompilerHandle, Includer, Interpreter, ThreadHandle, ValueHandle},
    library_name,
    rowan::TextSize,
    runtime::lambda::Lambda,
    stdlib,
    value::ModeWrite,
    ChunkHandle, ContainsDatum, ExternalCompilerContext, Fuel, GAstNode, Module, Value,
};
use reedline::{
    Highlighter, Prompt, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus,
    PromptViMode, Reedline, Signal, SqliteBackedHistory, Validator,
};

mod datum_printer;

const CLI_COMMANDS: &[&str] = &[
    "#gc", "#env", "#collect", "#quit", "#q", "#help", "#?", "#time",
];
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    mode: Option<Mode>,
    /// Read code case-insensitively (by default)
    #[arg(long, short = 'i')]
    case_insensitive: bool,
}

#[derive(Subcommand, Debug)]
pub enum Mode {
    /// Read input from a file instead of starting a REPL
    File {
        /// Input file to read
        name: String,
        /// Compile the code, but do not run the result
        #[arg(long, short = 'c')]
        compile: bool,
    },
}

struct PwdIncluder;
impl Includer for PwdIncluder {
    fn include(&self, filename: &str) -> anyhow::Result<Box<str>> {
        Ok(std::fs::read_to_string(filename)?.into_boxed_str())
    }
}

fn main() -> anyhow::Result<()> {
    use yansi::Condition;

    yansi::whenever(Condition::TTY_AND_COLOR);

    let args = Cli::parse();
    if let Some(Mode::File { name, compile }) = args.mode {
        if name == "-" {
            // Find a good way to display compiled data (using the visitor)
            todo!("read from standard input")
        } else if compile {
            compile_file(name, args.case_insensitive)
        } else {
            execute_file(name, args.case_insensitive)
        }
    } else {
        repl(args.case_insensitive)
    }
}

struct MagusHighlightor;
impl Highlighter for MagusHighlightor {
    fn highlight(&self, line: &str, cursor: usize) -> reedline::StyledText {
        use magus::{Abbreviation, DatumComment, NestedComment, SyntaxKind};
        use nu_ansi_term::{Color, Style};

        // For now, highlight *around* the cursor (ignore it)
        let parse = general_parse(line);
        let mut styled_buf = reedline::StyledText::new();
        styled_buf.push((Style::new(), line.to_string()));

        // Highlight cli commands
        if CLI_COMMANDS.contains(&line.to_lowercase().as_str()) {
            // Highlight as cli commands, and return!
            styled_buf.style_range(0, line.len(), Style::new().fg(Color::Green));
            return styled_buf;
        }

        // Highlight all identifiers in blue
        for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if ele.kind() == SyntaxKind::SYMBOL {
                ele.into_token()
            } else {
                None
            }
        }) {
            let identifier_style = Style::new().bold().fg(Color::Blue);
            // Just underline identifiers that end with "!" (but arent just "!") as those usually indicate mutation
            let mutation_style = Style::new().bold().underline().fg(Color::Blue);
            let keyword_style = Style::new().fg(Color::Green);
            let span = tok.text_range();
            // There are only 2 keywords (and they stop meaning a keyword once a non-keyword is encountered)
            const KEYWORDS: &[&str] = &[
                "import",
                "define-library",
                "export",
                "include",
                "include-ci",
                "include-library-declarations",
                "cond-expand",
                "begin",
                "only",
                "rename",
                "prefix",
                "except",
            ];
            let is_keyword = KEYWORDS.contains(&tok.text());
            styled_buf.style_range(
                span.start().into(),
                span.end().into(),
                if is_keyword {
                    keyword_style
                } else if tok.text() != "!" && tok.text().ends_with('!') {
                    mutation_style
                } else {
                    identifier_style
                },
            );
        }
        // Highlight all triggers and labels in purple (labels are dim purple)
        for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if matches!(ele.kind(), SyntaxKind::DLABEL | SyntaxKind::DTRIGGER) {
                ele.into_token()
            } else {
                None
            }
        }) {
            let number_style = Style::new().fg(Color::LightPurple);
            let is_label = tok.kind() == SyntaxKind::DLABEL;
            let span = tok.text_range();
            styled_buf.style_range(
                span.start().into(),
                span.end().into(),
                if is_label {
                    number_style.dimmed()
                } else {
                    number_style
                },
            );
        }
        // Highlight all numbers, bools, and characters in dim yellow
        for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if matches!(
                ele.kind(),
                SyntaxKind::NUMBER | SyntaxKind::CHARACTER | SyntaxKind::BOOLEAN
            ) {
                ele.into_token()
            } else {
                None
            }
        }) {
            let number_style = Style::new().fg(Color::Yellow);
            let span = tok.text_range();
            styled_buf.style_range(span.start().into(), span.end().into(), number_style);
        }
        // Highlight all strings in cyan
        for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if matches!(ele.kind(), SyntaxKind::STRING) {
                ele.into_token()
            } else {
                None
            }
        }) {
            let number_style = Style::new().fg(Color::Cyan);
            let span = tok.text_range();
            styled_buf.style_range(span.start().into(), span.end().into(), number_style);
        }
        // Highlight all abbreviated in bold red
        for abbrev in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if ele.kind() == SyntaxKind::ABBREV {
                Abbreviation::cast(ele.into_node()?)
            } else {
                None
            }
        }) {
            let abbrev_style = Style::new().fg(Color::Red);
            let span = abbrev.syntax().text_range();
            styled_buf.style_range(span.start().into(), span.end().into(), abbrev_style);
        }
        // Highlight all directives in dim red
        for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
            if ele.kind() == SyntaxKind::DIRECTIVE {
                ele.into_token()
            } else {
                None
            }
        }) {
            let directive_style = Style::new().dimmed().fg(Color::Red);
            let span = tok.text_range();
            styled_buf.style_range(span.start().into(), span.end().into(), directive_style);
        }

        // Highlight unbalanced parentheses
        {
            let mut parenthesis_stack = Vec::new();
            let unbalanced_style = Color::Red.reverse();
            let highlight_style = Color::LightCyan.reverse();
            for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
                if matches!(
                    ele.kind(),
                    SyntaxKind::LPAREN
                        | SyntaxKind::RPAREN
                        | SyntaxKind::START_BYTEVECTOR
                        | SyntaxKind::START_VECTOR
                ) {
                    ele.into_token()
                } else {
                    None
                }
            }) {
                let span = tok.text_range();
                if matches!(
                    tok.kind(),
                    SyntaxKind::LPAREN | SyntaxKind::START_BYTEVECTOR | SyntaxKind::START_VECTOR
                ) {
                    // lparen push their span to stack
                    parenthesis_stack.push(span);
                } else {
                    // rparens pop a span (or highlight if they failed)
                    if let Some(lpspan) = parenthesis_stack.pop() {
                        let cursor = TextSize::new(cursor as u32);
                        let maybe_highlight_span = if lpspan.contains(cursor) {
                            // highlight the span that doesn't contain the cursor
                            Some(span)
                        } else if span.contains(cursor) {
                            Some(lpspan)
                        } else {
                            None
                        };

                        if let Some(highlight_span) = maybe_highlight_span {
                            styled_buf.style_range(
                                highlight_span.start().into(),
                                highlight_span.end().into(),
                                highlight_style,
                            );
                        }
                    } else {
                        styled_buf.style_range(
                            span.start().into(),
                            span.end().into(),
                            unbalanced_style,
                        );
                    }
                }
            }

            // Highlight the unbalanced left parens now
            for span in parenthesis_stack {
                styled_buf.style_range(span.start().into(), span.end().into(), unbalanced_style);
            }
        }

        // Handle comments
        {
            let comment_style = Style::new().fg(Color::DarkGray);
            // one-line
            for tok in parse.syntax().descendants_with_tokens().filter_map(|ele| {
                if ele.kind() == SyntaxKind::OLCOMMENT {
                    ele.into_token()
                } else {
                    None
                }
            }) {
                let span = tok.text_range();
                styled_buf.style_range(span.start().into(), span.end().into(), comment_style);
            }
            // Datum comment
            for dc in parse.syntax().descendants_with_tokens().filter_map(|ele| {
                if ele.kind() == SyntaxKind::DCOMMENT {
                    DatumComment::cast(ele.into_node()?)
                } else {
                    None
                }
            }) {
                let span = dc.syntax().text_range();
                styled_buf.style_range(span.start().into(), span.end().into(), comment_style);
            }
            // Nested comment
            for nc in parse.syntax().descendants_with_tokens().filter_map(|ele| {
                if ele.kind() == SyntaxKind::NCOMMENT {
                    NestedComment::cast(ele.into_node()?)
                } else {
                    None
                }
            }) {
                let span = nc.syntax().text_range();
                styled_buf.style_range(span.start().into(), span.end().into(), comment_style);
            }
        }
        styled_buf
    }
}

struct SchemeValidator;
impl Validator for SchemeValidator {
    fn validate(&self, line: &str) -> reedline::ValidationResult {
        use magus::SyntaxKind;

        // run *just* the general parser, and make sure stuff is balanced
        let parse = general_parse(line);
        // check if parens (and #|) are balanced
        let lparen_count = parse
            .syntax()
            .descendants_with_tokens()
            .filter(|ele| {
                matches!(
                    ele.kind(),
                    SyntaxKind::LPAREN | SyntaxKind::START_BYTEVECTOR | SyntaxKind::START_VECTOR
                )
            })
            .count();
        let rparen_count = parse
            .syntax()
            .descendants_with_tokens()
            .filter(|ele| ele.kind() == SyntaxKind::RPAREN)
            .count();
        let snc_count = parse
            .syntax()
            .descendants_with_tokens()
            .filter(|ele| ele.kind() == SyntaxKind::START_NCOMMENT)
            .count();
        let enc_count = parse
            .syntax()
            .descendants_with_tokens()
            .filter(|ele| ele.kind() == SyntaxKind::END_NCOMMENT)
            .count();

        if lparen_count != rparen_count || line.ends_with(';') || snc_count != enc_count {
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

fn additional_features() -> std::sync::Arc<[std::sync::Arc<str>]> {
    std::sync::Arc::from(
        ["repl"]
            .into_iter()
            .map(std::sync::Arc::from)
            .collect::<Vec<_>>(),
    )
}

fn compile_to_chunk(
    case_insensitive: bool,
    module: &Module,
    includer: &dyn Includer,
    interpreter: &mut Interpreter,
    compiler: &CompilerHandle,
    thread: &ThreadHandle,
    world: &World,
) -> anyhow::Result<ChunkHandle> {
    // Run the code in through our compiler to get a chunk,
    // then execute that chunk on a new thread
    interpreter.compiler_context(
        thread,
        compiler,
        |mc, compiler, value_pointers, thread, interner| {
            let additional_features = additional_features();
            let programs = ("repl.scm", module).parse_program(mc, interner, case_insensitive)?;
            let mut ecc = ExternalCompilerContext {
                includer,
                world,
                interner,
            };
            let library_def = LibraryDefinitionContext {
                max_fuel: Some(1_000_000),
                value_pointers,
                additional_features: Some(&additional_features),
                thread,
            };
            Ok(compiler.compile(mc, &mut ecc, &library_def, programs)?)
        },
    )
}

fn chunk_debug(interpreter: &mut Interpreter, chunk: &ChunkHandle) {
    interpreter.enter(|_mc, arena, interner| {
        let chunk = arena.chunk(chunk);
        // TODO Make an actual debugger view?
        println!("==CONSTANTS TABLE==");
        for (idx, constant) in chunk.constants.iter().enumerate() {
            println!("{idx}: {constant:?}");
        }
        println!("==END CONSTANTS==");
        // expose what each spur means
        let mut shown = HashSet::new();
        println!("==SYMBOLS REFERENCED==");
        for code in chunk.code.iter().copied().chain(
            chunk
                .lambdas
                .iter()
                .filter_map(|l| {
                    if let Lambda::Compiled(l) = l {
                        Some(l)
                    } else {
                        None
                    }
                })
                .flat_map(|l| l.chunk().code.iter().copied().collect::<Vec<_>>()),
        ) {
            match code {
                Bytecode::Reference { symbol, .. } if !shown.contains(&symbol) => {
                    shown.insert(symbol);
                    println!("{} -> `{}`", symbol.into_inner(), interner.resolve(&symbol));
                }
                Bytecode::Define { symbol } if !shown.contains(&symbol) => {
                    shown.insert(symbol);
                    println!("{} -> `{}`", symbol.into_inner(), interner.resolve(&symbol));
                }
                Bytecode::SetBang { symbol, .. } if !shown.contains(&symbol) => {
                    shown.insert(symbol);
                    println!("{} -> `{}`", symbol.into_inner(), interner.resolve(&symbol));
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
        println!("==LAMBDAS==");
        for (idx, l) in chunk
            .lambdas
            .iter()
            .filter_map(|l| {
                if let Lambda::Compiled(l) = l {
                    Some(l)
                } else {
                    None
                }
            })
            .enumerate()
        {
            println!("==LAMBDA {idx}==");
            for (idx, code) in l.chunk().code.iter().enumerate() {
                println!("{idx:>3}: {code}")
            }
            println!("==END LAMBDA {idx}==");
        }
        println!("==END LAMBDAS==");
        // nice mnemonic format??
        println!("==CHUNK CODE (upvalues: {})==", chunk.upvalues);
        for (idx, code) in chunk.code.iter().enumerate() {
            // Use display
            println!("{idx:>3}: {code}");
        }
        println!("==END CHUNK==");
    });
}

// TODO Create a ThreadObserver struct that holds a thread handle
// in our main loop

/// Executes a given module
#[expect(clippy::too_many_arguments)]
fn execute(
    source: impl AsRef<str>,
    case_insensitive: bool,
    module: &Module,
    includer: &dyn Includer,
    interpreter: &mut Interpreter,
    compiler: &CompilerHandle,
    thread: &ThreadHandle,
    stashed_env: Option<&ValueHandle>,
    world: &World,
    termination_recv: &Receiver<()>,
) {
    use yansi::Paint;

    // TODO Return output (either () or the interpreter error)
    // Show what the parser sees
    println!("{:#?}", module.syntax());

    // Print the programs parsable external representation
    for datum in module.datum() {
        println!("{:#}", datum_printer::DisplayDatum(&datum));
    }

    // Run the code in through our compiler to get a chunk,
    // then execute that chunk on a new thread
    let chunk: Result<_, anyhow::Error> = compile_to_chunk(
        case_insensitive,
        module,
        includer,
        interpreter,
        compiler,
        thread,
        world,
    );

    match chunk {
        Ok(chunk) => {
            let mut fuel = Fuel::with(1_000);
            chunk_debug(interpreter, &chunk);
            // thread setup
            interpreter.run(thread, |ctx, arena, _interner| {
                let thread = ctx.thread;
                {
                    let chunk = arena.chunk(&chunk);
                    let mut thread = thread.borrow_mut(&ctx);
                    thread.include(ctx.mc, chunk, true);
                    // TODO Make an actual way to do this properly, and not so shenangian-y
                    // Shenanigans to share an environment
                    let Some(frame_env) = thread.env() else {
                        unreachable!()
                    };
                    // FIXME make stashed stack environments b/c Value::Environment
                    // is changing from a stack environment to a collection of import sets
                    // to *actually* support (scheme eval) (which is basically set specifications
                    // of imports, then compiling code in that context, building a chunk)
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
                }
            });
            // execution loop
            while !interpreter.is_finished(thread) {
                let should_continue = interpreter.try_run(thread, |ctx, _arena, interner| {
                    let thread = ctx.thread;
                    {
                        let mut thread = thread.borrow_mut(&ctx);
                        if let Ok(()) = termination_recv.try_recv() {
                            return false;
                        }
                        thread.step(ctx, interner, world, includer, &mut fuel);
                        fuel.refill(1_000, 1_000);
                        true
                    }
                });

                if !should_continue {
                    break;
                }
            }
            // thread coda (get result)
            interpreter.run(thread, |ctx, _, interner| {
                let thread = ctx.thread;
                {
                    let mut thread = thread.borrow_mut(&ctx);
                    // dbg!(&thread);
                    let sources = [(interner.get_or_intern_static("repl.scm"), source.as_ref())];
                    if let Some(res) = thread.result() {
                        match res {
                            Ok(res) => {
                                for v in res {
                                    println!(
                                        "{}",
                                        Value::resolve_into::<_, ModeWrite>(
                                            v,
                                            interner.clone(),
                                            ctx.null_value
                                        )
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
                    thread.reset();
                }
            });
        }
        Err(e) => {
            println!("{}: {e}", "COMPILE ERROR".red());
        }
    }
}

fn repl_stuff() -> anyhow::Result<(Interpreter, World, CompilerHandle, ThreadHandle)> {
    let mut interpreter = Interpreter::default();
    let mut world = World::default();

    let compiler = interpreter.new_compiler();
    let thread = interpreter.new_thread();
    // max_fuel = None is *inadvisable* in any form of production code, b/c it means that if an infinite loop is
    // defined and executed in a library, it will run forever.
    // Rather, pass in a large amount of fuel.

    // The only "annoying" part of this API (hopefully) is having to copy around the
    // `additional_features` to every declaration, but this can demonstrate a simple way to
    // do so (it has to be done b/c a compiler needs to know about additional features)
    let additional_features = additional_features();
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::magus_help::MagusHelp,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::base::Base {
            additional_features: std::sync::Arc::clone(&additional_features),
        },
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::write::Write,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::lazy::Lazy,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::inexact::Inexact,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    // This has to happen *after* registering (scheme base) otherwise it will fail b/c
    // it declares a dependency on (scheme base)! (yay!)
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::cxr::Cxr,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;
    // after (scheme base) and (scheme cxr)!
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        stdlib::srfi::list::Srfi1,
        None,
        |thread, vp| LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: Some(&additional_features),
            thread,
        },
    )?;

    Ok((interpreter, world, compiler, thread))
}

fn compile_file(path: impl AsRef<std::path::Path>, case_insensitive: bool) -> anyhow::Result<()> {
    use yansi::Paint;

    let path = path.as_ref();
    let source = std::fs::read_to_string(path).context("failed to read input file")?;

    match compile(&source) {
        Ok(module) => {
            let (mut interpreter, world, compiler, thread) = repl_stuff()?;
            let chunk = compile_to_chunk(
                case_insensitive,
                &module,
                &PwdIncluder,
                &mut interpreter,
                &compiler,
                &thread,
                &world,
            );
            match chunk {
                Ok(chunk) => {
                    chunk_debug(&mut interpreter, &chunk);
                }
                Err(e) => {
                    println!("{}: {e}", "COMPILE ERROR".red());
                }
            }
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

fn execute_file(path: impl AsRef<std::path::Path>, case_insensitive: bool) -> anyhow::Result<()> {
    use yansi::Paint;

    // fake receiver
    let (_tx, rx) = channel();

    let path = path.as_ref();
    let source = std::fs::read_to_string(path).context("failed to read input file")?;

    match compile(&source) {
        Ok(module) => {
            let (mut interpreter, world, compiler, thread) = repl_stuff()?;
            execute(
                source,
                case_insensitive,
                &module,
                &PwdIncluder,
                &mut interpreter,
                &compiler,
                &thread,
                None,
                &world,
                &rx,
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

fn repl(case_insensitive: bool) -> anyhow::Result<()> {
    use yansi::Paint;

    let mut readline = Reedline::create()
        .with_history(Box::new(
            SqliteBackedHistory::with_file("history.local.db".into(), None, None)
                .expect("failed to configure history file"),
        ))
        .with_validator(Box::new(SchemeValidator))
        .with_highlighter(Box::new(MagusHighlightor));
    let mut prompt = MagusPrompt::default();
    println!("Type `#q` or `#quit` to exit. Type `#help` for more commands.");

    let (tx, rx) = channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })
    .expect("failed to set Ctrl-C handler");

    // compiler setup
    let (mut interpreter, world, compiler, thread) = repl_stuff()?;
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
        // TODO Switch this to using EnvironmentHandle (once supported), as Value::Environment will change meaning
        // to support import sets (and thus its *actual* goal of supporting `eval`)
        arena.stash_value(
            Value::Environment(Gc::new(
                ctx.mc,
                RefLock::new(StackEnvironment::new(ctx.mc, None)),
            ))
            .into_ptr(&ctx),
        )
    });

    let mut exec_time = None;
    const HELP_STRING: &str = "### HELP ###
#?, #help - this help message
#q, #quit - quit repl
#gc - check GC stats
#time - check the time of last execution
#env - (todo) check current root environment
#collect - force GC collection";

    let mut double_ctrl_c = false;
    loop {
        match readline.read_line(&prompt) {
            Ok(Signal::Success(cmd)) if CLI_COMMANDS.contains(&cmd.to_lowercase().as_str()) => {
                double_ctrl_c = false;
                match cmd.to_lowercase().as_str() {
                    "#help" | "#?" => {
                        println!("{}", HELP_STRING)
                    }
                    "#quit" | "#q" => break,
                    "#env" => {
                        eprintln!("TO BE WRITTEN")
                    }
                    "#time" => {
                        if let Some(dur) = exec_time {
                            println!("### Time taken: {} ###", humantime::format_duration(dur));
                        } else {
                            println!("### No code executed ###");
                        }
                    }
                    "#gc" => {
                        let metrics = interpreter.metrics();
                        println!(
                            "### GC metrics ###\n\nPhase: {:?}\nTotal GC allocations: {} bytes (unfreed ptrs: {})\nAllocation debt: {}",
                            interpreter.collection_phase(),
                            metrics.total_gc_allocation(),
                            metrics.total_gc_count(),
                            metrics.allocation_debt(),
                        )
                    }
                    "#collect" => {
                        interpreter.finish_cycle();
                    }
                    _ => {
                        unreachable!("Unsupported command")
                    }
                }
            }
            Ok(Signal::Success(input)) => {
                double_ctrl_c = false;
                if input.is_empty() {
                    continue;
                }

                let src = input.as_str();

                // General parse
                match compile(src) {
                    Ok(module) => {
                        // consider this line successfully executed
                        prompt.completed_lines += 1;

                        // Clear all interrupts
                        let _ = rx.try_iter().count();

                        let start = Instant::now();
                        execute(
                            src,
                            case_insensitive,
                            &module,
                            &PwdIncluder,
                            &mut interpreter,
                            &compiler,
                            &thread,
                            Some(&stashed_env),
                            &world,
                            &rx,
                        );
                        let end = Instant::now();
                        exec_time = Some(end - start);
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
            // 2 Ctrl-Cs in a row will *also* close the program
            Ok(Signal::CtrlC) if double_ctrl_c => {
                break;
            }
            // Ctrl-C only kills the current buffer
            // TODO Make this also interrupt the currently running thread
            Ok(Signal::CtrlC) => {
                double_ctrl_c = true;
                continue;
            }
            Ok(Signal::CtrlD) => break,
            Err(e) => {
                panic!("{e}");
            }
        }
    }

    Ok(())
}
