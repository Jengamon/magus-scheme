use std::borrow::Cow;

use anyhow::Context;
use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{general_parser::GeneralParserError, ContainsDatum, GAstNode, Module};
use reedline::{
    FileBackedHistory, Prompt, PromptEditMode, PromptHistorySearch, PromptHistorySearchStatus,
    PromptViMode, Reedline, Signal, Validator,
};
use yansi::Paint;

mod datum_printer;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to read (use `-` for stdin, and not present to use REPL mode)
    file: Option<String>,
}

fn main() -> anyhow::Result<()> {
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

        if lparen_count != rparen_count {
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
fn execute(module: &Module) {
    // TODO Return output (either () or the interpreter error)
    // Show what the parser sees
    println!("{:#?}", module.syntax());

    // Print the programs parsable external representation
    for datum in module.datum() {
        println!("{:#}", datum_printer::DisplayDatum(&datum));
    }
}

fn execute_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
    let path = path.as_ref();
    let source = std::fs::read_to_string(path).context("failed to read input file")?;

    match compile(&source) {
        Ok(module) => {
            execute(&module);
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

    loop {
        match readline.read_line(&prompt) {
            Ok(Signal::Success(input)) => {
                if input.is_empty() {
                    break;
                }

                let src = input.as_str();

                // General parse
                match compile(src) {
                    Ok(module) => {
                        // consider this line successfully executed
                        prompt.completed_lines += 1;

                        execute(&module);
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
