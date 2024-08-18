use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    general_parser::{GAstToken, MagusSyntaxElement, Symbol, SyntaxKind},
    lexer::Token,
};
use rustyline::{history::MemHistory, Config};
use yansi::Paint;

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
            todo!("read from standard input")
        } else {
            todo!("open and read file")
        }
    } else {
        repl()
    }
}

// TODO Make SchemeHelper for all the REPL goodies
fn repl() -> anyhow::Result<()> {
    let mut readline =
        rustyline::Editor::<(), _>::with_history(Config::default(), MemHistory::new())?;

    while let Ok(input) = readline.readline(">> ") {
        let src = input.as_str();

        // General parse
        let gast = magus::general_parser::general_parse(&input);

        if let Some(ft) = gast
            .syntax()
            .children_with_tokens()
            .filter_map(|elem| match elem {
                // idents are within DATUM nodes, so...
                MagusSyntaxElement::Token(_) => None,
                MagusSyntaxElement::Node(n) => {
                    if n.kind() == SyntaxKind::DATUM {
                        Some(n)
                    } else {
                        None
                    }
                }
            })
            .find_map(|node| {
                node.children_with_tokens().find_map(|elem| match elem {
                    MagusSyntaxElement::Token(t) => Symbol::cast(t),
                    _ => None,
                })
            })
        {
            println!("First identifier of module: {:?}", ft.identifier(false));
        }

        // Show what the parser sees
        println!("{:#?}", gast.syntax());
        let idx = LineIndex::new(src);

        let block = (!gast.errors().is_empty())
            .then_some(gast.errors())
            .and_then(|errors| {
                Block::new(
                    &idx,
                    errors.iter().map(|err| {
                        Label::new(err.span())
                            .with_text(err.to_string())
                            .with_style(|s| s.red().to_string())
                    }),
                )
            });

        if let Some(block) = block.map(|blk| blk.map_code(|c| CodeWidth::new(c, c.len()))) {
            println!("{}[repl.scm]", block.prologue());
            print!("{block}");
            println!("{}", block.epilogue());
        }

        let tokens = Token::lexer(src);

        for (tok, span) in tokens {
            match tok {
                Ok(Token::Syntax(syntax)) => {
                    println!("[{span:?}] {}", format!("{syntax:?}").cyan())
                }
                Ok(Token::NestedComment(nc)) => println!("[{span:?}] {nc:?}"),
                Err(err) => println!("[{span:?}] {}", err.to_string().red()),
            }
        }

        readline.add_history_entry(input)?;
    }

    Ok(())
}
