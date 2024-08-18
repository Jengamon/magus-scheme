use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::lexer::Token;
use rustyline::{history::MemHistory, Config};
use yansi::Paint;

// TODO Make SchemeHelper for all the REPL goodies

fn main() -> anyhow::Result<()> {
    let mut readline =
        rustyline::Editor::<(), _>::with_history(Config::default(), MemHistory::new())?;

    while let Ok(input) = readline.readline(">> ") {
        let src = input.as_str();

        // General parse
        let gast = magus::general_parser::general_parse(&input);

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
