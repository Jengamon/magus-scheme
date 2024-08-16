use codesnake::{Block, CodeWidth, Label, LineIndex};
use logos::{Logos, Span};
use magus::lexer::{LexerError, Token};
use yansi::Paint;

fn make_block<'a>(
    idx: &'a LineIndex,
    labels: impl IntoIterator<Item = (Span, Result<Token, LexerError>)>,
) -> Option<Block<&'a str, String>> {
    Block::new(
        idx,
        labels.into_iter().map(|(range, tok)| {
            let text = format!("{tok:?}");
            Label::new(range)
                .with_text(if tok.is_ok() {
                    text.green().to_string()
                } else {
                    text.red().to_string()
                })
                .with_style(move |s| match tok {
                    Ok(Token::Identifier(_)) => s.blue().to_string(),
                    Ok(_) => s,
                    Err(_) => s.red().to_string(),
                })
        }),
    )
}

fn main() {
    let src = r"|a\na| a #!A";
    let idx = LineIndex::new(src);
    let tokens = Token::lexer(src);

    let mut blocks = vec![];
    let mut line_labels = vec![];
    for (token, span) in tokens.spanned() {
        match token {
            Ok(Token::LineEnding) => blocks.push(make_block(&idx, line_labels.drain(..))),
            tok => line_labels.push((span.clone(), tok)),
        }
    }

    if !line_labels.is_empty() {
        blocks.push(make_block(&idx, line_labels.drain(..)));
    }

    for block in blocks
        .into_iter()
        .filter_map(|blk| Some(blk?.map_code(|c| CodeWidth::new(c, c.len()))))
    {
        println!("{}{}", block.prologue(), "[main.scm]");
        print!("{block}");
        println!("{}", block.epilogue());
    }
}
