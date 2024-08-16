pub use logos::{Lexer, Logos};

fn process_piped_ident(lexer: &mut Lexer<Token>) -> Result<Box<str>, LexerError> {
    let mut built_ident = String::new();

    // Skip the | at the beginning
    let mut chars = lexer.slice().chars().skip(1).peekable();
    while let Some(chr) = chars.next() {
        match chr {
            '\\' => match chars.peek() {
                Some('x' | 'X') => {
                    let mut char_code = 0u32;
                    let _ = chars.next(); // consume 'x'
                    while let Some(c) = chars.peek() {
                        match c {
                            ';' => break,
                            c @ ('0'..='9' | 'a'..='f' | 'A'..='F') => {
                                char_code = char_code
                                    .checked_mul(16)
                                    .ok_or(LexerError::CharacterTooBig)?
                                    .checked_add(c.to_digit(16).unwrap())
                                    .ok_or(LexerError::CharacterTooBig)?;
                                _ = chars.next();
                            }
                            _ => Err(LexerError::MalformedIdent(Box::from(lexer.slice())))?,
                        }
                    }
                    if chars.next() != Some(';') {
                        return Err(LexerError::MalformedIdent(Box::from(lexer.slice())));
                    }
                    built_ident.push(
                        char::from_u32(char_code).ok_or(LexerError::InvalidCodepoint(char_code))?,
                    );
                }
                Some('a') => {
                    built_ident.push('\x07');
                    _ = chars.next(); // consume
                }
                Some('b') => {
                    built_ident.push('\x08');
                    _ = chars.next(); // consume
                }
                Some('t') => {
                    built_ident.push('\t');
                    _ = chars.next(); // consume
                }
                Some('n') => {
                    built_ident.push('\n');
                    _ = chars.next(); // consume
                }
                Some('r') => {
                    built_ident.push('\r');
                    _ = chars.next(); // consume
                }
                Some(_) | None => unreachable!(
                    "Logos lexer should validate the general shape of piped identfiers"
                ),
            },
            // Stop consuming at the ending pipe
            '|' => break,
            c => built_ident.push(c),
        }
    }

    assert!(chars.next().is_none());
    Ok(Box::from(built_ident.as_str()))
}

#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    #[default]
    #[error("invalid token encountered")]
    Invalid,
    #[error("malformed identifier: {0}")]
    MalformedIdent(Box<str>),
    #[error("character literal too big")]
    CharacterTooBig,
    #[error("invalid Unicode codepoint: {0}")]
    InvalidCodepoint(u32),
    #[error("invalid directive: {0}")]
    InvalidDirective(Box<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Directive {
    FoldCase,
    NoFoldCase,
}

/// Tokens are lexed from some source, and can arbitrarily borrow from it.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Logos)]
#[logos(error = LexerError)]
pub enum Token {
    #[regex("[ \t]+")]
    IntralineWhitespace,
    #[token("\n")]
    #[token("\r\n")]
    #[token("\r")]
    LineEnding,
    #[regex(r";[^\n]*")]
    Comment,
    // We kinda cheat for nested comments, in that we simply *ignore everything* until
    // we are out of the nested comment. Same for datum comments, because these are not regex-friendly
    #[token("#|")]
    StartNestedComment,
    #[token("|#")]
    EndNestedComment,
    #[token("#;")]
    DatumComment,
    #[regex("(?i)#!fold-case", |_| Directive::FoldCase)]
    #[regex("(?i)#!no-fold-case", |_| Directive::NoFoldCase)]
    #[regex(r"(?i)#![a-z0-9\-]+", |l| Err(LexerError::InvalidDirective(Box::from(&l.slice()[2..]))))]
    Directive(Directive),

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("'")]
    Quote,
    #[token("`")]
    Quasiquote,
    #[token(",")]
    Unquote,
    #[token(",@")]
    UnquoteSplicing,
    #[token("#(")]
    StartVector,
    #[regex(r"(?i)#u8\(")]
    StartBytevector,
    #[regex(r#"[a-zA-Z!$%&*/:<=>?^_~][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*"#, |l| Box::from(l.slice()))]
    #[regex(r#"\|([^|\\]|(?i:\\x[0-9a-f]+;?)|\\[abtnr|])*\|"#, process_piped_ident)]
    #[regex(r#"\|([^|\\]|(?i:\\x[0-9a-f]+;?)|\\[ABNTR])*\|"#, priority = 2, callback = |l| Err(LexerError::MalformedIdent(Box::from(l.slice()))))]
    #[token("+", |l| Box::from(l.slice()))]
    #[token("-", |l| Box::from(l.slice()))]
    #[regex(r"[-+][a-zA-Z!$%&*/:<=>?^_~+\-@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"[-+]\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    Identifier(Box<str>),
    #[regex("(?i)#t(rue)?", |_| true)]
    #[regex("(?i)#f(alse)?", |_| false)]
    Boolean(bool),
}

#[cfg(test)]
mod tests {
    use super::{Logos, Token};
    use assert2::{assert, check, let_assert};

    #[test]
    fn test_identifier_and_piped_identifier() {
        let id1 = r"Hello";
        let id2 = r"|H\x65;llo|";

        assert!(Token::lexer(id1).next() == Token::lexer(id2).next())
    }

    #[test]
    fn identifier_checklist() {
        macro_rules! test_valid {
            ($source:literal) => {{
                let mut lexer = Token::lexer($source);
                let token = lexer.next();
                // This contains a boxed slice, so not directly useful in patterns (currently)
                let_assert!(Some(Ok(Token::Identifier(_))) = token);
                assert!(lexer.slice() == $source);
            }};

            ($source:literal as $target:literal) => {{
                let mut lexer = Token::lexer($source);
                let token = lexer.next();
                let_assert!(Some(Ok(Token::Identifier(s))) = token);
                // handles indentifiers whose meaning is not directly related to how it is written
                assert!(s.as_ref() == $target);
            }};
        }

        // Taken from the reference as examples of valid identifiers
        test_valid!("...");
        test_valid!("<=?");
        test_valid!("+");
        test_valid!("+soup+");
        test_valid!("->string");
        test_valid!("a34kTMNs");
        test_valid!("lambda");
        test_valid!("q");
        test_valid!("V17a");
        test_valid!("|two words|" as "two words");
        test_valid!(r"|two\x20;words|" as "two words");
        test_valid!("the-word-recursion-has-many-meanings");
    }

    #[test]
    fn syntax_insensitivity() {
        // For the most part, scheme is syntax insensitive (except for the rules <letter>, <character name>, and <mnemonic escape> and thus
        // anything that uses those).

        check!(Token::lexer("#u8(").next() == Token::lexer("#U8(").next());
        check!(Token::lexer("#!fold-case").next() == Token::lexer("#!FOLD-CASE").next());
        check!(Token::lexer("#!no-fold-case").next() == Token::lexer("#!NO-FOLD-CASE").next());
        check!(Token::lexer("#t").next() == Token::lexer("#T").next());
        check!(Token::lexer("#true").next() == Token::lexer("#TrUe").next());
        check!(Token::lexer("#f").next() == Token::lexer("#F").next());
        check!(Token::lexer("#false").next() == Token::lexer("#FaLsE").next());
    }

    #[test]
    fn test_boolean() {
        assert!(Token::lexer("#t").next() == Some(Ok(Token::Boolean(true))));
        assert!(Token::lexer("#true").next() == Some(Ok(Token::Boolean(true))));
        assert!(Token::lexer("#f").next() == Some(Ok(Token::Boolean(false))));
        assert!(Token::lexer("#false").next() == Some(Ok(Token::Boolean(false))));
    }
}
