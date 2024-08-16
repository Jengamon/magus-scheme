use std::{collections::HashMap, sync::LazyLock};

pub use logos::{Lexer, Logos};

fn process_piped_ident(lexer: &mut Lexer<Token>) -> Result<Box<str>, LexerError> {
    let mut built_ident = String::new();

    // Skip the | at the beginning
    let mut chars = lexer.slice().chars().skip(1).peekable();
    while let Some(chr) = chars.next() {
        match chr {
            '\\' => match chars.peek() {
                Some('x' | 'X') => {
                    built_ident.push(read_hex_escape(&mut chars, || {
                        LexerError::MalformedIdent(Box::from(lexer.slice()))
                    })?);
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
                Some(_) | None => Err(LexerError::MalformedIdent(Box::from(lexer.slice())))?,
            },
            // Stop consuming at the ending pipe
            '|' => break,
            c => built_ident.push(c),
        }
    }

    assert!(chars.next().is_none());
    Ok(Box::from(built_ident.as_str()))
}

fn process_named_character(lexer: &mut Lexer<Token>) -> Result<char, LexerError> {
    static NAMED_MAP: LazyLock<HashMap<&str, char>> = LazyLock::new(|| {
        let mut named_map = HashMap::new();
        named_map.insert("alarm", '\x07');
        named_map.insert("backspace", '\x08');
        named_map.insert("delete", '\x7f');
        named_map.insert("escape", '\x1b');
        named_map.insert("newline", '\n');
        named_map.insert("null", '\x00');
        named_map.insert("return", '\r');
        named_map.insert("tab", '\t');
        // Non-standard codes
        named_map.insert("lambda", '\u{03bb}');
        named_map.insert("Lambda", '\u{039b}');
        named_map
    });

    // skip the #\ at the front
    let name = &lexer.slice()[2..];
    NAMED_MAP
        .get(name)
        .copied()
        .ok_or_else(|| LexerError::InvalidCharacterName(Box::from(name)))
}

fn process_hex_character(lexer: &mut Lexer<Token>) -> Result<char, LexerError> {
    let mut value = 0u32;

    // Skip the #\x
    for chr in lexer.slice().chars().skip(3) {
        match chr {
            c @ ('0'..='9' | 'a'..='f' | 'A'..='F') => {
                value = value
                    .checked_mul(16)
                    .ok_or(LexerError::CharacterTooBig)?
                    .checked_add(c.to_digit(16).unwrap())
                    .ok_or(LexerError::CharacterTooBig)?;
            }
            _ => unreachable!(),
        }
    }

    char::from_u32(value).ok_or(LexerError::InvalidCodepoint(value))
}

// reads hex escapes in the form `x[0-9a-fA-F]+` and outputs the corresponding character
fn read_hex_escape<F>(
    iter: &mut std::iter::Peekable<impl Iterator<Item = char>>,
    on_malformed: F,
) -> Result<char, LexerError>
where
    F: Fn() -> LexerError,
{
    // consume the x
    let _ = iter.next();

    let mut char_code = 0u32;
    while let Some(c) = iter.peek() {
        match c {
            ';' => break,
            c @ ('0'..='9' | 'a'..='f' | 'A'..='F') => {
                char_code = char_code
                    .checked_mul(16)
                    .ok_or(LexerError::CharacterTooBig)?
                    .checked_add(c.to_digit(16).unwrap())
                    .ok_or(LexerError::CharacterTooBig)?;
                _ = iter.next();
            }
            _ => Err(on_malformed())?,
        }
    }
    if iter.next() != Some(';') {
        return Err(on_malformed());
    }
    char::from_u32(char_code).ok_or(LexerError::InvalidCodepoint(char_code))
}

fn process_string(lexer: &mut Lexer<Token>) -> Result<Box<str>, LexerError> {
    // Our string syntax is described by /"([^\\"]|\\[abtnr"\\]|\\[ \t]*(\r|\n|\r\n)[ \t]*|\\x[0-9a-fA-f]+;)*"/
    // We use a more permissive version of this on the Logos side, so that errors are neater.

    let mut string = String::new();

    let mut chars = lexer.slice().chars().skip(1).peekable();

    while let Some(c) = chars.next() {
        match c {
            '"' => break,
            '\\' => match chars.peek() {
                Some('a') => {
                    string.push('\x07');
                    _ = chars.next(); // consume
                }
                Some('b') => {
                    string.push('\x08');
                    _ = chars.next(); // consume
                }
                Some('t') => {
                    string.push('\t');
                    _ = chars.next(); // consume
                }
                Some('n') => {
                    string.push('\n');
                    _ = chars.next(); // consume
                }
                Some('r') => {
                    string.push('\r');
                    _ = chars.next(); // consume
                }
                Some('\\') => {
                    string.push('\\');
                    _ = chars.next(); // consume
                }
                Some('"') => {
                    string.push('"');
                    _ = chars.next(); // consume
                }
                Some(' ' | '\t' | '\r' | '\n') => {
                    let _ = chars.next();
                    while let Some(' ' | '\t' | '\r' | '\n') = chars.peek() {
                        _ = chars.next();
                    }
                }
                Some('x' | 'X') => string.push(read_hex_escape(&mut chars, || {
                    LexerError::MalformedString(Box::from(lexer.slice()))
                })?),
                _ => todo!(),
            },
            c => string.push(c),
        }
    }

    assert!(chars.next().is_none());
    Ok(Box::from(string.as_str()))
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
    #[error("invalid character name: {0}")]
    InvalidCharacterName(Box<str>),
    #[error("malformed string: {0}")]
    MalformedString(Box<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum Directive {
    FoldCase,
    NoFoldCase,
}

/// Tokens are lexed from some source, and can arbitrarily borrow from it.
#[derive(Debug, Clone, PartialEq, Hash, Logos)]
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
    #[regex(r#"\|[^|]*\|"#, process_piped_ident)]
    #[token("+", |l| Box::from(l.slice()))]
    #[token("-", |l| Box::from(l.slice()))]
    #[regex(r"[-+][a-zA-Z!$%&*/:<=>?^_~+\-@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"[-+]\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    Identifier(Box<str>),
    #[regex("(?i)#t(rue)?", |_| true)]
    #[regex("(?i)#f(alse)?", |_| false)]
    Boolean(bool),
    #[regex(r"#\\.", callback = |l| l.slice().chars().nth(2).unwrap())] // Regex FTW
    #[regex(r"#\\[a-zA-Z]+", priority = 2, callback =  process_named_character)]
    #[regex(r"(?i)#\\x[0-9a-f]+", callback = process_hex_character)]
    Character(char),
    #[regex(r#""([^\\"]|\\[abntr"\\xX])*""#, process_string)]
    String(Box<str>),
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
        check!(Token::lexer("#t").next() == Some(Ok(Token::Boolean(true))));
        check!(Token::lexer("#true").next() == Some(Ok(Token::Boolean(true))));
        check!(Token::lexer("#f").next() == Some(Ok(Token::Boolean(false))));
        check!(Token::lexer("#false").next() == Some(Ok(Token::Boolean(false))));
    }

    #[test]
    fn test_character() {
        check!(Token::lexer(r"#\a").next() == Some(Ok(Token::Character('a'))));
        check!(Token::lexer(r"#\alarm").next() == Some(Ok(Token::Character('\u{7}'))));
        check!(Token::lexer(r"#\newline").next() == Some(Ok(Token::Character('\n'))));
        check!(Token::lexer(r"#\0").next() == Some(Ok(Token::Character('0'))));
        check!(Token::lexer(r"#\xa").next() == Some(Ok(Token::Character('\n'))));
        check!(Token::lexer(r"#\x03bb").next() == Some(Ok(Token::Character('\u{03bb}'))));
    }

    #[test]
    fn test_string() {
        macro_rules! verify_string {
            ($source:literal as $target:literal) => {
                let mut source = String::new();
                source.push('"');
                source.push_str($source);
                source.push('"');
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::String(bs))) = token);
                check!(bs.as_ref() == $target);
            };
        }

        verify_string!(r#"apple"# as "apple");
        verify_string!(r#"\xea;\n\"\a"# as "\u{ea}\n\"\u{7}");
    }
}
