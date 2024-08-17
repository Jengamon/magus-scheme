use std::{collections::HashMap, sync::LazyLock};

pub use logos::Span;
use logos::{Lexer, Logos};
use regex::Regex;

fn process_piped_ident(lexer: &mut Lexer<Token>) -> Result<Box<str>, LexerError> {
    let mut built_ident = String::new();

    // Skip the | at the beginning
    let mut chars = lexer.slice().chars().skip(1).peekable();
    while let Some(chr) = chars.next() {
        match chr {
            '\\' => match chars.peek() {
                Some('x' | 'X') => {
                    built_ident.push(read_hex_escape(&mut chars, || LexerError::MalformedIdent)?);
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
                Some(_) | None => Err(LexerError::MalformedIdent)?,
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
                Some('x' | 'X') => {
                    string.push(read_hex_escape(&mut chars, || LexerError::MalformedString)?)
                }
                _ => todo!(),
            },
            c => string.push(c),
        }
    }

    assert!(chars.next().is_none());
    Ok(Box::from(string.as_str()))
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SchemeNumber {
    ExactInteger {
        is_neg: bool,
        int: u64,
    },
    ExactRational {
        is_neg: bool,
        numer: u64,
        denom: u64,
    },
    ExactComplex {
        is_neg_real: bool,
        numer_real: u64,
        denom_real: u64,
        is_neg_im: bool,
        numer_im: u64,
        denom_im: u64,
    },
    ExactPolar {
        modulus_neg: bool,
        modulus_numer: u64,
        modulus_denom: u64,
        // argument is in radians
        argument_neg: bool,
        argument_numer: u64,
        argument_denom: u64,
    },

    // nan is always considered inexact (and writing the explicit "exact" literal is a lexer error)
    // basically +nan.0 is allowed (so is #i+nan.0), but #e+nan.0 is *not* allowed
    Inexact(f64),
    Inf {
        is_neg: bool,
        is_exact: bool,
    },
}

impl SchemeNumber {
    pub fn integer(sign: bool, int: u64) -> Self {
        // We represent integers as a standard sn/1+0/1i
        Self::ExactInteger { is_neg: sign, int }
    }
}

fn read_number(
    lexer: &mut Lexer<Token>,
    radix: u32,
    is_exact: bool,
) -> Result<SchemeNumber, LexerError> {
    let mut chars = lexer.slice().chars().peekable();

    // Exact can look like #!#e, #e#! or #! where ! is the base character [boxd] (or not present in case of decimal)
    // Inexact can only look like #!#i or #i#!
    let flags = {
        let mut flags = vec![];
        while let Some('#') = chars.peek() {
            let _ = chars.next(); // eat #
            let Some(
                flag @ ('e' | 'i' | 'b' | 'o' | 'x' | 'd' | 'E' | 'I' | 'B' | 'O' | 'X' | 'D'),
            ) = chars.next()
            else {
                Err(LexerError::MalformedNumber)?
            };
            flags.push(flag);
        }
        flags
    };

    let contains_flag = |flag: char| {
        flags.contains(&flag.to_ascii_lowercase()) || flags.contains(&flag.to_ascii_uppercase())
    };

    if contains_flag('b') && radix != 2
        || contains_flag('o') && radix != 8
        || contains_flag('x') && radix != 16
        || contains_flag('d') && radix != 10
    {
        // Radix mismatch
        Err(LexerError::MalformedNumber)?
    }

    let is_neg = match chars.peek() {
        Some('+') => {
            _ = chars.next();
            false
        }
        Some('-') => {
            _ = chars.next();
            true
        }
        _ => false,
    };

    // "i" and "n" are never used as a number in a base we use, so we can cheat a little
    match chars.peek() {
        Some('i' | 'n' | 'I' | 'N') => {
            // We have to be "inf.0" or "nan.0"
            static INF_REGEX: LazyLock<Regex> =
                LazyLock::new(|| Regex::new(r"^(?i)inf.0$").unwrap());
            static NAN_REGEX: LazyLock<Regex> =
                LazyLock::new(|| Regex::new(r"^(?i)nan.0$").unwrap());

            let rest = chars.collect::<Box<str>>();
            if INF_REGEX.is_match(&rest) {
                // is inf.0
                Ok(SchemeNumber::Inf { is_neg, is_exact })
            } else if NAN_REGEX.is_match(&rest) {
                // is nan.0
                if contains_flag('e') {
                    // You are not allowed to mark a nan as *explicitly* exact
                    Err(LexerError::MalformedNumber)
                } else {
                    Ok(SchemeNumber::Inexact(
                        if is_neg { -1.0 } else { 1.0 } * f64::NAN,
                    ))
                }
            } else {
                Err(LexerError::MalformedNumber)
            }
        }
        Some(c) if c.is_digit(radix) => {
            // we are a digit in our base! so lex it!
            dbg!(&flags);

            if contains_flag('e') || !contains_flag('i') {
                // we only read in exact numbers, which are
                // nnnn
                // nnnnsnnnni
                // nnnnsnnnn/nnnni
                // nnnn/nnnn
                // nnnn/nnnnsi
                // nnnn/nnnnsnnnni
                // nnnn/nnnnsnnnn/nnnni
                let mut integer_part = 0u64;
                while let Some(c) = chars.peek().copied() {
                    match c {
                        '+' | '-' | '/' => break,
                        c if c.is_digit(radix) => {
                            let _ = chars.next();
                            integer_part = integer_part
                                .checked_mul(radix as u64)
                                .ok_or(LexerError::NumberTooBig)?
                                .checked_add(c.to_digit(radix).unwrap() as u64)
                                .ok_or(LexerError::NumberTooBig)?;
                        }
                        _ => Err(LexerError::MalformedNumber)?,
                    }
                }

                match chars.peek() {
                    Some('+' | '-') => {
                        todo!("imaginary handling");
                    }
                    Some('/') => {
                        todo!("rational handling");
                    }
                    Some(_) => Err(LexerError::MalformedNumber)?,
                    None => Ok(SchemeNumber::ExactInteger {
                        is_neg,
                        int: integer_part,
                    }),
                }
            } else {
                // we handle !contains_flag e and contains_flag i
                todo!("Inexact handling")
            }
        }
        _ => Err(LexerError::MalformedNumber),
    }
}

#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    #[default]
    #[error("invalid token encountered")]
    Invalid,
    #[error("malformed identifier")]
    MalformedIdent,
    #[error("character literal too big")]
    CharacterTooBig,
    #[error("invalid Unicode codepoint: {0}")]
    InvalidCodepoint(u32),
    #[error("invalid directive: {0}")]
    InvalidDirective(Box<str>),
    #[error("invalid character name: {0}")]
    InvalidCharacterName(Box<str>),
    #[error("malformed string")]
    MalformedString,
    #[error("malformed number")]
    MalformedNumber,
    #[error("number literal too big")]
    NumberTooBig,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum Directive {
    FoldCase,
    NoFoldCase,
}

/// Tokens are lexed from some source, and can arbitrarily borrow from it.
#[derive(Debug, Clone, PartialEq, Logos)]
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

    // The number tower is supported by having each possible lex of a number type as a *separate* token type
    // Same as all the others, the lexer here is a bit more permissive to allow for better errors
    #[regex(r"(?i)((#e)?#b|#b(#e)?)([+-]?[01]+(/[01]+([+-][01]*i)?)?|[-+](inf|nan).0)?", |l| read_number(l, 2, true))]
    #[regex(r"(?i)(#i#b|#b#i)([+-]?[01]+(/[01]+([+-][01]*i)?)?|[-+](inf|nan).0)?", |l| read_number(l, 2, false))]
    Number(SchemeNumber),
}

impl Token {
    pub fn lexer(source: &str) -> Lexer<Self> {
        <Self as Logos>::lexer(source)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::SchemeNumber;

    use super::Token;
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

    #[test]
    fn test_number() {
        macro_rules! verify_number {
            (exact binary $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#e#b");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (binary exact $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#b#e");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (binary $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#b");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
        }

        // binary
        verify_number!(exact binary "+inf.0" as SchemeNumber::Inf { is_neg: false, is_exact: true });
        verify_number!(binary exact "-inf.0" as SchemeNumber::Inf { is_neg: true, is_exact: true });
        verify_number!(binary exact "0" as SchemeNumber::integer(false, 0));
        verify_number!(exact binary "-0" as SchemeNumber::integer(true, 0));
        verify_number!(binary "110" as SchemeNumber::integer(false, 6));
    }
}
