use std::{collections::HashMap, sync::LazyLock};

pub use logos::Span;
use logos::{Lexer, Logos};

use crate::{ExactReal, SchemeNumber};

fn process_piped_ident(lexer: &mut Lexer<Token>) -> Result<Box<str>, LexerError> {
    let mut built_ident = String::new();

    // Skip the | at the beginning
    let mut chars = lexer.slice().chars().skip(1).peekable();
    while let Some(chr) = chars.next() {
        match chr {
            '\\' => match chars.peek() {
                Some('x' | 'X') => {
                    built_ident.push(read_hex_escape(&mut chars, || {
                        LexerError::MalformedIdentifier
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
                Some(_) | None => Err(LexerError::MalformedIdentifier)?,
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

fn read_number(lexer: &mut Lexer<Token>, radix: u32) -> Result<SchemeNumber, LexerError> {
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

    // fn read_sign(chars: &mut std::iter::Peekable<impl Iterator<Item = char>>) -> bool {
    //     match chars.peek() {
    //         Some('+') => {
    //             _ = chars.next();
    //             false
    //         }
    //         Some('-') => {
    //             _ = chars.next();
    //             true
    //         }
    //         _ => false,
    //     }
    // }

    fn read_number_part(
        iter: &mut std::iter::Peekable<impl Iterator<Item = char>>,
        radix: u32,
        is_imaginary: bool,
    ) -> Result<ExactReal, LexerError> {
        // Write a FSM to recognize the number and parse it into a RealNumber
        #[derive(Debug)]
        enum State {
            Start,
            ReadSign,
            ReadDigit,
            ReadRational,
            FinishImaginaryRational,
            ReadDecipoint,
            ReadExponentSign,
            ReadExponent,
            FinishImaginaryDecimal,
            // on inf.0
            OnI,
            OnIn,
            OnInf,
            OnDotInf,
            OnDotInfIm,
            // on nan.0
            OnN,
            OnNa,
            OnNan,
            OnDotNan,
            OnDotNanIm,
        }
        let mut s = State::Start;
        let mut number_state = None::<u64>;
        let mut second_number_state = None::<u64>;
        let mut third_number_state = None::<u64>;
        let mut exponent_sign_state = None::<bool>;
        let mut is_neg_state = None::<bool>;
        loop {
            eprintln!("State ({s:?}) (im? {is_imaginary}) ({number_state:?} {second_number_state:?} {third_number_state:?} {exponent_sign_state:?} {is_neg_state:?})");
            let ns = match s {
                // Start
                State::Start => match iter.next() {
                    Some('+') => {
                        is_neg_state = Some(false);
                        State::ReadSign
                    }
                    Some('-') => {
                        is_neg_state = Some(true);
                        State::ReadSign
                    }
                    Some('.') if radix == 10 => State::ReadDecipoint,
                    Some(c) if c.is_digit(radix) => {
                        number_state = Some(c.to_digit(radix).unwrap() as u64);
                        State::ReadDigit
                    }
                    _ => return Err(LexerError::MalformedNumber),
                },
                // read sign, inf nan or num
                State::ReadSign => match iter.next() {
                    Some('i') => State::OnI,
                    Some('n') => State::OnN,
                    Some('.') if radix == 10 => State::ReadDecipoint,
                    Some(c) if c.is_digit(radix) => {
                        number_state = Some(c.to_digit(radix).unwrap() as u64);
                        State::ReadDigit
                    }
                    _ => return Err(LexerError::MalformedNumber),
                },
                // inf reading
                State::OnI => {
                    assert!(is_neg_state.is_some());
                    match iter.next() {
                        Some('n') => State::OnIn,
                        _ if is_imaginary => {
                            return Ok(ExactReal::Integer {
                                value: 1,
                                is_neg: is_neg_state.unwrap(),
                            })
                        }
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                State::OnIn => match iter.next() {
                    Some('f') => State::OnInf,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnInf => match iter.next() {
                    Some('.') => State::OnDotInf,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnDotInf => match iter.next() {
                    Some('0') if !is_imaginary => {
                        return Ok(ExactReal::Inf {
                            is_neg: is_neg_state.unwrap(),
                        })
                    }
                    Some('0') => State::OnDotInfIm,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnDotInfIm => {
                    assert!(is_imaginary);
                    return match iter.next() {
                        Some('i' | 'I') => Ok(ExactReal::Inf {
                            is_neg: is_neg_state.unwrap(),
                        }),
                        _ => Err(LexerError::MalformedNumber),
                    };
                }
                // nan reading
                State::OnN => {
                    assert!(is_neg_state.is_some());
                    match iter.next() {
                        Some('a') => State::OnNa,
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                State::OnNa => match iter.next() {
                    Some('n') => State::OnNan,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnNan => match iter.next() {
                    Some('.') => State::OnDotNan,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnDotNan => match iter.next() {
                    Some('0') if !is_imaginary => {
                        return Ok(ExactReal::Nan {
                            is_neg: is_neg_state.unwrap(),
                        })
                    }
                    Some('0') => State::OnDotNanIm,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::OnDotNanIm => {
                    assert!(is_imaginary);
                    return match iter.next() {
                        Some('i' | 'I') => Ok(ExactReal::Nan {
                            is_neg: is_neg_state.unwrap(),
                        }),
                        _ => Err(LexerError::MalformedNumber),
                    };
                }
                // read digit, continue reading digits until interrupt [/+-] (add [.e] if decimal)
                State::ReadDigit => {
                    assert!(number_state.is_some());
                    match iter.peek().copied() {
                        Some('.') if radix == 10 => {
                            let _ = iter.next();
                            State::ReadDecipoint
                        }
                        Some('e' | 'E') if radix == 10 => {
                            let _ = iter.next();
                            State::ReadExponent
                        }
                        Some('/') => {
                            let _ = iter.next();
                            State::ReadRational
                        }
                        Some('+' | '-') | None => {
                            // What follows must be an imaginary number, so break successfully if we are a real number
                            return if !is_imaginary {
                                Ok(ExactReal::Integer {
                                    value: number_state.unwrap(),
                                    is_neg: is_neg_state.unwrap_or(false),
                                })
                            } else {
                                Err(LexerError::MalformedNumber)
                            };
                        }
                        Some('i') if is_imaginary => {
                            // b/c of how we parse/detect imaginary numbers, is_neg_state will always be set
                            // when encountering an imaginary number
                            assert!(is_neg_state.is_some());
                            return Ok(ExactReal::Integer {
                                value: number_state.unwrap(),
                                is_neg: is_neg_state.unwrap(),
                            });
                        }
                        Some(c) if c.is_digit(radix) => {
                            let _ = iter.next();
                            number_state = Some(
                                number_state
                                    .unwrap()
                                    .checked_mul(radix as u64)
                                    .ok_or(LexerError::NumberTooBig)?
                                    .checked_add(c.to_digit(radix).unwrap() as u64)
                                    .ok_or(LexerError::NumberTooBig)?,
                            );
                            State::ReadDigit
                        }
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                // read [eE], read in the exponent (or jump straight to digits)
                State::ReadExponent => {
                    assert!(radix == 10);
                    match iter.next() {
                        Some('+') => {
                            exponent_sign_state = Some(false);
                            State::ReadExponentSign
                        }
                        Some('-') => {
                            exponent_sign_state = Some(true);
                            State::ReadExponentSign
                        }
                        Some(c @ '0'..='9') => {
                            third_number_state = Some(c.to_digit(10).unwrap() as u64);
                            State::ReadExponentSign
                        }
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                // read ., read the postdot and/or exponent
                State::ReadDecipoint => {
                    assert!(radix == 10);
                    match iter.peek().copied() {
                        // to allow writing NN[.]e10, but not .e10 (.1e10 is allowed)
                        Some('e' | 'E')
                            if number_state.is_some() || second_number_state.is_some() =>
                        {
                            let _ = iter.next();
                            State::ReadExponent
                        }
                        Some(c @ '0'..='9') => {
                            let _ = iter.next();
                            if let Some(sn) = second_number_state {
                                second_number_state = Some(
                                    sn.checked_mul(10)
                                        .ok_or(LexerError::NumberTooBig)?
                                        .checked_add(c.to_digit(radix).unwrap() as u64)
                                        .ok_or(LexerError::NumberTooBig)?,
                                );
                            } else {
                                second_number_state = Some(c.to_digit(radix).unwrap() as u64);
                            }
                            State::ReadDecipoint
                        }
                        Some('+' | '-') if !is_imaginary => {
                            // Next is an imaginary number
                            return Ok(ExactReal::Decimal {
                                base: number_state.unwrap_or(0),
                                post_dot: second_number_state.unwrap_or(0),
                                exponent: 0,
                                exponent_neg: false,
                                is_neg: is_neg_state.unwrap_or(false),
                            });
                        }
                        None if !is_imaginary => {
                            // Number ended
                            return Ok(ExactReal::Decimal {
                                base: number_state.unwrap_or(0),
                                post_dot: second_number_state.unwrap_or(0),
                                exponent: 0,
                                exponent_neg: false,
                                is_neg: is_neg_state.unwrap_or(false),
                            });
                        }
                        Some('i') => State::FinishImaginaryDecimal,
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                // get trailing digits of a decimal
                State::ReadExponentSign => {
                    assert!(radix == 10);
                    match iter.peek().copied() {
                        Some(c @ '0'..='9') => {
                            let _ = iter.next();
                            if let Some(tn) = third_number_state {
                                third_number_state = Some(
                                    tn.checked_mul(10)
                                        .ok_or(LexerError::NumberTooBig)?
                                        .checked_add(c.to_digit(radix).unwrap() as u64)
                                        .ok_or(LexerError::NumberTooBig)?,
                                );
                            } else {
                                third_number_state = Some(c.to_digit(radix).unwrap() as u64);
                            }
                            State::ReadExponentSign
                        }
                        Some('+' | '-') | None if !is_imaginary => {
                            return Ok(ExactReal::Decimal {
                                base: number_state.unwrap_or(0),
                                post_dot: second_number_state.unwrap_or(0),
                                exponent: third_number_state.unwrap_or(0),
                                exponent_neg: exponent_sign_state.unwrap_or(false),
                                is_neg: is_neg_state.unwrap_or(false),
                            })
                        }
                        Some('i') if is_imaginary => State::FinishImaginaryDecimal,
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                State::FinishImaginaryDecimal => {
                    assert!(is_imaginary && radix == 10);
                    return match iter.next() {
                        Some('i') => Ok(ExactReal::Decimal {
                            base: number_state.unwrap_or(0),
                            post_dot: second_number_state.unwrap_or(0),
                            exponent: third_number_state.unwrap_or(0),
                            exponent_neg: exponent_sign_state.unwrap_or(false),
                            is_neg: is_neg_state.unwrap_or(false),
                        }),
                        _ => Err(LexerError::MalformedNumber),
                    };
                }
                s => todo!("unimplemented state {s:?}"),
            };
            s = ns;
        }
    }

    if contains_flag('e') || !contains_flag('i') {
        // we only read in exact numbers, which are
        // nnnn
        // nnnnsnnnni
        // nnnnsnnnn/nnnni
        // nnnn/nnnn
        // nnnn/nnnnsi
        // nnnn/nnnnsnnnni
        // nnnn/nnnnsnnnn/nnnni
        let real_part = read_number_part(&mut chars, radix, false)?;
        match chars.peek() {
            Some('+' | '-') => {
                let im_part = read_number_part(&mut chars, radix, true)?;
                Ok(SchemeNumber::ExactComplex {
                    real: real_part,
                    imaginary: im_part,
                })
            }
            Some(_) => Err(LexerError::MalformedNumber)?,
            None => Ok(SchemeNumber::Exact(real_part)),
        }
    } else {
        // we handle !contains_flag e and contains_flag i
        let real_part = read_number_part(&mut chars, radix, false)?;
        match chars.peek() {
            Some('+' | '-') => {
                let im_part = read_number_part(&mut chars, radix, true)?;
                Ok(SchemeNumber::InexactComplex {
                    real: real_part.inexact(),
                    imaginary: im_part.inexact(),
                })
            }
            Some(_) => Err(LexerError::MalformedNumber)?,
            None => Ok(SchemeNumber::Inexact(real_part.inexact())),
        }
    }
}

#[derive(thiserror::Error, Debug, PartialEq, Clone, Default)]
pub enum LexerError {
    #[default]
    #[error("invalid token encountered")]
    Invalid,
    #[error("malformed identifier")]
    MalformedIdentifier,
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

    // The number tower is supported at least by the lexer (and currently is mostly rejected by the general parser)
    // Same as all the others, the lexer here is a bit more permissive to allow for better errors
    // binary
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?[01]+(/[01]+)?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?[01]+(/[01]+)?[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?[01]+(/[01]+)?[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?[+-](inf|nan).0i?", |l| read_number(l, 2))]
    // octal
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?[0-7]+(/[0-7]+)?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?[0-7]+(/[0-7]+)?[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?[0-7]+(/[0-7]+)?[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?[+-](inf|nan).0i?", |l| read_number(l, 8))]
    // hex
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?[0-9a-f]+(/[0-9a-f]+)?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-](inf|nan).0i?", |l| read_number(l, 16))]
    // decimal
    // - decimal real
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+e[+-]?[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?", |l| read_number(l, 10))]
    // - decimal complex
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+[+-]\.[0-9]+(e[+-]?[0-9]+)i", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0[+-]\.[0-9]+(e[+-]?[0-9]+)i", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-][0-9]+e[+-]?[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    // - inexact decimal real
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+e[+-]?[0-9]+", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?", |l| read_number(l, 10))]
    // - inexact decimal complex
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+[+-]\.[0-9]+(e[+-]?[0-9]+)i", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0[+-]\.[0-9]+(e[+-]?[0-9]+)i", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]*i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-][0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?[+-]\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    Number(SchemeNumber),
}

impl Token {
    pub fn lexer(source: &str) -> Lexer<Self> {
        <Self as Logos>::lexer(source)
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::SchemeNumber, ExactReal};

    use super::Token;
    use arbtest::{arbtest, ArbTest};
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
        // My own
        test_valid!(r"|λ\x3bb;|" as "λλ");
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
            (exact octal $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#e#o");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (octal exact $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#o#e");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (octal $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#o");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (exact hex $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#e#x");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (hex exact $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#x#e");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (hex $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#x");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (exact decimal $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#e#d");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (decimal exact $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#d#e");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            (decimal $source:literal as $num:expr) => {
                let mut source = String::new();
                source.push_str("#d");
                source.push_str($source);
                let token = Token::lexer(&source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
            ($source:literal as $num:expr) => {
                let token = Token::lexer($source).next();
                let_assert!(Some(Ok(Token::Number(bnum))) = token);
                check!(bnum == $num);
            };
        }

        // binary
        // verify_number!(exact binary "+inf.0" as SchemeNumber::Inf { is_neg: false, is_exact: true });
        // verify_number!(binary exact "-inf.0" as SchemeNumber::Inf { is_neg: true, is_exact: true });
        verify_number!(binary exact "0" as SchemeNumber::integer(false, 0));
        verify_number!(exact binary "-0" as SchemeNumber::integer(true, 0));
        verify_number!(binary "110" as SchemeNumber::integer(false, 6));
        verify_number!("42.4e-4" as SchemeNumber::real_decimal(false, 42, 4, true, 4));
        verify_number!(hex exact "-a0" as SchemeNumber::integer(true, 160));
        // todo more thorough tests
    }

    #[test]
    fn test_number_arbtest_decimal() {
        arbtest(|u| {
            let number: ExactReal = u.arbitrary()?;
            let decimal = number.display(10).unwrap();
            check!(
                Token::lexer(&decimal).next()
                    == Some(Ok(Token::Number(SchemeNumber::Exact(number)))),
                "{number:?} `{decimal}` does not roundtrip"
            );
            let inexact_decimal = format!("#i#d{}", number.display(10).unwrap());
            // might be NaN
            if matches!(number, ExactReal::Nan { .. }) {
                let_assert!(
                    Some(Ok(Token::Number(SchemeNumber::Inexact(inum)))) =
                        Token::lexer(&inexact_decimal).next()
                );
                check!(inum.is_nan());
            } else {
                check!(
                    Token::lexer(&inexact_decimal).next()
                        == Some(Ok(Token::Number(SchemeNumber::Inexact(number.inexact())))),
                    "inexact {number:?} `{inexact_decimal}` does not roundtrip"
                );
            }
            let im: ExactReal = u.arbitrary()?;
            let im_decimal = format!(
                "{}{}{}i",
                number.display(10).unwrap(),
                if im.is_numeric() && !im.is_neg() {
                    "+"
                } else {
                    ""
                },
                im.display(10).unwrap(),
            );
            check!(
                Token::lexer(&im_decimal).next()
                    == Some(Ok(Token::Number(SchemeNumber::ExactComplex {
                        real: number,
                        imaginary: im,
                    }))),
                "(real {number:?}, im {im:?}) `{im_decimal}` does not roundtrip"
            );
            let inexact_im_decimal = format!(
                "#d#i{}{}{}i",
                number.display(10).unwrap(),
                if im.is_numeric() && !im.is_neg() {
                    "+"
                } else {
                    ""
                },
                im.display(10).unwrap(),
            );
            match (number, im) {
                (ExactReal::Nan { .. }, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary.is_nan());
                }
                (ExactReal::Nan { .. }, _) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary == im.inexact());
                }
                (_, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(real == number.inexact() && imaginary.is_nan());
                }
                _ => {
                    check!(
                        Token::lexer(&inexact_im_decimal).next()
                            == Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                                real: number.inexact(),
                                imaginary: im.inexact(),
                            }))),
                        "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip"
                    );
                }
            }
            Ok(())
        });
    }

    #[test]
    fn test_number_arbtest_hex() {
        test_number_arbtest_nondecimal(16);
    }

    #[test]
    fn test_number_arbtest_binary() {
        test_number_arbtest_nondecimal(2);
    }

    #[test]
    fn test_number_arbtest_octal() {
        test_number_arbtest_nondecimal(8);
    }

    fn test_number_arbtest_nondecimal(
        radix: u32,
    ) -> ArbTest<impl FnMut(&mut arbitrary::Unstructured<'_>) -> arbitrary::Result<()>> {
        assert!(radix != 10);
        let radix_str = match radix {
            2 => "#b",
            8 => "#o",
            16 => "#x",
            _ => panic!("Unsupported radix {radix}"),
        };

        arbtest(move |u| {
            let number: ExactReal = u.arbitrary()?;
            let decimal = format!(
                "{radix_str}{}",
                match number.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                }
            );
            check!(
                Token::lexer(&decimal).next()
                    == Some(Ok(Token::Number(SchemeNumber::Exact(number)))),
                "{number:?} `{decimal}` does not roundtrip"
            );
            let inexact_decimal = format!(
                "#i{radix_str}{}",
                match number.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                }
            );
            // might be NaN
            if matches!(number, ExactReal::Nan { .. }) {
                let_assert!(
                    Some(Ok(Token::Number(SchemeNumber::Inexact(inum)))) =
                        Token::lexer(&inexact_decimal).next()
                );
                check!(inum.is_nan());
            } else {
                check!(
                    Token::lexer(&inexact_decimal).next()
                        == Some(Ok(Token::Number(SchemeNumber::Inexact(number.inexact())))),
                    "inexact {number:?} `{inexact_decimal}` does not roundtrip"
                );
            }
            let im: ExactReal = u.arbitrary()?;
            let im_decimal = format!(
                "{radix_str}{}{}{}i",
                match number.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                },
                if im.is_numeric() && !im.is_neg() {
                    "+"
                } else {
                    ""
                },
                match im.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                },
            );
            check!(
                Token::lexer(&im_decimal).next()
                    == Some(Ok(Token::Number(SchemeNumber::ExactComplex {
                        real: number,
                        imaginary: im,
                    }))),
                "(real {number:?}, im {im:?}) `{im_decimal}` does not roundtrip"
            );
            let inexact_im_decimal = format!(
                "{radix_str}#i{}{}{}i",
                match number.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                },
                if im.is_numeric() && !im.is_neg() {
                    "+"
                } else {
                    ""
                },
                match im.display(radix) {
                    Some(n) => n,
                    None => return Ok(()),
                },
            );
            match (number, im) {
                (ExactReal::Nan { .. }, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                (ExactReal::Nan { .. }, _) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                    check!(imaginary == im.inexact(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                (_, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = Token::lexer(&inexact_im_decimal).next()
                    );
                    check!(imaginary.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                    check!(real == number.inexact(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                _ => {
                    check!(
                        Token::lexer(&inexact_im_decimal).next()
                            == Some(Ok(Token::Number(SchemeNumber::InexactComplex {
                                real: number.inexact(),
                                imaginary: im.inexact(),
                            }))),
                        "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip"
                    );
                }
            }
            Ok(())
        })
    }
}
