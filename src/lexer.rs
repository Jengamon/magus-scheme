use std::{collections::HashMap, sync::LazyLock};

use arbitrary::Arbitrary;
use logos::Logos;
pub use logos::Span;

use crate::{ExactReal, SchemeNumber};

fn process_piped_ident(lexer: &mut logos::Lexer<SyntaxToken>) -> Result<Box<str>, LexerError> {
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
                Some('|') => {
                    built_ident.push('|');
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

fn process_character(lexer: &mut logos::Lexer<SyntaxToken>) -> Result<char, LexerError> {
    // the lexer allows through #\ and \, so check
    // if it's the second, and if so, reject as malformed
    if !lexer.slice().starts_with('#') {
        return Err(LexerError::MalformedCharacter);
    }

    // skip the #\
    Ok(lexer.slice().chars().nth(2).unwrap())
}

fn process_named_character(lexer: &mut logos::Lexer<SyntaxToken>) -> Result<char, LexerError> {
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

    // the lexer allows through #\ and \, so check
    // if it's the second, and if so, reject as malformed
    if !lexer.slice().starts_with('#') {
        return Err(LexerError::MalformedCharacter);
    }

    // skip the #\ at the front
    let name = &lexer.slice()[2..];
    NAMED_MAP
        .get(name)
        .copied()
        .ok_or_else(|| LexerError::InvalidCharacterName(Box::from(name)))
}

fn process_hex_character(lexer: &mut logos::Lexer<SyntaxToken>) -> Result<char, LexerError> {
    let mut value = 0u32;

    // the lexer allows through #\x and \x, so check
    // if it's the second, and if so, reject as malformed
    if !lexer.slice().starts_with('#') {
        return Err(LexerError::MalformedCharacter);
    }

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
fn read_hex_escape(
    iter: &mut std::iter::Peekable<impl Iterator<Item = char>>,
    on_malformed: impl Fn() -> LexerError,
) -> Result<char, LexerError> {
    // consume the x
    let _ = iter.next();

    let mut char_code = 0u32;
    while let Some(c) = iter.peek() {
        match c {
            ';' => break,
            c @ ('0'..='9' | 'a'..='f' | 'A'..='F') => {
                char_code = char_code
                    .checked_mul(16)
                    .ok_or(on_malformed())?
                    .checked_add(c.to_digit(16).unwrap())
                    .ok_or(on_malformed())?;
                _ = iter.next();
            }
            _ => Err(on_malformed())?,
        }
    }
    if iter.next() != Some(';') {
        return Err(on_malformed());
    }
    char::from_u32(char_code).ok_or(on_malformed())
}

fn process_string(lexer: &mut logos::Lexer<SyntaxToken>) -> Result<Box<str>, LexerError> {
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

fn read_number(
    lexer: &mut logos::Lexer<SyntaxToken>,
    radix: u32,
) -> Result<SchemeNumber, LexerError> {
    // Before we convert, run a simple procedure that find if we are just looking at an imaginary number
    let num_start = lexer.slice().trim_start_matches([
        '#', 'e', 'E', 'i', 'I', 'b', 'B', 'o', 'O', 'x', 'X', 'd', 'D',
    ]);
    let without_sign = num_start.strip_prefix(['+', '-']).unwrap_or(num_start);
    let just_imaginary = ["-i", "+i"].contains(&lexer.slice())
        || if let Some(without_inf) = without_sign.strip_prefix("inf.0") {
            without_inf.contains('i') && !without_inf.contains(['+', '-'])
        } else {
            without_sign.contains('i') && !without_sign.contains(['+', '-'])
        };
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
            // eprintln!("State ({s:?}) (im? {is_imaginary}) ({number_state:?} {second_number_state:?} {third_number_state:?} {exponent_sign_state:?} {is_neg_state:?})");
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
                    Some('i' | 'I') => State::OnI,
                    Some('n' | 'N') => State::OnN,
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
                        Some('n' | 'N') => State::OnIn,
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
                    Some('f' | 'F') => State::OnInf,
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
                        Some('a' | 'A') => State::OnNa,
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                State::OnNa => match iter.next() {
                    Some('n' | 'N') => State::OnNan,
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
                        Some('i' | 'I') if is_imaginary => {
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
                // read /, continue rational
                State::ReadRational => match iter.peek().copied() {
                    Some(c) if c.is_digit(radix) => {
                        let _ = iter.next();
                        if let Some(sn) = second_number_state {
                            second_number_state = Some(
                                sn.checked_mul(radix as u64)
                                    .ok_or(LexerError::NumberTooBig)?
                                    .checked_add(c.to_digit(radix).unwrap() as u64)
                                    .ok_or(LexerError::NumberTooBig)?,
                            );
                        } else {
                            second_number_state = Some(c.to_digit(radix).unwrap() as u64);
                        }
                        State::ReadRational
                    }
                    Some('+' | '-') | None if !is_imaginary && second_number_state.is_some() => {
                        return Ok(ExactReal::Rational {
                            numer: number_state.unwrap_or(0),
                            denom: second_number_state.unwrap_or(0),
                            is_neg: is_neg_state.unwrap_or(false),
                        })
                    }
                    Some('i' | 'I') if is_imaginary => State::FinishImaginaryRational,
                    _ => return Err(LexerError::MalformedNumber),
                },
                State::FinishImaginaryRational => match iter.next() {
                    Some('i' | 'I') => {
                        return Ok(ExactReal::Rational {
                            numer: number_state.unwrap_or(0),
                            denom: second_number_state.unwrap_or(0),
                            is_neg: is_neg_state.unwrap_or(false),
                        })
                    }
                    _ => return Err(LexerError::MalformedNumber),
                },
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
                        Some('i' | 'I') if is_imaginary => State::FinishImaginaryDecimal,
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
                        Some('i' | 'I') if is_imaginary => State::FinishImaginaryDecimal,
                        _ => return Err(LexerError::MalformedNumber),
                    }
                }
                State::FinishImaginaryDecimal => {
                    assert!(is_imaginary && radix == 10);
                    return match iter.next() {
                        Some('i' | 'I') => Ok(ExactReal::Decimal {
                            base: number_state.unwrap_or(0),
                            post_dot: second_number_state.unwrap_or(0),
                            exponent: third_number_state.unwrap_or(0),
                            exponent_neg: exponent_sign_state.unwrap_or(false),
                            is_neg: is_neg_state.unwrap_or(false),
                        }),
                        _ => Err(LexerError::MalformedNumber),
                    };
                }
            };
            s = ns;
        }
    }

    if contains_flag('e') || !contains_flag('i') {
        // we only read in exact numbers
        // but we default to exact numbers
        let real_part = read_number_part(&mut chars, radix, just_imaginary)?;
        match chars.peek() {
            Some('+' | '-') if !just_imaginary => {
                let im_part = read_number_part(&mut chars, radix, true)?;
                Ok(SchemeNumber::ExactComplex {
                    real: real_part,
                    imaginary: im_part,
                })
            }
            Some(_) => Err(LexerError::MalformedNumber)?,
            None if !just_imaginary => Ok(SchemeNumber::Exact(real_part)),
            None => Ok(SchemeNumber::ExactComplex {
                real: ExactReal::Integer {
                    value: 0,
                    is_neg: false,
                },
                imaginary: real_part,
            }),
        }
    } else {
        // we handle !contains_flag e and contains_flag i
        let real_part = read_number_part(&mut chars, radix, just_imaginary)?;
        match chars.peek() {
            Some('+' | '-') if !just_imaginary => {
                let im_part = read_number_part(&mut chars, radix, true)?;
                Ok(SchemeNumber::InexactComplex {
                    real: real_part.inexact(),
                    imaginary: im_part.inexact(),
                })
            }
            Some(_) => Err(LexerError::MalformedNumber)?,
            None if !just_imaginary => Ok(SchemeNumber::Inexact(real_part.inexact())),
            None => Ok(SchemeNumber::InexactComplex {
                real: 0.0,
                imaginary: real_part.inexact(),
            }),
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
    #[error("malformed character")]
    MalformedCharacter,
    #[error("malformed string")]
    MalformedString,
    #[error("malformed number")]
    MalformedNumber,
    #[error("number literal too big")]
    NumberTooBig,
    #[error("label too big")]
    LabelTooBig,
    #[error("trigger too big")]
    TriggerTooBig,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Arbitrary)]
pub enum Directive {
    FoldCase,
    NoFoldCase,
}

#[derive(Debug, Clone, PartialEq, Logos, Arbitrary)]
pub enum NestedCommentToken {
    #[token("|#")]
    EndNestedComment,
    #[token("#|")]
    StartNestedComment,
    #[regex(r"[^|#]+")]
    #[token("#")]
    #[token("|")]
    CommentText,
}

impl NestedCommentToken {
    pub fn lexer(source: &str) -> logos::Lexer<NestedCommentToken> {
        <Self as Logos>::lexer(source)
    }
}

/// Tokens are lexed from some source, and can arbitrarily borrow from it.
#[derive(Debug, Clone, PartialEq, Logos, Arbitrary)]
#[logos(error = LexerError)]
pub enum SyntaxToken {
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
    #[token(".")]
    Dot,
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
    #[regex(r#"\|([^|\\]|\\[xXabntr|])*\|"#, process_piped_ident)]
    #[token("+", |l| Box::from(l.slice()))]
    #[token("-", |l| Box::from(l.slice()))]
    #[regex(r"[-+][a-zA-Z!$%&*/:<=>?^_~+\-@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"[-+]\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    #[regex(r"\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*", |l| Box::from(l.slice()))]
    Identifier(Box<str>),
    #[regex("(?i)#t(rue)?", |_| true)]
    #[regex("(?i)#f(alse)?", |_| false)]
    Boolean(bool),
    #[regex(r"#?\\.", callback = process_character)]
    #[regex(r"#?\\[a-zA-Z]+", priority = 2, callback = process_named_character)]
    #[regex(r"(?i)#?\\x[0-9a-f]+", callback = process_hex_character)]
    Character(char),
    #[regex(r#""([^\\"]|\\[abntr"\\xX])*""#, process_string)]
    String(Box<str>),

    // The number tower is supported at least by the lexer (and currently is mostly rejected by the general parser)
    // Same as all the others, the lexer here is a bit more permissive to allow for better errors
    // explicit
    #[regex("(?i)((#[ie])?#b|#b(#[ie])?)[+-]i", priority = 5, callback = |l| read_number(l, 2))]
    #[regex("(?i)((#[ie])?#o|#o(#[ie])?)[+-]i", priority = 5, callback = |l| read_number(l, 8))]
    #[regex("(?i)((#[ie])?#x|#x(#[ie])?)[+-]i", priority = 5, callback = |l| read_number(l, 16))]
    #[regex("(?i)((#[ie])?(#d)?|(#d)?(#[ie])?)[+-]i", priority = 5, callback = |l| read_number(l, 10))]
    // binary
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?([01]*i|[01]+(/[01]+)?)", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?[01]+(/[01]+)?[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-](inf|nan).0[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)((#e)?#b|#b(#e)?)[+-]?[01]+(/[01]+)?[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-](inf|nan).0[+-][01]*(/[01]+)?i?", |l| read_number(l, 2))]
    #[regex(r"(?i)(#i#b|#b#i)[+-]?[01]+(/[01]+)?[+-](inf|nan).0i?", |l| read_number(l, 2))]
    // octal
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?([0-7]+(/[0-7]+)?|[0-7]+i)", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?[0-7]+(/[0-7]+)?[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-](inf|nan).0[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)((#e)?#o|#o(#e)?)[+-]?[0-7]+(/[0-7]+)?[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-](inf|nan).0[+-][0-7]*(/[0-7]+)?i?", |l| read_number(l, 8))]
    #[regex(r"(?i)(#i#o|#o#i)[+-]?[0-7]+(/[0-7]+)?[+-](inf|nan).0i?", |l| read_number(l, 8))]
    // hex
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?([0-9a-f]+(/[0-9a-f]+)?|[0-9a-f]*i)", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-](inf|nan).0[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)((#e)?#x|#x(#e)?)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0[+-](inf|nan).0i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-](inf|nan).0[+-][0-9a-f]*(/[0-9a-f]+)?i?", |l| read_number(l, 16))]
    #[regex(r"(?i)(#i#x|#x#i)[+-]?[0-9a-f]+(/[0-9a-f]+)?[+-](inf|nan).0i?", |l| read_number(l, 16))]
    // decimal
    // - decimal real
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+e[+-]?[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)((#e)?(#d)?|(#d)?(#e)?)[+-]?\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
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
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-](inf|nan).0i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+/[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+e[+-]?[0-9]+i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?[0-9]+\.[0-9]*(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
    #[regex(r"(?i)(#i(#d)?|(#d)?#i)[+-]?\.[0-9]+(e[+-]?[0-9]+)?i?", |l| read_number(l, 10))]
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
    #[regex(r"#[0-9]+=", |l| l.slice().chars().skip(1).take(l.slice().len()-2).collect::<Box<str>>().parse::<usize>().map_err(|_| LexerError::LabelTooBig))]
    DatumLabel(usize),
    #[regex(r"#[0-9]+#", |l| l.slice().chars().skip(1).take(l.slice().len()-2).collect::<Box<str>>().parse::<usize>().map_err(|_| LexerError::TriggerTooBig))]
    DatumLabelValue(usize),
}

impl SyntaxToken {
    pub fn lexer(source: &str) -> logos::Lexer<SyntaxToken> {
        <Self as Logos>::lexer(source)
    }
}

#[derive(Clone)]
enum Modes<'src> {
    Syntax(logos::Lexer<'src, SyntaxToken>),
    NestedComment(logos::Lexer<'src, NestedCommentToken>),
}

pub struct Lexer<'src> {
    mode: Modes<'src>,
    nested_comment_level: usize,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Result<Token, LexerError>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        // Check what kind of lexer we should be, and morph to be the right one
        if self.nested_comment_level > 0 {
            // we should be a nested comment lexer
            self.mode = match self.mode.clone() {
                Modes::NestedComment(nc) => Modes::NestedComment(nc),
                Modes::Syntax(syntax) => Modes::NestedComment(syntax.morph()),
            };
        } else {
            // we should be in syntax mode
            self.mode = match self.mode.clone() {
                Modes::NestedComment(nc) => Modes::Syntax(nc.morph()),
                Modes::Syntax(syntax) => Modes::Syntax(syntax),
            };
        }

        match &mut self.mode {
            Modes::Syntax(ref mut syntax) => {
                assert!(self.nested_comment_level == 0);
                match syntax.next() {
                    Some(Ok(s @ SyntaxToken::StartNestedComment)) => {
                        self.nested_comment_level += 1;
                        Some((Ok(Token::Syntax(s)), syntax.span()))
                    }
                    Some(Ok(tok)) => Some((Ok(Token::Syntax(tok)), syntax.span())),
                    Some(Err(err)) => Some((Err(err), syntax.span())),
                    None => None,
                }
            }
            Modes::NestedComment(ref mut nc) => {
                assert!(self.nested_comment_level > 0);
                match nc.next() {
                    Some(Ok(s @ NestedCommentToken::StartNestedComment)) => {
                        self.nested_comment_level += 1;
                        Some((Ok(Token::NestedComment(s)), nc.span()))
                    }
                    Some(Ok(e @ NestedCommentToken::EndNestedComment)) => {
                        self.nested_comment_level -= 1;
                        Some((Ok(Token::NestedComment(e)), nc.span()))
                    }
                    Some(Ok(t @ NestedCommentToken::CommentText)) => {
                        Some((Ok(Token::NestedComment(t)), nc.span()))
                    }
                    Some(Err(())) => Some((Err(LexerError::Invalid), nc.span())),
                    None => None,
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Arbitrary)]
pub enum Token {
    Syntax(SyntaxToken),
    NestedComment(NestedCommentToken),
}

impl Token {
    pub fn lexer(source: &str) -> Lexer {
        Lexer {
            mode: Modes::Syntax(SyntaxToken::lexer(source)),
            nested_comment_level: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::SchemeNumber, ExactReal};

    use super::SyntaxToken;
    use arbtest::{arbtest, ArbTest};
    use assert2::{assert, check, let_assert};

    #[test]
    fn test_number_arbtest_decimal() {
        arbtest(|u| {
            let number: ExactReal = u.arbitrary()?;
            let decimal = number.display(10).unwrap();
            check!(
                SyntaxToken::lexer(&decimal).next()
                    == Some(Ok(SyntaxToken::Number(SchemeNumber::Exact(number)))),
                "{number:?} `{decimal}` does not roundtrip"
            );
            let inexact_decimal = format!("#i#d{}", number.display(10).unwrap());
            // might be NaN
            if matches!(number, ExactReal::Nan { .. }) {
                let_assert!(
                    Some(Ok(SyntaxToken::Number(SchemeNumber::Inexact(inum)))) =
                        SyntaxToken::lexer(&inexact_decimal).next()
                );
                check!(inum.is_nan());
            } else {
                check!(
                    SyntaxToken::lexer(&inexact_decimal).next()
                        == Some(Ok(SyntaxToken::Number(SchemeNumber::Inexact(
                            number.inexact()
                        )))),
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
                SyntaxToken::lexer(&im_decimal).next()
                    == Some(Ok(SyntaxToken::Number(SchemeNumber::ExactComplex {
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
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary.is_nan());
                }
                (ExactReal::Nan { .. }, _) => {
                    let_assert!(
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary == im.inexact());
                }
                (_, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(real == number.inexact() && imaginary.is_nan());
                }
                _ => {
                    check!(
                        SyntaxToken::lexer(&inexact_im_decimal).next()
                            == Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
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

    #[track_caller]
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
                SyntaxToken::lexer(&decimal).next()
                    == Some(Ok(SyntaxToken::Number(SchemeNumber::Exact(number)))),
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
                    Some(Ok(SyntaxToken::Number(SchemeNumber::Inexact(inum)))) =
                        SyntaxToken::lexer(&inexact_decimal).next()
                );
                check!(inum.is_nan());
            } else {
                check!(
                    SyntaxToken::lexer(&inexact_decimal).next()
                        == Some(Ok(SyntaxToken::Number(SchemeNumber::Inexact(
                            number.inexact()
                        )))),
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
                SyntaxToken::lexer(&im_decimal).next()
                    == Some(Ok(SyntaxToken::Number(SchemeNumber::ExactComplex {
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
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan() && imaginary.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                (ExactReal::Nan { .. }, _) => {
                    let_assert!(
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(real.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                    check!(imaginary == im.inexact(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                (_, ExactReal::Nan { .. }) => {
                    let_assert!(
                        Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
                            real,
                            imaginary
                        }))) = SyntaxToken::lexer(&inexact_im_decimal).next()
                    );
                    check!(imaginary.is_nan(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                    check!(real == number.inexact(), "inexact (real {number:?}, im {im:?}) `{inexact_im_decimal}` does not roundtrip");
                }
                _ => {
                    check!(
                        SyntaxToken::lexer(&inexact_im_decimal).next()
                            == Some(Ok(SyntaxToken::Number(SchemeNumber::InexactComplex {
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
