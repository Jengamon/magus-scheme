//! General parsing starts where the lexer dropped of, and handles nested syntax, while
//! also forming a GAST which is a relatively simple layer on top of a [`rowan`] CST.
use gast::MagusSyntaxNode;
use logos::Span;
use rowan::{GreenNode, GreenNodeBuilder};

use crate::{
    lexer::{LexerError, NestedCommentToken, SyntaxToken, Token},
    ExactReal, SchemeNumber,
};
pub mod gast;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenKind {
    IntralineWhitespace,
    LineEnding,
    Comment,
    StartNestedComment,
    EndNestedComment,
    CommentText,
    DatumComment,
    Directive,
    LParen,
    RParen,
    Dot,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    StartVector,
    StartBytevector,
    Identifier,
    Boolean,
    Character,
    String,
    Number,
    DatumLabel,
    DatumLabelValue,
}

impl From<SyntaxToken> for TokenKind {
    fn from(st: SyntaxToken) -> Self {
        match st {
            SyntaxToken::IntralineWhitespace => TokenKind::IntralineWhitespace,
            SyntaxToken::LineEnding => TokenKind::LineEnding,
            SyntaxToken::Comment => TokenKind::Comment,
            SyntaxToken::StartNestedComment => TokenKind::StartNestedComment,
            SyntaxToken::EndNestedComment => TokenKind::EndNestedComment,
            SyntaxToken::DatumComment => TokenKind::DatumComment,
            SyntaxToken::Directive(_) => TokenKind::Directive,
            SyntaxToken::LParen => TokenKind::LParen,
            SyntaxToken::RParen => TokenKind::RParen,
            SyntaxToken::Dot => TokenKind::Dot,
            SyntaxToken::Quote => TokenKind::Quote,
            SyntaxToken::Quasiquote => TokenKind::Quasiquote,
            SyntaxToken::Unquote => TokenKind::Unquote,
            SyntaxToken::UnquoteSplicing => TokenKind::UnquoteSplicing,
            SyntaxToken::StartVector => TokenKind::StartVector,
            SyntaxToken::StartBytevector => TokenKind::StartBytevector,
            SyntaxToken::Identifier(_) => TokenKind::Identifier,
            SyntaxToken::Boolean(_) => TokenKind::Boolean,
            SyntaxToken::Character(_) => TokenKind::Character,
            SyntaxToken::String(_) => TokenKind::String,
            SyntaxToken::Number(_) => TokenKind::Number,
            SyntaxToken::DatumLabel(_) => TokenKind::DatumLabel,
            SyntaxToken::DatumLabelValue(_) => TokenKind::DatumLabelValue,
        }
    }
}

impl From<NestedCommentToken> for TokenKind {
    fn from(nct: NestedCommentToken) -> Self {
        match nct {
            NestedCommentToken::StartNestedComment => TokenKind::StartNestedComment,
            NestedCommentToken::EndNestedComment => TokenKind::EndNestedComment,
            NestedCommentToken::CommentText => TokenKind::CommentText,
        }
    }
}

impl From<Token> for TokenKind {
    fn from(value: Token) -> Self {
        match value {
            Token::Syntax(st) => st.into(),
            Token::NestedComment(nct) => nct.into(),
        }
    }
}

#[derive(Debug)]
pub enum CompoundTermKind {
    NestedComment,
    DatumComment,
    Abbreviation,
    Labeled,
    List,
    Bytevector,
    Vector,
}

#[derive(Debug, thiserror::Error)]
pub enum GeneralParserError {
    #[error("malformed {kind:?}")]
    Malformed { kind: TokenKind, at: Span },
    #[error("malformed list")]
    MalformedList { at: Span },
    #[error("unterminated {kind:?}")]
    Unterminated {
        kind: CompoundTermKind,
        index: usize,
        // len of the unterminated signifier token
        len: usize,
    },
    #[error("unexpected {found:?}")]
    UnexpectedToken { found: TokenKind, at: Span },
    #[error("unexpected {found:?}: expected {}", 
        if expected.len() == 1 {
            format!("{:?}", expected[0])
        } else {
            format!("one of {expected:?}") 
        }
    )]
    ExpectedToken {
        expected: Box<[TokenKind]>,
        found: TokenKind,
        at: Span,
    },
    #[error("unexpected {}: expected one of {expected:?}", if let Some(text) = text { format!("text `{text}`") } else { "eof".to_string() })]
    Expected {
        expected: Box<[TokenKind]>,
        text: Option<Box<str>>,
        at: Span,
    },
    #[error("number out of byte range")]
    ByteOutOfRange { at: Span },
}

impl GeneralParserError {
    pub fn span(&self) -> Span {
        match self {
            GeneralParserError::Malformed { at, .. } => at.clone(),
            GeneralParserError::MalformedList { at } => at.clone(),
            GeneralParserError::Unterminated { index, len, .. } => *index..(*index + *len),
            GeneralParserError::UnexpectedToken { at, .. } => at.clone(),
            GeneralParserError::ExpectedToken { at, .. } => at.clone(),
            GeneralParserError::Expected { at, .. } => at.clone(),
            GeneralParserError::ByteOutOfRange { at } => at.clone(),
        }
    }
}

/// Lexes to produce a GAst, which is a parse result that can have 0+ errors
pub fn general_parse(source: impl AsRef<str>) -> GAst {
    use gast::SyntaxKind::*;
    use rowan::Checkpoint;

    // This stores checkpoints and allows us to complete them later
    #[derive(Debug)]
    enum CheckpointItem {
        // We have started a nested comment
        NestedComment(Checkpoint),
        // We have started a datum comment
        DatumComment(Checkpoint),
        // Start an abbreviation
        Abbreviation(Checkpoint),
        // Start labeled datum
        Labeled(Checkpoint),
        // We have started a list
        // how many elements after a dot was encountered?
        List(Checkpoint, usize, Option<usize>),
        // ditto bytevector
        Bytevector(Checkpoint),
        // ...
        Vector(Checkpoint),
    }

    // We use the lexer to give us an idea of where things are, so it is used transiently
    let source = source.as_ref();
    let lexer = Token::lexer(source);
    let mut errors = vec![];
    type Checkpair = (CheckpointItem, core::ops::Range<usize>);
    let mut checkpoints = Vec::<Checkpair>::new();
    let mut builder = GreenNodeBuilder::new();

    const TOPLEVEL_EXPECTED_TOKENS: &[TokenKind] = &[
        TokenKind::StartNestedComment,
        TokenKind::Quote,
        TokenKind::Quasiquote,
        TokenKind::Unquote,
        TokenKind::UnquoteSplicing,
        TokenKind::Identifier,
        TokenKind::Number,
        TokenKind::String,
        TokenKind::Character,
        TokenKind::Boolean,
        TokenKind::DatumComment,
        TokenKind::Comment,
        TokenKind::DatumLabel,
        TokenKind::DatumLabelValue,
    ];

    const DATUM_EXPECTED_TOKENS: &[TokenKind] = &[
        TokenKind::Quote,
        TokenKind::Quasiquote,
        TokenKind::Unquote,
        TokenKind::UnquoteSplicing,
        TokenKind::Identifier,
        TokenKind::Number,
        TokenKind::LParen,
        TokenKind::StartVector,
        TokenKind::StartBytevector,
    ];

    // helping functions
    macro_rules! check_if_in {
        ($fn_name:ident for $item:ident) => {
            fn $fn_name(checkpoints: &[Checkpair]) -> bool {
                matches!(checkpoints.last(), Some((CheckpointItem::$item(_), _)))
            }
        };
    }
    check_if_in!(in_nested_comment for NestedComment);
    check_if_in!(in_datum_comment for DatumComment);
    check_if_in!(in_abbreviation for Abbreviation);
    check_if_in!(in_labeled for Labeled);
    fn in_list(checkpoints: &[Checkpair]) -> bool {
        matches!(checkpoints.last(), Some((CheckpointItem::List(_, _, _), _)))
    }
    check_if_in!(in_vector for Vector);
    check_if_in!(in_bytevector for Bytevector);

    fn finish_datum(
        checkpoints: &mut Vec<Checkpair>,
        builder: &mut GreenNodeBuilder<'static>,
        datum_checkpoint: Option<Checkpoint>,
        datum_fn: impl FnOnce(&mut GreenNodeBuilder<'static>),
    ) {
        if datum_checkpoint.is_none() {
            builder.start_node(DATUM.into());
        }
        datum_fn(builder);
        if let Some(checkpoint) = datum_checkpoint {
            builder.start_node_at(checkpoint, DATUM.into());
        }
        builder.finish_node();

        while in_abbreviation(checkpoints) || in_labeled(checkpoints) {
            let (checkpoint, is_labeled) = match checkpoints.pop() {
                Some((CheckpointItem::Abbreviation(checkpoint), _)) => (checkpoint, false),
                Some((CheckpointItem::Labeled(checkpoint), _)) => (checkpoint, true),
                _ => unreachable!(),
            };
            builder.start_node_at(checkpoint, DATUM.into());
            builder.start_node_at(checkpoint, if is_labeled { LABELED } else { ABBREV }.into());
            builder.finish_node();
            builder.finish_node();
        }

        if in_datum_comment(checkpoints) {
            let Some((CheckpointItem::DatumComment(checkpoint), _)) = checkpoints.pop() else {
                unreachable!()
            };
            builder.start_node_at(checkpoint, DCOMMENT.into());
            builder.finish_node();
        }

        // lists with a dot need to know how many datum have been completed
        if in_list(checkpoints) {
            let Some((CheckpointItem::List(checkpoint, pre_dot, post_dot), span)) =
                checkpoints.pop()
            else {
                unreachable!()
            };
            let (pre_dot, post_dot) = if let Some(prev) = post_dot {
                (pre_dot, Some(prev + 1))
            } else {
                (pre_dot + 1, None)
            };
            checkpoints.push((CheckpointItem::List(checkpoint, pre_dot, post_dot), span));
        }
    }

    builder.start_node(ROOT.into());

    for (elem, span) in lexer {
        match elem {
            // Nested comment
            Ok(
                Token::Syntax(SyntaxToken::StartNestedComment)
                | Token::NestedComment(NestedCommentToken::StartNestedComment),
            ) => {
                let checkpoint = builder.checkpoint();
                if !in_labeled(&checkpoints) && !in_abbreviation(&checkpoints) {
                    builder.token(START_NCOMMENT.into(), "#|");
                } else {
                    builder.token(ERROR.into(), "#|");
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from(DATUM_EXPECTED_TOKENS),
                        found: TokenKind::StartNestedComment,
                        at: span.clone(),
                    });
                }
                // we have to act as if the comment is valid, because this triggers the *lexer* to
                // behave differently...
                // So an erroneous nested comment vs a normal nested comment is only marked by whether the first token ERROR
                // or START_NCOMMENT
                checkpoints.push((CheckpointItem::NestedComment(checkpoint), span.clone()));
            }
            Ok(
                Token::Syntax(SyntaxToken::EndNestedComment)
                | Token::NestedComment(NestedCommentToken::EndNestedComment),
            ) => {
                if in_nested_comment(&checkpoints) {
                    // end the top level comment
                    builder.token(END_NCOMMENT.into(), "|#");
                    let Some((CheckpointItem::NestedComment(checkpoint), _)) = checkpoints.pop()
                    else {
                        unreachable!()
                    };
                    builder.start_node_at(checkpoint, NCOMMENT.into());
                    builder.finish_node();
                } else {
                    builder.token(ERROR.into(), "|#");
                    errors.push(GeneralParserError::UnexpectedToken {
                        found: TokenKind::EndNestedComment,
                        at: span,
                    });
                }
            }
            Ok(Token::NestedComment(NestedCommentToken::CommentText))
                if in_nested_comment(&checkpoints) =>
            {
                builder.token(COMMENT.into(), &source[span]);
            }
            Ok(Token::NestedComment(NestedCommentToken::CommentText)) => {
                unreachable!("should not be produced outside of a nested comment")
            }
            Ok(Token::Syntax(SyntaxToken::IntralineWhitespace)) => {
                // read in as whitespace, which is only an error for 2 constructs: abbreviations and labeled
                if in_abbreviation(&checkpoints) || in_labeled(&checkpoints) {
                    // the two constructs expect the start of a datum
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from(DATUM_EXPECTED_TOKENS),
                        found: TokenKind::IntralineWhitespace,
                        at: span.clone(),
                    })
                }

                builder.token(WHITESPACE.into(), &source[span]);
            }
            Ok(Token::Syntax(SyntaxToken::LineEnding)) => {
                // read in as whitespace, which is only an error for 2 constructs: abbreviations and labeled
                if in_abbreviation(&checkpoints) || in_labeled(&checkpoints) {
                    // the two constructs expect the start of a datum
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from(DATUM_EXPECTED_TOKENS),
                        found: TokenKind::LineEnding,
                        at: span.clone(),
                    })
                }

                builder.token(LINEEND.into(), &source[span]);
            }
            Ok(Token::Syntax(SyntaxToken::DatumComment)) => {
                let checkpoint = builder.checkpoint();
                builder.token(DCOMMENT_SYM.into(), "#;");
                checkpoints.push((CheckpointItem::DatumComment(checkpoint), span.clone()));
            }
            Ok(Token::Syntax(SyntaxToken::Comment)) => {
                if in_abbreviation(&checkpoints) || in_labeled(&checkpoints) {
                    // the two constructs expect the start of a datum
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from(DATUM_EXPECTED_TOKENS),
                        found: TokenKind::Comment,
                        at: span.clone(),
                    })
                }

                builder.token(OLCOMMENT.into(), &source[span]);
            }
            Ok(Token::Syntax(SyntaxToken::Directive(_))) => {
                if in_abbreviation(&checkpoints) || in_labeled(&checkpoints) {
                    // the two constructs expect the start of a datum
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from(DATUM_EXPECTED_TOKENS),
                        found: TokenKind::Directive,
                        at: span.clone(),
                    })
                }

                builder.token(DIRECTIVE.into(), &source[span]);
            }
            // Since the lexer keeps track of whether we are in a nested comment or not
            // we can just handled the other nodes as if they *aren't* in a comment
            Ok(Token::Syntax(
                SyntaxToken::Quote
                | SyntaxToken::Quasiquote
                | SyntaxToken::Unquote
                | SyntaxToken::UnquoteSplicing,
            )) => {
                let checkpoint = builder.checkpoint();
                // read the symbol token as ABBREV_SYM
                builder.token(ABBREV_SYM.into(), &source[span.clone()]);
                checkpoints.push((CheckpointItem::Abbreviation(checkpoint), span.clone()));
            }
            Ok(Token::Syntax(SyntaxToken::DatumLabel(_))) => {
                let checkpoint = builder.checkpoint();
                builder.token(DLABEL.into(), &source[span.clone()]);
                checkpoints.push((CheckpointItem::Labeled(checkpoint), span.clone()));
            }
            // datum can handle themselves and any checkpoint item that needs them
            // (and we wrote a helper to help them)
            Ok(Token::Syntax(SyntaxToken::Identifier(_))) => {
                if in_bytevector(&checkpoints) {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::Identifier,
                        at: span.clone(),
                    })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(SYMBOL.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::Number(n))) => {
                if in_bytevector(&checkpoints)
                    && !matches!(
                        n,
                        SchemeNumber::Exact(ExactReal::Integer {
                            value: 0..=255,
                            is_neg: false
                        })
                    )
                {
                    errors.push(GeneralParserError::ByteOutOfRange { at: span.clone() })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(NUMBER.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::Boolean(_))) => {
                if in_bytevector(&checkpoints) {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::Boolean,
                        at: span.clone(),
                    })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(BOOLEAN.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::String(_))) => {
                if in_bytevector(&checkpoints) {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::String,
                        at: span.clone(),
                    })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(STRING.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::Character(_))) => {
                if in_bytevector(&checkpoints) {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::Character,
                        at: span.clone(),
                    })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(CHARACTER.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::DatumLabelValue(_))) => {
                if in_bytevector(&checkpoints) {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::DatumLabelValue,
                        at: span.clone(),
                    })
                }

                finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                    builder.token(DTRIGGER.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::LParen)) => {
                let checkpoint = builder.checkpoint();
                builder.token(LPAREN.into(), "(");

                if !in_bytevector(&checkpoints) {
                    checkpoints.push((CheckpointItem::List(checkpoint, 0, None), span.clone()));
                } else {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::LParen,
                        at: span,
                    })
                }
            }
            Ok(Token::Syntax(SyntaxToken::Dot)) if in_list(&checkpoints) => {
                let Some((CheckpointItem::List(checkpoint, pre_dot, post_dot), lspan)) =
                    checkpoints.pop()
                else {
                    unreachable!()
                };

                let post_dot = if post_dot.is_none() {
                    Some(0)
                } else {
                    errors.push(GeneralParserError::UnexpectedToken {
                        found: TokenKind::Dot,
                        at: span,
                    });
                    post_dot
                };

                builder.token(DOT.into(), ".");
                checkpoints.push((CheckpointItem::List(checkpoint, pre_dot, post_dot), lspan));
            }
            Ok(Token::Syntax(SyntaxToken::Dot)) => {
                builder.token(DOT.into(), ".");
                errors.push(GeneralParserError::UnexpectedToken {
                    found: TokenKind::Dot,
                    at: span,
                });
            }
            Ok(Token::Syntax(SyntaxToken::StartBytevector)) => {
                let checkpoint = builder.checkpoint();
                builder.token(START_BYTEVECTOR.into(), &source[span.clone()]);
                checkpoints.push((CheckpointItem::Bytevector(checkpoint), span.clone()));
            }
            Ok(Token::Syntax(SyntaxToken::StartVector)) => {
                let checkpoint = builder.checkpoint();
                builder.token(START_VECTOR.into(), &source[span.clone()]);

                if !in_bytevector(&checkpoints) {
                    checkpoints.push((CheckpointItem::Vector(checkpoint), span.clone()));
                } else {
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::Number]),
                        found: TokenKind::StartVector,
                        at: span,
                    })
                }
            }
            Ok(Token::Syntax(SyntaxToken::RParen)) => {
                enum Paired {
                    List,
                    Vector,
                    Bytevector,
                }
                // complete the correct type as a datum
                let pair_data = if in_list(&checkpoints) {
                    let Some((CheckpointItem::List(checkpoint, pre_dot, post_dot), lspan)) =
                        checkpoints.pop()
                    else {
                        unreachable!()
                    };
                    // if there is a dot, check to make sure it matches
                    // ( <datum>+ . <datum> )
                    if let Some(post_dot) = post_dot {
                        if post_dot != 1 || pre_dot == 0 {
                            errors.push(GeneralParserError::MalformedList {
                                at: lspan.start..span.end,
                            })
                        }
                    }
                    Some((checkpoint, Paired::List))
                } else if in_vector(&checkpoints) {
                    let Some((CheckpointItem::Vector(checkpoint), _)) = checkpoints.pop() else {
                        unreachable!()
                    };
                    Some((checkpoint, Paired::Vector))
                } else if in_bytevector(&checkpoints) {
                    let Some((CheckpointItem::Bytevector(checkpoint), _)) = checkpoints.pop()
                    else {
                        unreachable!()
                    };
                    Some((checkpoint, Paired::Bytevector))
                } else {
                    errors.push(GeneralParserError::UnexpectedToken {
                        found: TokenKind::RParen,
                        at: span.clone(),
                    });
                    None
                };

                finish_datum(
                    &mut checkpoints,
                    &mut builder,
                    pair_data.as_ref().map(|(ckp, _)| ckp).cloned(),
                    |builder| {
                        builder.token(RPAREN.into(), ")");

                        if let Some((checkpoint, paired)) = pair_data {
                            match paired {
                                Paired::List => {
                                    builder.start_node_at(checkpoint, LIST.into());
                                    builder.finish_node();
                                }
                                Paired::Vector => {
                                    builder.start_node_at(checkpoint, VECTOR.into());
                                    builder.finish_node();
                                }
                                Paired::Bytevector => {
                                    builder.start_node_at(checkpoint, BYTEVECTOR.into());
                                    builder.finish_node();
                                }
                            }
                        }
                    },
                );
            }
            Err(e) => {
                // mark malformed as what they *might* be
                // only if we have no idea do we mark something as ERROR
                match e {
                    LexerError::MalformedNumber | LexerError::NumberTooBig => {
                        finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                            builder.token(NUMBER.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Number,
                            at: span,
                        });
                    }
                    LexerError::MalformedIdentifier => {
                        finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                            builder.token(SYMBOL.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Identifier,
                            at: span,
                        });
                    }
                    LexerError::CharacterTooBig
                    | LexerError::MalformedCharacter
                    | LexerError::InvalidCodepoint(_)
                    | LexerError::InvalidCharacterName(_) => {
                        finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                            builder.token(CHARACTER.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Character,
                            at: span,
                        });
                    }
                    LexerError::MalformedString => {
                        finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                            builder.token(STRING.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::String,
                            at: span,
                        });
                    }
                    LexerError::InvalidDirective(_) => {
                        builder.token(DIRECTIVE.into(), &source[span.clone()]);
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Directive,
                            at: span,
                        });
                    }
                    LexerError::LabelTooBig => {
                        builder.token(DLABEL.into(), &source[span.clone()]);
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::DatumLabel,
                            at: span,
                        });
                    }
                    LexerError::TriggerTooBig => {
                        finish_datum(&mut checkpoints, &mut builder, None, |builder| {
                            builder.token(DTRIGGER.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::DatumLabelValue,
                            at: span,
                        });
                    }
                    LexerError::Invalid => {
                        builder.token(ERROR.into(), &source[span.clone()]);
                        errors.push(GeneralParserError::Expected {
                            expected: Box::from(TOPLEVEL_EXPECTED_TOKENS),
                            text: Some(Box::from(&source[span.clone()])),
                            at: span,
                        });
                    }
                }
            }
        }
    }

    // Close any open checkpoints
    for (ic, span) in checkpoints.into_iter().rev() {
        let (checkpoint, skind, kind) = match ic {
            CheckpointItem::NestedComment(chkp) => {
                (chkp, NCOMMENT, CompoundTermKind::NestedComment)
            }
            CheckpointItem::DatumComment(chkp) => (chkp, DCOMMENT, CompoundTermKind::DatumComment),
            CheckpointItem::Abbreviation(chkp) => (chkp, ABBREV, CompoundTermKind::Abbreviation),
            CheckpointItem::Labeled(chkp) => (chkp, LABELED, CompoundTermKind::Labeled),
            CheckpointItem::List(chkp, _, _) => (chkp, LIST, CompoundTermKind::List),
            CheckpointItem::Bytevector(chkp) => (chkp, BYTEVECTOR, CompoundTermKind::Bytevector),
            CheckpointItem::Vector(chkp) => (chkp, VECTOR, CompoundTermKind::Vector),
        };

        builder.start_node_at(checkpoint, skind.into());
        builder.finish_node();
        errors.push(GeneralParserError::Unterminated {
            kind,
            index: span.start,
            len: span.end - span.start,
        });
    }

    builder.finish_node();

    // Sort errors into text order
    // (for codesnake)
    errors.sort_unstable_by_key(|err| err.span().start);

    GAst {
        green_node: builder.finish(),
        errors,
    }
}

// Using rowan and CST should (and hopefully does) give us a nice property that
// calculating lines should be possible (as it is a full fidelity structure)

/// General parser parse result
#[derive(Debug)]
pub struct GAst {
    green_node: GreenNode,
    errors: Vec<GeneralParserError>,
}

impl GAst {
    pub fn syntax(&self) -> MagusSyntaxNode {
        MagusSyntaxNode::new_root(self.green_node.clone())
    }

    pub fn errors(&self) -> &[GeneralParserError] {
        &self.errors
    }

    pub fn into_errors(self) -> Vec<GeneralParserError> {
        self.errors
    }
}

#[cfg(test)]
mod tests {
    use icu_casemap::CaseMapper;

    #[test]
    fn test_casemapping() {
        let mixture = "aΛ";
        let mixture2 = "Aλ";
        let cm = CaseMapper::new();

        // locale-independant
        assert!(cm.fold_string(mixture) == cm.fold_string(mixture2));

        // When #!fold-case, we pass all identifiers and character names into the case-folding
        // algorithm
    }
}
