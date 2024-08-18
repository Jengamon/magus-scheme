//! General parsing starts where the lexer dropped of, and handles nested syntax, while
//! also forming a GAST which is a relatively simple layer on top of a [`rowan`] CST.
use logos::Span;
use rowan::{GreenNode, GreenNodeBuilder};

use crate::lexer::{LexerError, NestedCommentToken, SyntaxToken, Token};
#[rustfmt::skip]
pub use gast::{
    GAstNode, GAstToken, MagusSyntaxElement, MagusSyntaxNode, MagusSyntaxToken,
    // trivia
    NestedComment,
    // token
    Symbol,
    // top-level
    Module,
};

mod gast;

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
    #[error("lexer error: {error}")]
    LexerError { error: LexerError, at: Span },
    #[error("malformed {kind:?}")]
    Malformed { kind: TokenKind, at: Span },
    #[error("unterminated {kind:?}")]
    Unterminated {
        kind: CompoundTermKind,
        index: usize,
        // len of the unterminated signifier token
        len: usize,
    },
    #[error("unexpected {found:?}")]
    UnexpectedToken { found: TokenKind, at: Span },
    #[error("unexpected {found:?}: expected one of {expected:?}")]
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
}

impl GeneralParserError {
    pub fn span(&self) -> Span {
        match self {
            GeneralParserError::LexerError { at, .. } => at.clone(),
            GeneralParserError::Malformed { at, .. } => at.clone(),
            GeneralParserError::Unterminated { index, len, .. } => *index..(*index + *len),
            GeneralParserError::UnexpectedToken { at, .. } => at.clone(),
            GeneralParserError::ExpectedToken { at, .. } => at.clone(),
            GeneralParserError::Expected { at, .. } => at.clone(),
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
        List(Checkpoint),
        // ditto bytevector
        Bytevector(Checkpoint),
        // ...
        Vector(Checkpoint),
    }

    // We use the lexer to give us an idea of where things are, so it is used transiently
    let source = source.as_ref();
    let lexer = Token::lexer(source);
    let mut errors = vec![];
    type Checkpair = (CheckpointItem, usize);
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
        TokenKind::DatumComment,
        TokenKind::Comment,
    ];

    // helping functions
    fn in_nested_comment(checkpoints: &[Checkpair]) -> bool {
        matches!(
            checkpoints.last(),
            Some((CheckpointItem::NestedComment(_), _))
        )
    }
    fn in_datum_comment(checkpoints: &[Checkpair]) -> bool {
        matches!(
            checkpoints.last(),
            Some((CheckpointItem::DatumComment(_), _))
        )
    }
    fn in_abbreviation(checkpoints: &[Checkpair]) -> bool {
        matches!(
            checkpoints.last(),
            Some((CheckpointItem::Abbreviation(_), _))
        )
    }
    fn in_labeled(checkpoints: &[Checkpair]) -> bool {
        matches!(checkpoints.last(), Some((CheckpointItem::Labeled(_), _)))
    }
    fn finish_datum(
        checkpoints: &mut Vec<Checkpair>,
        builder: &mut GreenNodeBuilder<'static>,
        datum_fn: impl FnOnce(&mut GreenNodeBuilder<'static>),
    ) {
        builder.start_node(DATUM.into());
        datum_fn(builder);
        builder.finish_node();

        if in_abbreviation(checkpoints) {
            let Some((CheckpointItem::Abbreviation(checkpoint), _)) = checkpoints.pop() else {
                unreachable!()
            };
            builder.start_node_at(checkpoint, ABBREV.into());
            builder.finish_node();
        } else if in_labeled(checkpoints) {
            let Some((CheckpointItem::Labeled(checkpoint), _)) = checkpoints.pop() else {
                unreachable!()
            };
            builder.start_node_at(checkpoint, LABELED.into());
            builder.finish_node();
        } else if in_datum_comment(checkpoints) {
            let Some((CheckpointItem::DatumComment(checkpoint), _)) = checkpoints.pop() else {
                unreachable!()
            };
            builder.start_node_at(checkpoint, DCOMMENT.into());
            builder.finish_node();
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
                        expected: Box::new([TokenKind::LParen]),
                        found: TokenKind::StartNestedComment,
                        at: span.clone(),
                    });
                }
                // we have to act as if the comment is valid, because this triggers the *lexer* to
                // behave differently...
                // So an erroneous nested comment vs a normal nested comment is only marked by whether the first token ERROR
                // or START_NCOMMENT
                checkpoints.push((CheckpointItem::NestedComment(checkpoint), span.start));
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
            Ok(
                Token::Syntax(t @ SyntaxToken::IntralineWhitespace)
                | Token::Syntax(t @ SyntaxToken::LineEnding),
            ) => {
                // read in as whitespace, which is only an error for 2 constructs: abbreviations and labeled
                if !in_abbreviation(&checkpoints) && !in_labeled(&checkpoints) {
                    builder.token(WHITESPACE.into(), &source[span]);
                } else {
                    // the two constructs expect the start of a datum
                    errors.push(GeneralParserError::ExpectedToken {
                        expected: Box::from([TokenKind::LParen]),
                        found: t.into(),
                        at: span,
                    })
                }
            }
            Ok(Token::Syntax(SyntaxToken::DatumComment)) => {
                let checkpoint = builder.checkpoint();
                builder.token(DCOMMENT_SYM.into(), "#;");
                checkpoints.push((CheckpointItem::DatumComment(checkpoint), span.start));
            }
            Ok(Token::Syntax(SyntaxToken::Comment)) => {
                builder.token(OLCOMMENT.into(), &source[span]);
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
                checkpoints.push((CheckpointItem::Abbreviation(checkpoint), span.start));
            }
            // datum can handle themselves and any checkpoint item that needs them
            // (and we wrote a helper to help them)
            Ok(Token::Syntax(SyntaxToken::Identifier(_))) => {
                finish_datum(&mut checkpoints, &mut builder, |builder| {
                    builder.token(SYMBOL.into(), &source[span]);
                })
            }
            Ok(Token::Syntax(SyntaxToken::Number(_))) => {
                finish_datum(&mut checkpoints, &mut builder, |builder| {
                    builder.token(NUMBER.into(), &source[span]);
                })
            }
            Ok(tok) => {
                builder.token(ERROR.into(), &source[span.clone()]);
                errors.push(GeneralParserError::ExpectedToken {
                    expected: Box::from(TOPLEVEL_EXPECTED_TOKENS),
                    found: tok.into(),
                    at: span,
                })
            }
            Err(e) => {
                builder.token(ERROR.into(), &source[span.clone()]);
                match e {
                    // cheat out and mark malformed as what they *might* be
                    LexerError::MalformedNumber => {
                        finish_datum(&mut checkpoints, &mut builder, |builder| {
                            builder.token(NUMBER.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Number,
                            at: span,
                        });
                    }
                    LexerError::MalformedIdentifier => {
                        finish_datum(&mut checkpoints, &mut builder, |builder| {
                            builder.token(SYMBOL.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::Identifier,
                            at: span,
                        });
                    }
                    LexerError::MalformedString => {
                        finish_datum(&mut checkpoints, &mut builder, |builder| {
                            builder.token(STRING.into(), &source[span.clone()]);
                        });
                        errors.push(GeneralParserError::Malformed {
                            kind: TokenKind::String,
                            at: span,
                        });
                    }
                    _ => errors.push(GeneralParserError::Expected {
                        expected: Box::from(TOPLEVEL_EXPECTED_TOKENS),
                        text: Some(Box::from(&source[span.clone()])),
                        at: span,
                    }),
                }
            }
        }
    }

    // Close any open checkpoints
    for (ic, start) in checkpoints.into_iter().rev() {
        let (checkpoint, skind, kind) = match ic {
            CheckpointItem::NestedComment(chkp) => {
                (chkp, NCOMMENT, CompoundTermKind::NestedComment)
            }
            CheckpointItem::DatumComment(chkp) => (chkp, DCOMMENT, CompoundTermKind::DatumComment),
            CheckpointItem::Abbreviation(chkp) => (chkp, ABBREV, CompoundTermKind::Abbreviation),
            CheckpointItem::Labeled(chkp) => (chkp, LABELED, CompoundTermKind::Labeled),
            CheckpointItem::List(chkp) => (chkp, LIST, CompoundTermKind::List),
            CheckpointItem::Bytevector(chkp) => (chkp, BYTEVECTOR, CompoundTermKind::Bytevector),
            CheckpointItem::Vector(chkp) => (chkp, VECTOR, CompoundTermKind::Vector),
        };

        builder.start_node_at(checkpoint, skind.into());
        builder.finish_node();
        errors.push(GeneralParserError::Unterminated {
            kind,
            index: start,
            len: 2,
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
