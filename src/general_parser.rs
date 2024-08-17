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
    Identifier,
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
    #[error("lexer error {at:?}: {error}")]
    LexerError { error: LexerError, at: Span },
    #[error("malformed {kind:?} at {at:?}")]
    Malformed { kind: TokenKind, at: Span },
    #[error("unterminated {kind:?} starting at {index}")]
    Unterminated {
        kind: CompoundTermKind,
        index: usize,
    },
    #[error("unexpected {found:?} at {at:?}")]
    UnexpectedToken { found: TokenKind, at: Span },
    #[error("unexpected {found:?} at {at:?}: expected one of {expected:?}")]
    ExpectedToken {
        expected: Box<[TokenKind]>,
        found: TokenKind,
        at: Span,
    },
    #[error("unexpected {} at {at:?}: expected one of {expected:?}", if let Some(text) = text { format!("text `{text}`") } else { "eof".to_string() })]
    Expected {
        expected: Box<[TokenKind]>,
        text: Option<Box<str>>,
        at: Span,
    },
}

/// Lexes to produce a GAst, which is a parse result that can have 0+ errors
pub fn general_parse(source: impl AsRef<str>) -> GAst {
    use gast::SyntaxKind::*;
    use rowan::Checkpoint;

    // This stores checkpoints and allows us to complete them later
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

    const EXPECTED_TOKENS: &[TokenKind] = &[TokenKind::StartNestedComment];

    let in_nested_comment = |checkpoints: &[Checkpair]| {
        matches!(
            checkpoints.last(),
            Some((CheckpointItem::NestedComment(_), _))
        )
    };
    let in_abbreviation = |checkpoints: &[Checkpair]| {
        matches!(
            checkpoints.last(),
            Some((CheckpointItem::Abbreviation(_), _))
        )
    };
    let in_labeled = |checkpoints: &[Checkpair]| {
        matches!(checkpoints.last(), Some((CheckpointItem::Labeled(_), _)))
    };

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
            Ok(tok) => {
                builder.token(ERROR.into(), &source[span.clone()]);
                errors.push(GeneralParserError::ExpectedToken {
                    expected: Box::from(EXPECTED_TOKENS),
                    found: tok.into(),
                    at: span,
                })
            }
            Err(e) => {
                builder.token(ERROR.into(), &source[span.clone()]);
                errors.push(match e {
                    // cheat out and mark malformed as what they *might* be
                    // but if we are here, it's definitely not supposed to be here
                    LexerError::MalformedNumber => GeneralParserError::ExpectedToken {
                        expected: Box::from(EXPECTED_TOKENS),
                        found: TokenKind::Number,
                        at: span,
                    },
                    LexerError::MalformedIdentifier => GeneralParserError::ExpectedToken {
                        expected: Box::from(EXPECTED_TOKENS),
                        found: TokenKind::Identifier,
                        at: span,
                    },
                    LexerError::MalformedString => GeneralParserError::ExpectedToken {
                        expected: Box::from(EXPECTED_TOKENS),
                        found: TokenKind::String,
                        at: span,
                    },
                    _ => GeneralParserError::Expected {
                        expected: Box::from(EXPECTED_TOKENS),
                        text: Some(Box::from(&source[span.clone()])),
                        at: span,
                    },
                })
            }
        }
    }

    // Close any open checkpoints
    for (ic, start) in checkpoints.into_iter().rev() {
        let (checkpoint, skind, kind) = match ic {
            CheckpointItem::NestedComment(chkp) => {
                (chkp, NCOMMENT, CompoundTermKind::NestedComment)
            }
            CheckpointItem::DatumComment(_) => todo!(),
            CheckpointItem::Abbreviation(_) => todo!(),
            CheckpointItem::Labeled(_) => todo!(),
            CheckpointItem::List(_) => todo!(),
            CheckpointItem::Bytevector(_) => todo!(),
            CheckpointItem::Vector(_) => todo!(),
        };

        builder.start_node_at(checkpoint, skind.into());
        builder.finish_node();
        errors.push(GeneralParserError::Unterminated { kind, index: start });
    }

    builder.finish_node();

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
