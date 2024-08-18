use crate::lexer::{Directive, LexerError, SyntaxToken};
use icu_casemap::CaseMapper;

/// GAst Syntax Types
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum SyntaxKind {
    // Literals
    // (which correspond to SyntaxTokens)
    L_PAREN = 0, // '('
    R_PAREN,     // ')'
    // Both the successful and malformed are put under this same syntax kind
    SYMBOL,
    NUMBER,
    STRING,
    BOOLEAN,
    CHARACTER,
    ABBREV_SYM,     // one of ' , ,@ `
    START_NCOMMENT, // #|
    END_NCOMMENT,   // |#
    DCOMMENT_SYM,   // #;
    DLABEL,         // #0= "datum label"
    DTRIGGER,       // #0# "datum trigger"
    OLCOMMENT,      // ; comment
    COMMENT,        // type used for any text *within* a comment
    DIRECTIVE,      // #!(no-)fold-case
    WHITESPACE,     // whitespaces is explicit
    ERROR,          // as well as errors

    // composite nodes
    NCOMMENT,   // #| #| nested |# comment |#
    DCOMMENT,   // #;'(datum comment)
    LIST,       // `(+ 2 3)`, `()` or `(() . x)`
    ABBREV,     // (,|'|`|,@) DATUM
    LABELED,    // DLABEL DATUM
    BYTEVECTOR, // #U8( 3 #b01 )
    VECTOR,     // #(  data is "cool")
    DATUM,      // wraps any valid datum
    ROOT,       // top-level node: a list of s-expressions
}
use rowan::SyntaxText;
use SyntaxKind::*;

/// Some boilerplate is needed, as rowan settled on using its own
/// `struct SyntaxKind(u16)` internally, instead of accepting the
/// user's `enum SyntaxKind` as a type parameter.
///
/// First, to easily pass the enum variants into rowan via `.into()`:
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MagusSchemeLang {}
impl rowan::Language for MagusSchemeLang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type MagusSyntaxNode = rowan::SyntaxNode<MagusSchemeLang>;
pub type MagusSyntaxToken = rowan::SyntaxToken<MagusSchemeLang>;
pub type MagusSyntaxElement = rowan::NodeOrToken<MagusSyntaxNode, MagusSyntaxToken>;
/// Interface for anything that is a node from the GAst
pub trait GAstNode {
    fn cast(syntax: MagusSyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &MagusSyntaxNode;
}

/// Anything that is a non-trivia individual node from the syntax
pub trait GAstToken {
    fn cast(syntax: MagusSyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &MagusSyntaxToken;
}

/// Root GAst type for a file
pub struct Module(MagusSyntaxNode);
impl Module {}
impl GAstNode for Module {
    fn cast(syntax: MagusSyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        (syntax.kind() == ROOT).then_some(Self(syntax))
    }

    fn syntax(&self) -> &MagusSyntaxNode {
        &self.0
    }
}

pub struct NestedComment(MagusSyntaxNode);
impl NestedComment {
    // does this represent a valid nested comment
    pub fn is_valid(&self) -> bool {
        // only if the very first token is START_NCOMMENT
        match self.0.first_token().map(|t| t.kind()) {
            Some(START_NCOMMENT) => true,
            Some(ERROR) => false,
            _ => unreachable!(),
        }
    }

    pub fn content(&self) -> SyntaxText {
        self.0.text()
    }
}
impl GAstNode for NestedComment {
    fn cast(syntax: MagusSyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        (syntax.kind() == NCOMMENT).then_some(Self(syntax))
    }

    fn syntax(&self) -> &MagusSyntaxNode {
        &self.0
    }
}

pub struct Symbol(MagusSyntaxToken);
impl Symbol {
    /// returns the case-folded identifier or the malformed identifier (not case-folded)
    /// this token corresponds to
    pub fn identifier(&self, case_insensitive: bool) -> Result<Box<str>, Box<str>> {
        // Look for the last directive the precedes us (if any)
        // If there is one, that directive determines our case-sensitivity
        // If there is not one, use the passed in case-insensitivity
        fn read_ident(s: &str, ci: bool, stok: &MagusSyntaxToken) -> Result<Box<str>, Box<str>> {
            let tok = SyntaxToken::lexer(s).next();
            match tok {
                Some(Ok(SyntaxToken::Identifier(id))) => Ok(if ci {
                    Box::from(CaseMapper::new().fold_string(&id).as_str())
                } else {
                    id
                }),
                // also handle invalidly lexed variants
                Some(Err(LexerError::MalformedIdentifier)) => Err(Box::from(stok.text())),
                _ => unreachable!(),
            }
        }
        let preceding_directive = self.0.parent_ancestors().find_map(|anc| {
            anc.children_with_tokens()
                .filter_map(|se| se.into_token())
                .find(|p| {
                    p.kind() == DIRECTIVE && p.text_range().end() < self.0.text_range().start()
                })
        });
        if let Some(directive) = preceding_directive {
            let ci = match SyntaxToken::lexer(directive.text()).next() {
                Some(Ok(SyntaxToken::Directive(Directive::FoldCase))) => true,
                Some(Ok(SyntaxToken::Directive(Directive::NoFoldCase))) => false,
                _ => unreachable!(),
            };
            read_ident(self.0.text(), ci, &self.0)
        } else {
            read_ident(self.0.text(), case_insensitive, &self.0)
        }
    }
}
impl GAstToken for Symbol {
    fn cast(syntax: MagusSyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        (syntax.kind() == SYMBOL).then_some(Self(syntax))
    }

    fn syntax(&self) -> &MagusSyntaxToken {
        &self.0
    }
}
