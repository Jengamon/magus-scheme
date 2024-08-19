use crate::lexer::{Directive, LexerError, SyntaxToken};
use icu_casemap::CaseMapper;

/// GAst Syntax Types
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[repr(u16)]
pub enum SyntaxKind {
    // Literals
    // (which correspond to SyntaxTokens)
    /// (
    LPAREN = 0,
    /// )
    RPAREN,
    /// .
    DOT,
    /// #u8(
    START_BYTEVECTOR,
    /// #(
    START_VECTOR,
    /// ` | , | ,@ | '
    ABBREV_SYM,
    /// #|
    START_NCOMMENT,
    /// |#
    END_NCOMMENT,
    /// #;
    DCOMMENT_SYM,
    /// #\d+=
    DLABEL,
    /// #\d+#
    DTRIGGER,
    /// ; comment
    OLCOMMENT,
    /// any kind of comment text
    COMMENT,
    /// #!(no-)?fold-case
    DIRECTIVE,
    /// any kind of inline whitespace
    WHITESPACE, // whitespaces is explicit
    /// \r | \n | \r\n
    LINEEND,
    /// a symbol literal (=identifier) or a malformed one
    SYMBOL,
    /// a number literal or a malformed one
    NUMBER,
    /// a string literal or a malformed one
    STRING,
    /// a boolean literal
    BOOLEAN,
    /// a character literal or a malformed one
    CHARACTER,
    /// syntax errors
    ERROR,

    // composite nodes
    /// #| #| nested |# comment |#
    NCOMMENT,
    /// #;'(datum comment)
    DCOMMENT,
    /// `(+ 2 3)`, `()` or `(() . x)`
    LIST,
    /// (,|'|`|,@) DATUM
    ABBREV,
    /// DLABEL DATUM
    LABELED,
    /// #U8( 3 #b01 )
    BYTEVECTOR,
    /// #(  data is "cool")
    VECTOR,
    /// wraps any valid datum
    DATUM,
    /// top-level node: a list of s-expressions
    ROOT,
}
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
pub type MagusSyntaxElementRef<'a> = rowan::NodeOrToken<&'a MagusSyntaxNode, &'a MagusSyntaxToken>;

/// Anything that is a non-terminal
pub trait GAstNode {
    fn cast(syntax: MagusSyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &MagusSyntaxNode;
}

/// Anything that is a terminal
pub trait GAstToken {
    fn cast(syntax: MagusSyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &MagusSyntaxToken;
}

macro_rules! simple_gast {
    (node $name:ident from $kind:ident) => {
        impl GAstNode for $name {
            fn cast(syntax: MagusSyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                (syntax.kind() == $kind).then_some(Self(syntax))
            }

            fn syntax(&self) -> &MagusSyntaxNode {
                &self.0
            }
        }
    };

    (node $name:ident from $kind:ident $with:expr) => {
        impl GAstNode for $name {
            fn cast(syntax: MagusSyntaxNode) -> Option<Self>
            where
                Self: Sized,
            {
                (syntax.kind() == $kind && $with(&syntax)).then_some(Self(syntax))
            }

            fn syntax(&self) -> &MagusSyntaxNode {
                &self.0
            }
        }
    };
    (token $name:ident from $kind:ident) => {
        impl GAstToken for $name {
            fn cast(syntax: MagusSyntaxToken) -> Option<Self>
            where
                Self: Sized,
            {
                (syntax.kind() == $kind).then_some(Self(syntax))
            }

            fn syntax(&self) -> &MagusSyntaxToken {
                &self.0
            }
        }
    };
}

// Cross-cutting properties are put into traits so that you can metaprogram
/// Any node that can contain comments
pub trait ContainsComments {
    fn comments(&self) -> impl Iterator<Item = Comment>;
}

pub trait ContainsDatum {
    fn datum(&self) -> impl Iterator<Item = Datum>;
}

macro_rules! contains {
    (comments $tyn:ident) => {
        impl ContainsComments for $tyn {
            fn comments(&self) -> impl Iterator<Item = Comment> {
                self.0
                    .children_with_tokens()
                    .filter_map(|elem| Comment::try_from(elem).ok())
            }
        }
    };

    (datum $tyn:ident) => {
        impl ContainsDatum for $tyn {
            fn datum(&self) -> impl Iterator<Item = Datum> {
                self.0.children().filter_map(Datum::cast)
            }
        }
    };
}

/// Root GAst type for a file
#[derive(Debug, Clone)]
pub struct Module(MagusSyntaxNode);
impl Module {}
simple_gast!(node Module from ROOT);
contains!(comments Module);
contains!(datum Module);

#[derive(Debug, Clone)]
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
}

simple_gast!(node NestedComment from NCOMMENT);
#[derive(Debug, Clone)]
pub struct OneLineComment(MagusSyntaxToken);
impl OneLineComment {}
simple_gast!(token OneLineComment from OLCOMMENT);
#[derive(Debug, Clone)]
pub struct DatumComment(MagusSyntaxNode);
impl DatumComment {}
simple_gast!(node DatumComment from DCOMMENT);

#[derive(Debug, Clone)]
pub enum Comment {
    OneLine(OneLineComment),
    Datum(DatumComment),
    Nested(NestedComment),
}
impl Comment {
    pub fn syntax(&self) -> MagusSyntaxElementRef {
        match self {
            Self::OneLine(ol) => MagusSyntaxElementRef::Token(ol.syntax()),
            Self::Datum(datum) => MagusSyntaxElementRef::Node(datum.syntax()),
            Self::Nested(nest) => MagusSyntaxElementRef::Node(nest.syntax()),
        }
    }
}
impl TryFrom<MagusSyntaxElement> for Comment {
    type Error = ();
    fn try_from(value: MagusSyntaxElement) -> Result<Self, Self::Error> {
        match value {
            MagusSyntaxElement::Node(n) => match n.kind() {
                NCOMMENT => NestedComment::cast(n).map(Comment::Nested).ok_or(()),
                DCOMMENT => DatumComment::cast(n).map(Comment::Datum).ok_or(()),
                _ => Err(()),
            },
            MagusSyntaxElement::Token(t) => match t.kind() {
                OLCOMMENT => OneLineComment::cast(t).map(Comment::OneLine).ok_or(()),
                _ => Err(()),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Datum(MagusSyntaxNode);
impl Datum {
    pub fn as_list(&self) -> Option<List> {
        // we know there's exactly one child, so we only have to
        // check nodes if we are looking for a list!
        match self.0.children().next() {
            Some(node) if node.kind() == LIST => List::cast(node),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self.0.children_with_tokens().next() {
            Some(MagusSyntaxElement::Token(token)) if token.kind() == SYMBOL => Symbol::cast(token),
            _ => None,
        }
    }
}
// *all* validly parsed datum only contain 1 child
simple_gast!(node Datum from DATUM |syntax: &MagusSyntaxNode| {
    syntax.children_with_tokens().count() == 1
});
/*
impl TryFrom<MagusSyntaxNode> for Datum {
    type Error = ();
    fn try_from(value: MagusSyntaxNode) -> Result<Self, Self::Error> {
        match value.children_with_tokens().first() {
            Some(MagusSyntaxElement::Node(n)) => match n.kind() {
                LIST => List::cast(n).map(Datum::List).ok_or(()),
                _ => Err(())
            }
            Some(MagusSyntaxElement::Token(tok)) => match tok.kind() {
                SYMBOL => Symbol::cast(s).map(Datum::Symbol).ok_or(()),
                _ => Err(())
            }
        }
    }
}
*/

#[derive(Debug, Clone)]
pub struct List(MagusSyntaxNode);
impl List {}
simple_gast!(node List from LIST);
contains!(comments List);
contains!(datum List);

#[derive(Debug, Clone)]
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
                .filter(|t| {
                    t.text_range().end() < self.0.text_range().start() && t.kind() == DIRECTIVE
                })
                .collect::<Vec<_>>()
                .into_iter()
                .next_back()
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
simple_gast!(token Symbol from SYMBOL);
