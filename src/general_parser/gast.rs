//! The types of this module make the results of the general parser more
//! accessible by providing a strongly-typed layer on top of the CST produced
//! by the parser.
use core::fmt;
use std::collections::HashSet;

use crate::{
    lexer::{Directive, SyntaxToken},
    ExactReal, SchemeNumber,
};
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
pub trait ContainsTrivia {
    fn comments(&self) -> impl Iterator<Item = Comment>;
    fn whitespace(&self) -> impl Iterator<Item = Whitespace>;
    fn directives(&self) -> impl Iterator<Item = DirectiveToken>;
}

/// Any node that can contain datum
pub trait ContainsDatum {
    fn datum(&self) -> impl Iterator<Item = Datum>;
}

macro_rules! contains {
    (trivia $tyn:ident) => {
        impl ContainsTrivia for $tyn {
            fn comments(&self) -> impl Iterator<Item = Comment> {
                self.0
                    .children_with_tokens()
                    .filter_map(|elem| Comment::try_from(elem).ok())
            }

            fn whitespace(&self) -> impl Iterator<Item = Whitespace> {
                self.0.children_with_tokens().filter_map(|elem| match elem {
                    MagusSyntaxElement::Token(tok) => Whitespace::try_from(tok).ok(),
                    _ => None,
                })
            }

            fn directives(&self) -> impl Iterator<Item = DirectiveToken> {
                self.0.children_with_tokens().filter_map(|elem| match elem {
                    MagusSyntaxElement::Token(tok) => DirectiveToken::cast(tok),
                    _ => None,
                })
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

/// An interface for recursively operating on datum
pub trait DatumVisitor {
    fn visit_datum(&mut self, datum: &Datum) {
        // always using the right kind guarantees that we can convert the node
        // (individual node types can represent invalid data, but the *node type* is
        // still correct)
        if let Some(kind) = datum.kind() {
            match kind {
                DatumKind::List => self.visit_list(&datum.as_list().unwrap()),
                DatumKind::Vector => self.visit_vector(&datum.as_vector().unwrap()),
                DatumKind::Labeled => self.visit_labeled(&datum.as_labeled().unwrap()),
                DatumKind::Symbol => self.visit_symbol(&datum.as_symbol().unwrap()),
                DatumKind::LabelRef => self.visit_label_ref(&datum.as_label_ref().unwrap()),
                DatumKind::Abbreviation => {
                    self.visit_abbreviation(&datum.as_abbreviation().unwrap())
                }
                DatumKind::Number => self.visit_number(&datum.as_number().unwrap()),
                DatumKind::Bytevector => self.visit_bytevector(&datum.as_bytevector().unwrap()),
                DatumKind::StringToken => self.visit_string(&datum.as_string().unwrap()),
                DatumKind::Character => self.visit_char(&datum.as_char().unwrap()),
                DatumKind::Boolean => self.visit_bool(&datum.as_bool().unwrap()),
            }
        }
    }

    // meta-programming FTW!
    fn visit_composite<C: ContainsDatum>(&mut self, composite: &C) {
        for datum in composite.datum() {
            self.visit_datum(&datum);
        }
    }

    fn visit_list(&mut self, list: &List) {
        _ = list;
    }

    fn visit_bytevector(&mut self, bytevector: &Bytevector) {
        _ = bytevector;
    }

    fn visit_vector(&mut self, vector: &Vector) {
        _ = vector;
    }

    fn visit_labeled(&mut self, labeled: &LabeledDatum) {
        _ = labeled;
    }

    fn visit_abbreviation(&mut self, abbreviation: &Abbreviation) {
        _ = abbreviation;
    }

    fn visit_symbol(&mut self, symbol: &Symbol) {
        _ = symbol;
    }

    fn visit_number(&mut self, number: &Number) {
        _ = number;
    }

    fn visit_string(&mut self, string: &StringToken) {
        _ = string;
    }

    fn visit_char(&mut self, char: &Character) {
        _ = char;
    }

    fn visit_bool(&mut self, bool: &Boolean) {
        _ = bool;
    }

    fn visit_label_ref(&mut self, label_ref: &LabelRef) {
        _ = label_ref;
    }
}

/// Root GAst type for a file
#[derive(Debug, Clone)]
pub struct Module(MagusSyntaxNode);
simple_gast!(node Module from ROOT);
contains!(trivia Module);
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
pub struct InlineWhitespace(MagusSyntaxToken);
simple_gast!(token InlineWhitespace from WHITESPACE);
#[derive(Debug, Clone)]
pub struct LineEnding(MagusSyntaxToken);
simple_gast!(token LineEnding from LINEEND);
pub enum Whitespace {
    Inline(InlineWhitespace),
    LineEnding(LineEnding),
}
impl GAstToken for Whitespace {
    fn cast(syntax: MagusSyntaxToken) -> Option<Self>
    where
        Self: Sized,
    {
        Self::try_from(syntax).ok()
    }

    fn syntax(&self) -> &MagusSyntaxToken {
        match self {
            Self::Inline(inline) => inline.syntax(),
            Self::LineEnding(line_end) => line_end.syntax(),
        }
    }
}
impl TryFrom<MagusSyntaxToken> for Whitespace {
    type Error = ();
    fn try_from(value: MagusSyntaxToken) -> Result<Self, Self::Error> {
        match value.kind() {
            WHITESPACE => Ok(Whitespace::Inline(InlineWhitespace::cast(value).ok_or(())?)),
            LINEEND => Ok(Whitespace::LineEnding(LineEnding::cast(value).ok_or(())?)),
            _ => Err(()),
        }
    }
}
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

#[derive(Default)]
struct ContainedTriggerVisitor {
    triggers: HashSet<usize>,
}

impl DatumVisitor for ContainedTriggerVisitor {
    // we don't check bytevectors, as they can only validly contain bytes

    fn visit_list(&mut self, list: &List) {
        self.visit_composite(list);
    }

    fn visit_vector(&mut self, vector: &Vector) {
        self.visit_composite(vector);
    }

    fn visit_labeled(&mut self, labeled: &LabeledDatum) {
        self.visit_composite(labeled);
    }

    fn visit_abbreviation(&mut self, abbreviation: &Abbreviation) {
        self.visit_composite(abbreviation);
    }

    fn visit_label_ref(&mut self, label_ref: &LabelRef) {
        if let Some(trigger) = label_ref.trigger() {
            self.triggers.insert(trigger);
        }
    }
}

#[derive(Debug, Clone)]
pub struct LabeledDatum(MagusSyntaxNode);
impl LabeledDatum {
    pub fn label(&self) -> Option<usize> {
        if let Some(Ok(SyntaxToken::DatumLabel(label))) = self
            .0
            .children_with_tokens()
            .filter_map(MagusSyntaxElement::into_token)
            .find(|tok| tok.kind() == DLABEL)
            .and_then(|tok| SyntaxToken::lexer(tok.text()).next())
        {
            Some(label)
        } else {
            None
        }
    }

    // Is there a subexpression that contains a reference to us?
    pub fn is_circular(&self) -> bool {
        let Some(label) = self.label() else {
            return false;
        };

        let mut contained_triggers = ContainedTriggerVisitor::default();
        contained_triggers.visit_labeled(self);

        contained_triggers.triggers.contains(&label)
    }
}
simple_gast!(node LabeledDatum from LABELED);
// we should contain only 1 datum...
contains!(datum LabeledDatum);
// NOTE Only erroneous labeled datum contain trivia!!!
contains!(trivia LabeledDatum);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbbreviationKind {
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
}
impl fmt::Display for AbbreviationKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbbreviationKind::Quote => write!(f, "quote"),
            AbbreviationKind::Quasiquote => write!(f, "quasiquote"),
            AbbreviationKind::Unquote => write!(f, "unquote"),
            AbbreviationKind::UnquoteSplicing => write!(f, "unquote-splicing"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Abbreviation(MagusSyntaxNode);
impl Abbreviation {
    pub fn kind(&self) -> Option<AbbreviationKind> {
        let abbrev_sym = self.0.children_with_tokens().find_map(|elem| match elem {
            MagusSyntaxElement::Token(tok) => {
                if tok.kind() == ABBREV_SYM {
                    Some(tok)
                } else {
                    None
                }
            }
            _ => None,
        });

        if let Some(symtok) = abbrev_sym {
            match symtok.text() {
                "'" => Some(AbbreviationKind::Quote),
                "`" => Some(AbbreviationKind::Quasiquote),
                "," => Some(AbbreviationKind::Unquote),
                ",@" => Some(AbbreviationKind::UnquoteSplicing),
                _ => None,
            }
        } else {
            None
        }
    }
}
simple_gast!(node Abbreviation from ABBREV);
contains!(datum Abbreviation);
// NOTE Only erroneous abbreviations contain trivia!!!
contains!(trivia Abbreviation);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DatumKind {
    List,
    Bytevector,
    Vector,
    Labeled,
    Abbreviation,
    Symbol,
    Number,
    StringToken,
    Character,
    Boolean,
    LabelRef,
}

#[derive(Debug, Clone)]
pub struct Datum(MagusSyntaxNode);
impl Datum {
    // If this says `Some`, calling the correct as_* and unwrapping must never panic
    pub fn kind(&self) -> Option<DatumKind> {
        match self.0.children_with_tokens().next() {
            None => None,
            Some(MagusSyntaxElement::Node(node)) => match node.kind() {
                LIST => Some(DatumKind::List),
                BYTEVECTOR => Some(DatumKind::Bytevector),
                VECTOR => Some(DatumKind::Vector),
                LABELED => Some(DatumKind::Labeled),
                ABBREV => Some(DatumKind::Abbreviation),
                _ => None,
            },
            Some(MagusSyntaxElement::Token(tok)) => match tok.kind() {
                SYMBOL => Some(DatumKind::Symbol),
                DTRIGGER => Some(DatumKind::LabelRef),
                NUMBER => Some(DatumKind::Number),
                STRING => Some(DatumKind::StringToken),
                CHARACTER => Some(DatumKind::Character),
                BOOLEAN => Some(DatumKind::Boolean),
                _ => None,
            },
        }
    }
}
// *all* validly parsed datum only contain 1 child
simple_gast!(node Datum from DATUM |syntax: &MagusSyntaxNode| {
    syntax.children_with_tokens().count() == 1
});

macro_rules! datum_as_type {
    (node $name:ident for $type:ident from $stype:ident) => {
        impl Datum {
            pub fn $name(&self) -> Option<$type> {
                match self.0.children().next() {
                    Some(node) if node.kind() == $stype => $type::cast(node),
                    _ => None,
                }
            }
        }
    };

    (token $name:ident for $type:ident from $stype:ident) => {
        impl Datum {
            pub fn $name(&self) -> Option<$type> {
                match self.0.children_with_tokens().next() {
                    Some(MagusSyntaxElement::Token(token)) if token.kind() == $stype => {
                        $type::cast(token)
                    }
                    _ => None,
                }
            }
        }
    };
}
datum_as_type!(node as_list for List from LIST);
datum_as_type!(node as_vector for Vector from VECTOR);
datum_as_type!(node as_bytevector for Bytevector from BYTEVECTOR);
datum_as_type!(node as_labeled for LabeledDatum from LABELED);
datum_as_type!(node as_abbreviation for Abbreviation from ABBREV);
datum_as_type!(token as_symbol for Symbol from SYMBOL);
datum_as_type!(token as_label_ref for LabelRef from DTRIGGER);
datum_as_type!(token as_number for Number from NUMBER);
datum_as_type!(token as_string for StringToken from STRING);
datum_as_type!(token as_char for Character from CHARACTER);
datum_as_type!(token as_bool for Boolean from BOOLEAN);

#[derive(Debug, Clone)]
pub struct List(MagusSyntaxNode);
impl List {
    /// Get the head element
    pub fn head(&self) -> Option<Datum> {
        self.datum().next()
    }

    /// Looks for a dot token within (without checking for valid structure)
    pub fn has_dot(&self) -> bool {
        self.0
            .children_with_tokens()
            .any(|elem| matches!(elem, MagusSyntaxElement::Token(tok) if tok.kind() == DOT))
    }

    /// This only asks if a list is syntactically valid, it does not check if the list should be in
    /// a certain shape because it is a special form. use [`List::special_form()`] for that
    pub fn is_valid(&self) -> bool {
        let dot_token = self.0.children_with_tokens().find_map(|elem| match elem {
            MagusSyntaxElement::Token(tok) if tok.kind() == DOT => Some(tok),
            _ => None,
        });

        match dot_token {
            Some(tok) => {
                // Datum counting time!
                let datum_indicies: Vec<_> = self
                    .0
                    .children()
                    .filter_map(Datum::cast)
                    .map(|dat| dat.syntax().text_range().start())
                    .collect();

                let (before_dot, after_dot) =
                    datum_indicies
                        .iter()
                        .fold((0, 0), |(before_dot, after_dot), di| {
                            if di > &tok.text_range().start() {
                                (before_dot, after_dot + 1)
                            } else {
                                (before_dot + 1, after_dot)
                            }
                        });

                before_dot > 0 && after_dot == 1
            }
            None => true,
        }
    }
}
simple_gast!(node List from LIST);
contains!(trivia List);
contains!(datum List);

#[derive(Debug, Clone)]
pub struct Vector(MagusSyntaxNode);
impl Vector {}
simple_gast!(node Vector from VECTOR);
contains!(trivia Vector);
contains!(datum Vector);

#[derive(Debug, Clone)]
pub struct Bytevector(MagusSyntaxNode);
impl Bytevector {
    pub fn bytes(&self) -> impl Iterator<Item = u8> + '_ {
        self.datum()
            .filter_map(|dat| dat.as_number())
            .filter_map(|num| match num.number() {
                Some(SchemeNumber::Exact(ExactReal::Integer {
                    value,
                    is_neg: false,
                })) if value <= 255 => Some(value as u8),
                _ => None,
            })
    }

    pub fn is_valid(&self) -> bool {
        self.0.children()
            .filter_map(Datum::cast)
            .map(|dat| dat.as_number())
            .all(|maybe_byte| match maybe_byte {
                Some(num) => num.number().is_some_and(|sn|
                    matches!(sn, SchemeNumber::Exact(ExactReal::Integer { value, is_neg: false }) if value <= 255)
                ),
                None => false
            })
    }
}
simple_gast!(node Bytevector from BYTEVECTOR);
contains!(trivia Bytevector);
contains!(datum Bytevector);

#[derive(Debug, Clone)]
pub struct Symbol(MagusSyntaxToken);
impl Symbol {
    /// returns the case-folded identifier
    pub fn identifier(&self, case_insensitive: bool) -> Option<Box<str>> {
        fn read_ident(s: &str, ci: bool) -> Option<Box<str>> {
            let tok = SyntaxToken::lexer(s).next();
            match tok {
                Some(Ok(SyntaxToken::Identifier(id))) => Some(if ci {
                    Box::from(CaseMapper::new().fold_string(&id).as_str())
                } else {
                    id
                }),
                _ => None,
            }
        }
        // Look for the last directive the precedes us (if any)
        // If there is one, that directive determines our case-sensitivity
        // If there is not one, use the passed in case-insensitivity
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
            read_ident(self.0.text(), ci)
        } else {
            read_ident(self.0.text(), case_insensitive)
        }
    }
}
simple_gast!(token Symbol from SYMBOL);

macro_rules! simple_extract {
    ($ty:ident::$name:ident from $stt:ident as $type:ty ) => {
        impl $ty {
            pub fn $name(&self) -> Option<$type> {
                if let Some(Ok(SyntaxToken::$stt(val))) = SyntaxToken::lexer(self.0.text()).next() {
                    Some(val)
                } else {
                    None
                }
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Number(MagusSyntaxToken);
simple_gast!(token Number from NUMBER);
simple_extract!(Number::number from Number as SchemeNumber);

#[derive(Debug, Clone)]
pub struct StringToken(MagusSyntaxToken);
simple_gast!(token StringToken from STRING);
simple_extract!(StringToken::string from String as Box<str>);

#[derive(Debug, Clone)]
pub struct Character(MagusSyntaxToken);
simple_gast!(token Character from CHARACTER);
simple_extract!(Character::char from Character as char);

#[derive(Debug, Clone)]
pub struct Boolean(MagusSyntaxToken);
simple_gast!(token Boolean from BOOLEAN);
simple_extract!(Boolean::bool from Boolean as bool);

#[derive(Debug, Clone)]
pub struct LabelRef(MagusSyntaxToken);
simple_gast!(token LabelRef from DTRIGGER);
simple_extract!(LabelRef::trigger from DatumLabelValue as usize);

#[derive(Debug, Clone)]
pub struct DirectiveToken(MagusSyntaxToken);
simple_gast!(token DirectiveToken from DIRECTIVE);
simple_extract!(DirectiveToken::directive from Directive as Directive);
