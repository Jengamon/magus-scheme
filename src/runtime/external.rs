use core::fmt;

use crate::{ContainsDatum, Datum, DatumKind, SchemeNumber};

// all scheme code can be represented by these structures
// macros interact with this, Treewalk runs this
// TODO Vm converts this into bytecode to run
#[derive(Debug, Clone, PartialEq)]
pub enum ExternalRepresentationKind {
    Label(usize),
    Boolean(bool),
    Number(SchemeNumber),
    String(Box<str>),
    Char(char),
    Symbol(Box<str>),
    // attempting to evaluate this if empty is an error
    // a quote of an empty list evaluates to null
    List(Vec<ExternalRepresentation>),
    Vector(Vec<ExternalRepresentation>),
    Bytevector(Vec<u8>),
}

// Implement visitor pattern for ExternalRepresentation
pub trait ExternalRepresentationVisitor {
    fn visit_representation(&mut self, repr: &ExternalRepresentation) {
        match repr {
            ExternalRepresentation::Simple(kind) => match kind {
                ExternalRepresentationKind::Label(trigger) => self.visit_label(*trigger),
                ExternalRepresentationKind::Boolean(bool) => self.visit_bool(*bool),
                ExternalRepresentationKind::Number(num) => self.visit_number(*num),
                ExternalRepresentationKind::String(strg) => self.visit_string(strg.as_ref()),
                ExternalRepresentationKind::Char(chr) => self.visit_char(*chr),
                ExternalRepresentationKind::Symbol(sym) => self.visit_symbol(sym.as_ref()),
                ExternalRepresentationKind::List(lst) => self.visit_list(lst.as_slice()),
                ExternalRepresentationKind::Vector(vec) => self.visit_vector(vec.as_slice()),
                ExternalRepresentationKind::Bytevector(bytes) => {
                    self.visit_bytevector(bytes.as_slice())
                }
            },
            ExternalRepresentation::Labeled(label, repr) => {
                self.visit_labeled(*label, repr.as_ref())
            }
        }
    }

    fn visit_labeled(&mut self, label: usize, repr: &ExternalRepresentation) {
        _ = label;
        _ = repr;
    }

    fn visit_label(&mut self, trigger: usize) {
        _ = trigger;
    }

    fn visit_bool(&mut self, value: bool) {
        _ = value;
    }

    fn visit_number(&mut self, value: SchemeNumber) {
        _ = value;
    }

    fn visit_string(&mut self, value: &str) {
        _ = value;
    }

    fn visit_char(&mut self, value: char) {
        _ = value;
    }

    fn visit_symbol(&mut self, symbol: &str) {
        _ = symbol;
    }

    fn visit_list(&mut self, exprs: &[ExternalRepresentation]) {
        _ = exprs;
    }

    fn visit_vector(&mut self, exprs: &[ExternalRepresentation]) {
        _ = exprs;
    }

    fn visit_bytevector(&mut self, bytes: &[u8]) {
        _ = bytes;
    }
}

/// External representation of Scheme code
// structed this way to support representing self-reference
// without requiring a self-referential structure
#[derive(Debug, Clone, PartialEq)]
pub enum ExternalRepresentation {
    Simple(ExternalRepresentationKind),
    Labeled(usize, Box<ExternalRepresentation>),
}

impl fmt::Display for ExternalRepresentation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(repr) => write!(f, "{repr}"),
            Self::Labeled(lbl, repr) => write!(f, "#{lbl}={repr}"),
        }
    }
}

pub trait ToExternal {
    fn to_external(&self) -> ExternalRepresentation;
}

macro_rules! to_external {
    (simple $ty:ty as $er:ident) => {
        impl ToExternal for $ty {
            fn to_external(&self) -> ExternalRepresentation {
                ExternalRepresentation::Simple(ExternalRepresentationKind::$er(*self))
            }
        }
    };
    (simple $ty:ty as $er:ident into $target:ty) => {
        impl ToExternal for $ty {
            fn to_external(&self) -> ExternalRepresentation {
                ExternalRepresentation::Simple(ExternalRepresentationKind::$er(
                    Into::<$target>::into(*self),
                ))
            }
        }
    };
}

impl ToExternal for ExternalRepresentation {
    fn to_external(&self) -> ExternalRepresentation {
        self.clone()
    }
}

// null is an empty list
impl ToExternal for () {
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Simple(ExternalRepresentationKind::List(Vec::new()))
    }
}

to_external!(simple char as Char);
to_external!(simple bool as Boolean);
to_external!(simple u8 as Number into SchemeNumber);
to_external!(simple u16 as Number into SchemeNumber);
to_external!(simple u32 as Number into SchemeNumber);
to_external!(simple u64 as Number into SchemeNumber);
to_external!(simple i8 as Number into SchemeNumber);
to_external!(simple i16 as Number into SchemeNumber);
to_external!(simple i32 as Number into SchemeNumber);
to_external!(simple i64 as Number into SchemeNumber);

// Type wrapper to label an externally representable type
pub struct Labeled<T>(pub usize, pub T);
impl<T: ToExternal> ToExternal for Labeled<T> {
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Labeled(self.0, Box::new(self.1.to_external()))
    }
}

// Type wrapper to make a label real quick
pub struct Label(pub usize);
impl ToExternal for Label {
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Simple(ExternalRepresentationKind::Label(self.0))
    }
}

// provides a quick way to convert a string or identifier to an external representation
pub enum StringOrSymbol<'a> {
    Symbol(&'a str),
    String(&'a str),
}

impl<'a> ToExternal for StringOrSymbol<'a> {
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Simple(match self {
            Self::Symbol(sym) => ExternalRepresentationKind::Symbol(Box::from(*sym)),
            Self::String(strg) => ExternalRepresentationKind::String(Box::from(*strg)),
        })
    }
}

impl<'a> ToExternal for &'a [u8] {
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Simple(ExternalRepresentationKind::Bytevector(self.to_vec()))
    }
}

pub enum ListOrVector<'a, T> {
    List(&'a [T]),
    Vector(&'a [T]),
}

impl<'a, T> ToExternal for ListOrVector<'a, T>
where
    T: ToExternal,
{
    fn to_external(&self) -> ExternalRepresentation {
        ExternalRepresentation::Simple(match self {
            Self::List(lst) => ExternalRepresentationKind::List(
                lst.iter().map(|item| item.to_external()).collect(),
            ),
            Self::Vector(vct) => ExternalRepresentationKind::Vector(
                vct.iter().map(|item| item.to_external()).collect(),
            ),
        })
    }
}

// the ultimate goal
// b/c of how scheme works, this
// is literally the runnable code
// ...
// TODO Do we want Rust code to be able to participate?
// it should be possible
// ok, so we don't really prettify the code this produces...
//
// This is a quick and dirty way to print an external representation
impl fmt::Display for ExternalRepresentationKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Label(lbl) => write!(f, "#{lbl}#"),
            Self::Boolean(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            Self::Char(chr) => write!(f, "#\\{chr}"),
            Self::Number(num) => write!(f, "{num}"),
            Self::String(strg) => write!(f, "\"{}\"", strg.replace('\"', "\\\"")),
            Self::Symbol(sym)
                if sym.chars().enumerate().all(|(i, chr)| {
                    if i == 0 {
                        chr.is_ascii_alphabetic() || "!$%&*/:<=>?^_~".contains(chr)
                    } else {
                        chr.is_ascii_alphanumeric() || "!$%&*/:<=>?^_~+-.@".contains(chr)
                    }
                }) || sym
                    .trim_start_matches(
                        ['+', '-']
                            .into_iter()
                            .chain("!$%&*/:<=>?^_~".chars())
                            .collect::<Vec<_>>()
                            .as_slice(),
                    )
                    .is_empty() =>
            {
                write!(f, "{sym}")
            }
            // please don't use \ as any part of your identifier, it is literally not.
            Self::Symbol(sym) => write!(f, "|{}|", sym.replace('|', "\\|")),
            Self::List(lst) => {
                write!(
                    f,
                    "({})",
                    lst.iter()
                        .map(|ext| ext.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Self::Vector(vec) => {
                write!(
                    f,
                    "#({})",
                    vec.iter()
                        .map(|ext| ext.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Self::Bytevector(bytes) => {
                // my preference
                write!(
                    f,
                    "#u8({})",
                    bytes
                        .iter()
                        .map(|ext| format!("#x{ext:02x}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
        }
    }
}

impl TryFrom<Datum> for ExternalRepresentation {
    type Error = ();
    fn try_from(value: Datum) -> Result<Self, Self::Error> {
        if let Some(kind) = value.kind() {
            match kind {
                DatumKind::List => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::List(
                        value
                            .as_list()
                            .unwrap()
                            .datum()
                            .filter_map(|dat| dat.try_into().ok())
                            .collect(),
                    ),
                )),
                DatumKind::Bytevector => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Bytevector(
                        value.as_bytevector().unwrap().bytes().collect(),
                    ),
                )),
                DatumKind::Vector => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Vector(
                        value
                            .as_vector()
                            .unwrap()
                            .datum()
                            .filter_map(|dat| dat.try_into().ok())
                            .collect(),
                    ),
                )),
                DatumKind::Labeled => {
                    let labeled = value.as_labeled().unwrap();
                    let inner = labeled
                        .datum()
                        .find_map(|dat| {
                            dat.try_into()
                                .ok()
                                .map(|exin: ExternalRepresentation| Box::new(exin))
                        })
                        .ok_or(())?;
                    Ok(ExternalRepresentation::Labeled(
                        labeled.label().ok_or(())?,
                        inner,
                    ))
                }
                DatumKind::Abbreviation => {
                    let abbrev = value.as_abbreviation().unwrap();
                    let payload = abbrev
                        .datum()
                        .find_map(|dat| dat.try_into().ok())
                        .ok_or(())?;
                    Ok(ExternalRepresentation::Simple(
                        ExternalRepresentationKind::List(
                            [
                                ExternalRepresentation::Simple(ExternalRepresentationKind::Symbol(
                                    Box::from(abbrev.kind().ok_or(())?.to_string().as_str()),
                                )),
                                payload,
                            ]
                            .into_iter()
                            .collect(),
                        ),
                    ))
                }
                DatumKind::Symbol => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Symbol(
                        value.as_symbol().unwrap().identifier(false).ok_or(())?,
                    ),
                )),
                DatumKind::Number => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Number(
                        value.as_number().unwrap().number().ok_or(())?,
                    ),
                )),
                DatumKind::StringToken => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::String(
                        value.as_string().unwrap().string().ok_or(())?,
                    ),
                )),
                DatumKind::Character => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Char(value.as_char().unwrap().char().ok_or(())?),
                )),
                DatumKind::Boolean => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Boolean(value.as_bool().unwrap().bool().ok_or(())?),
                )),
                DatumKind::LabelRef => Ok(ExternalRepresentation::Simple(
                    ExternalRepresentationKind::Label(
                        value.as_label_ref().unwrap().trigger().ok_or(())?,
                    ),
                )),
            }
        } else {
            Err(())
        }
    }
}
