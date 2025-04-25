use core::fmt;

use gc_arena::{Gc, Mutation};

use crate::{
    AbbreviationKind, ContainsDatum as _, DatumVisitor, ExactReal, GAstNode, GAstToken as _,
    SchemeNumber,
    bytecode::SourceData,
    compiler::{ListHead, Program, ProgramData},
    general_parser::GeneralParserError,
};

use super::{ParseProgram, ProgramPtr};

#[derive(thiserror::Error, Debug)]
pub struct GeneralParseErrors(Box<[GeneralParserError]>);
impl fmt::Display for GeneralParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "failed to parse code:")?;

        for err in &self.0 {
            writeln!(f, "  - {err} ({err:?})")?;
        }

        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum StringProgramError {
    #[error(transparent)]
    GeneralParse(#[from] GeneralParseErrors),
    #[error(transparent)]
    GAst(#[from] GAstProgramError),
}
impl<T: AsRef<str>, SN: AsRef<str>> ParseProgram for (SN, T) {
    type Error = StringProgramError;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error> {
        let (source_name, source) = self;
        let gast = crate::general_parse(source);
        if !gast.errors().is_empty() {
            return Err(GeneralParseErrors(gast.into_errors().into()))?;
        }

        Ok((
            source_name,
            &crate::Module::cast(gast.syntax()).expect("ICE: top-level code cannot be Module"),
        )
            .parse_program(mc, interner, case_insensitive)?)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum GAstProgramError {
    #[error(transparent)]
    NumberError(#[from] std::num::TryFromIntError),
    #[error("out of range")]
    OutOfRange(rowan::TextRange),
    #[error("number is not supported")]
    UnsupportedNumber(rowan::TextRange),
    #[error("not parsable")]
    Unparseable(rowan::TextRange),
}
impl<SN: AsRef<str>> ParseProgram for (SN, &'_ crate::Module) {
    type Error = GAstProgramError;
    fn parse_program<'gc>(
        self,
        mc: &Mutation<'gc>,
        interner: &mut lasso::Rodeo,
        case_insensitive: bool,
    ) -> Result<Vec<ProgramPtr<'gc>>, Self::Error> {
        let (source_name, module) = self;

        struct ProgramVisitor<'a, 'gc> {
            source_id: lasso::Spur,
            mc: &'a Mutation<'gc>,
            interner: &'a mut lasso::Rodeo,
            case_insensitive: bool,
            ptr: Option<Result<ProgramPtr<'gc>, GAstProgramError>>,
        }

        fn rowan_to_pair(tr: rowan::TextRange) -> (usize, usize) {
            (tr.start().into(), tr.end().into())
        }

        macro_rules! source_data {
            ($v:expr, $nd:expr) => {
                Some(SourceData {
                    source_id: $v.source_id,
                    range: rowan_to_pair($nd.syntax().text_range()),
                })
            };
        }

        impl DatumVisitor for ProgramVisitor<'_, '_> {
            fn visit_abbreviation(&mut self, abbreviation: &crate::Abbreviation) {
                let Some(abbr) = abbreviation.kind() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        abbreviation.syntax().text_range(),
                    )));
                    return;
                };

                // get the subprogram
                let Some(d) = abbreviation.datum().next() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        abbreviation.syntax().text_range(),
                    )));
                    return;
                };

                self.visit_datum(&d);

                let Some(Ok(inner)) = self.ptr.take() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        abbreviation.syntax().text_range(),
                    )));
                    return;
                };

                let head = match abbr {
                    AbbreviationKind::Quote => Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Symbol(self.interner.get_or_intern_static("quote")),
                            Some(SourceData {
                                source_id: self.source_id,
                                range: rowan_to_pair(abbreviation.syntax().text_range()),
                            }),
                        ),
                    ),
                    AbbreviationKind::Quasiquote => Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Symbol(self.interner.get_or_intern_static("quasiquote")),
                            source_data!(self, abbreviation),
                        ),
                    ),
                    AbbreviationKind::Unquote => Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Symbol(self.interner.get_or_intern_static("unquote")),
                            source_data!(self, abbreviation),
                        ),
                    ),
                    AbbreviationKind::UnquoteSplicing => Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Symbol(
                                self.interner.get_or_intern_static("unquote-splicing"),
                            ),
                            source_data!(self, abbreviation),
                        ),
                    ),
                };

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(
                        ProgramData::List {
                            head: ListHead::Program(head),
                            body: vec![inner],
                        },
                        source_data!(self, abbreviation),
                    ),
                )));
            }

            fn visit_number(&mut self, number: &crate::Number) {
                let num = number.number();
                match num {
                    Some(SchemeNumber::Inexact(f)) => {
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(ProgramData::Inexact(f), source_data!(self, number)),
                        )));
                    }
                    Some(SchemeNumber::InexactComplex {
                        real: f,
                        imaginary: 0.,
                    }) => {
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(ProgramData::Inexact(f), source_data!(self, number)),
                        )));
                    }
                    Some(SchemeNumber::InexactComplex { .. })
                    | Some(SchemeNumber::ExactPolar { .. }) => {
                        self.ptr = Some(Err(GAstProgramError::UnsupportedNumber(
                            number.syntax().text_range(),
                        )));
                    }
                    Some(SchemeNumber::Exact(ExactReal::Integer { value, is_neg })) => {
                        let num = TryInto::<i64>::try_into(value)
                            .map(|i| i * if is_neg { -1 } else { 1 })
                            .map_err(GAstProgramError::NumberError);
                        self.ptr = Some(num.map(|i| {
                            Gc::new(
                                self.mc,
                                Program::new(ProgramData::Integer(i), source_data!(self, number)),
                            )
                        }));
                    }
                    Some(SchemeNumber::Exact(ExactReal::Rational {
                        numer,
                        denom,
                        is_neg,
                    })) => {
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(
                                ProgramData::Rational(is_neg, numer, denom),
                                source_data!(self, number),
                            ),
                        )));
                    }
                    Some(SchemeNumber::Exact(ExactReal::Inf { is_neg })) => {
                        // infinities are always inexact
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(
                                ProgramData::Inexact(if is_neg {
                                    f64::NEG_INFINITY
                                } else {
                                    f64::INFINITY
                                }),
                                source_data!(self, number),
                            ),
                        )));
                    }
                    Some(SchemeNumber::Exact(ExactReal::Nan { is_neg })) => {
                        // negativity of nan is ignored, as it is not meaningful
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(
                                ProgramData::Inexact(f64::NAN.copysign(if is_neg {
                                    -1.
                                } else {
                                    1.
                                })),
                                source_data!(self, number),
                            ),
                        )));
                    }
                    Some(SchemeNumber::ExactComplex {
                        real: ExactReal::Integer { value, is_neg },
                        imaginary: i,
                    }) if i.is_zero() => {
                        let num = TryInto::<i64>::try_into(value)
                            .map(|i| i * if is_neg { -1 } else { 1 })
                            .map_err(GAstProgramError::NumberError);
                        self.ptr = Some(num.map(|i| {
                            Gc::new(
                                self.mc,
                                Program::new(ProgramData::Integer(i), source_data!(self, number)),
                            )
                        }));
                    }
                    Some(SchemeNumber::Exact(_)) | Some(SchemeNumber::ExactComplex { .. }) => {
                        self.ptr = Some(Err(GAstProgramError::UnsupportedNumber(
                            number.syntax().text_range(),
                        )));
                    }
                    None => {
                        self.ptr = Some(Err(GAstProgramError::Unparseable(
                            number.syntax().text_range(),
                        )))
                    }
                }
            }

            fn visit_labeled(&mut self, labeled: &crate::LabeledDatum) {
                let Some(label) = labeled.label() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        labeled.syntax().text_range(),
                    )));
                    return;
                };

                let Some(datum) = labeled.datum().next() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        labeled.syntax().text_range(),
                    )));
                    return;
                };

                self.visit_datum(&datum);
                self.ptr = match self.ptr.take() {
                    Some(Ok(p)) => Some(Ok(Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Labeled { label, item: p },
                            source_data!(self, labeled),
                        ),
                    ))),
                    _ => Some(Err(GAstProgramError::Unparseable(
                        labeled.syntax().text_range(),
                    ))),
                };
            }

            fn visit_label_ref(&mut self, label_ref: &crate::LabelRef) {
                let Some(label) = label_ref.trigger() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        label_ref.syntax().text_range(),
                    )));
                    return;
                };

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::LabelRef(label), source_data!(self, label_ref)),
                )))
            }

            fn visit_symbol(&mut self, symbol: &crate::Symbol) {
                let Some(symbol_str) = symbol.identifier(self.case_insensitive) else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        symbol.syntax().text_range(),
                    )));
                    return;
                };
                let sym = self.interner.get_or_intern(symbol_str);

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::Symbol(sym), source_data!(self, symbol)),
                )))
            }

            fn visit_bool(&mut self, bool: &crate::Boolean) {
                let Some(boolv) = bool.bool() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        bool.syntax().text_range(),
                    )));
                    return;
                };

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::Bool(boolv), source_data!(self, bool)),
                )))
            }

            fn visit_char(&mut self, char: &crate::Character) {
                let Some(charv) = char.char() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        char.syntax().text_range(),
                    )));
                    return;
                };

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::Char(charv), source_data!(self, char)),
                )))
            }

            fn visit_string(&mut self, string: &crate::StringToken) {
                let Some(stringv) = string.string() else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        string.syntax().text_range(),
                    )));
                    return;
                };
                let spur = self.interner.get_or_intern(&stringv);

                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::String(spur), source_data!(self, string)),
                )))
            }

            fn visit_list(&mut self, list: &crate::List) {
                // This is the "most fun" one....
                // so for lists, we have to first check the head, if it is import
                // or define-library, mark it as such, then evaluate the bodies...
                // ... yay....
                // define our 2 special symbols
                let import = self.interner.get_or_intern_static("import");
                let define_library = self.interner.get_or_intern_static("define-library");
                let mut datum = list.datum();
                if let Some(dot) = list.dot() {
                    // The list has a dot, so interpret as a dotted list
                    // we *don't* have to handle the head separately
                    if !list.is_valid() {
                        // cuz GAst can handle syntactically invalid programs listwise
                        // make sure the list is actually valid (which means we can assume that after a dot, only 1 datum exists)
                        self.ptr = Some(Err(GAstProgramError::Unparseable(
                            list.syntax().text_range(),
                        )));
                        return;
                    }

                    let dot_range = dot.text_range();

                    let mut dot_encountered = false;
                    let mut pre_dot = vec![];
                    let mut post_dot = None;
                    loop {
                        let Some(data) = datum.next() else {
                            if dot_encountered {
                                break;
                            } else {
                                self.ptr = Some(Err(GAstProgramError::Unparseable(
                                    list.syntax().text_range(),
                                )));
                                return;
                            }
                        };

                        dot_encountered |= data.syntax().text_range().start() > dot_range.start();

                        self.visit_datum(&data);
                        match self.ptr.take() {
                            Some(Ok(p)) => {
                                if dot_encountered {
                                    if post_dot.is_some() {
                                        unreachable!("is_valid should check for this");
                                    }

                                    post_dot = Some(p);
                                } else {
                                    pre_dot.push(p);
                                }
                            }
                            None | Some(Err(_)) => {
                                self.ptr = Some(Err(GAstProgramError::Unparseable(
                                    list.syntax().text_range(),
                                )));
                                return;
                            }
                        }
                    }

                    self.ptr = Some(Ok(Gc::new(
                        self.mc,
                        Program {
                            data: ProgramData::DottedList {
                                pre_dot,
                                dot: post_dot.unwrap(),
                            },
                            source: source_data!(self, list),
                        },
                    )));
                } else {
                    let Some(head) = datum.next() else {
                        self.ptr = Some(Ok(Gc::new(
                            self.mc,
                            Program::new(ProgramData::EmptyList, source_data!(self, list)),
                        )));
                        return;
                    };

                    // eval head
                    self.visit_datum(&head);

                    // .. and read the result!
                    let head = match self.ptr.take() {
                        Some(Ok(p)) if matches!(&p.data, ProgramData::Symbol(sym) if *sym == import) => {
                            ListHead::Import
                        }
                        Some(Ok(p)) if matches!(&p.data, ProgramData::Symbol(sym) if *sym == define_library) => {
                            ListHead::DefineLibrary
                        }
                        Some(Ok(p)) => ListHead::Program(p),
                        None | Some(Err(_)) => {
                            self.ptr = Some(Err(GAstProgramError::Unparseable(
                                list.syntax().text_range(),
                            )));
                            return;
                        }
                    };

                    // read in body
                    let body: Result<Vec<_>, _> = datum
                        .map(|d| {
                            self.visit_datum(&d);
                            match self.ptr.take() {
                                Some(Ok(p)) => Ok(p),
                                None | Some(Err(_)) => {
                                    Err(GAstProgramError::Unparseable(list.syntax().text_range()))
                                }
                            }
                        })
                        .collect();

                    match body {
                        Ok(body) => {
                            self.ptr = Some(Ok(Gc::new(
                                self.mc,
                                Program::new(
                                    ProgramData::List { head, body },
                                    source_data!(self, list),
                                ),
                            )));
                        }
                        Err(e) => {
                            self.ptr = Some(Err(e));
                        }
                    }
                }
            }

            fn visit_bytevector(&mut self, bytevector: &crate::Bytevector) {
                if bytevector.is_valid() {
                    self.ptr = Some(Ok(Gc::new(
                        self.mc,
                        Program::new(
                            ProgramData::Bytevector(bytevector.bytes().flatten().collect()),
                            source_data!(self, bytevector),
                        ),
                    )));
                } else {
                    self.ptr = Some(Err(GAstProgramError::Unparseable(
                        bytevector.syntax().text_range(),
                    )));
                }
            }

            fn visit_vector(&mut self, vector: &crate::Vector) {
                let mut items = vec![];
                for d in vector.datum() {
                    self.visit_datum(&d);
                    match self.ptr.take() {
                        Some(Ok(p)) => {
                            items.push(p);
                        }
                        None | Some(Err(_)) => {
                            self.ptr = Some(Err(GAstProgramError::Unparseable(
                                vector.syntax().text_range(),
                            )));
                            return;
                        }
                    }
                }
                self.ptr = Some(Ok(Gc::new(
                    self.mc,
                    Program::new(ProgramData::Vector(items), source_data!(self, vector)),
                )));
            }
        }

        let mut programs = vec![];
        for d in module.datum() {
            let mut visitor = ProgramVisitor {
                source_id: interner.get_or_intern(source_name.as_ref()),
                mc,
                interner,
                case_insensitive,
                ptr: None,
            };
            // dbg!(&d);
            visitor.visit_datum(&d);
            programs.push(visitor.ptr.expect("null program")?);
        }

        Ok(programs)
    }
}
