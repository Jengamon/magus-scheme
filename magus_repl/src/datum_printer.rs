use core::fmt;
use magus::{ContainsDatum, Datum, DatumVisitor};
use yansi::Paint;

struct DatumPrintImpl<'a, 'f> {
    fmt: &'a mut fmt::Formatter<'f>,

    result: Option<fmt::Result>,
    indent: usize,
}

impl<'a, 'f> DatumPrintImpl<'a, 'f> {
    fn new(fmt: &'a mut fmt::Formatter<'f>) -> Self {
        Self {
            fmt,
            result: None,
            indent: 0,
        }
    }

    fn handle_error(&mut self, func: impl FnOnce(&mut Self) -> fmt::Result) {
        if self.result.is_none() || self.result.is_some_and(|res| res.is_ok()) {
            // call the function, and set the result to it
            self.result = Some((func)(self));
        }
    }

    fn write_new_line(&mut self) -> fmt::Result {
        write!(self.fmt, "\n{}", "\t".repeat(self.indent))
    }
}

impl DatumVisitor for DatumPrintImpl<'_, '_> {
    fn visit_list(&mut self, list: &magus::List) {
        self.handle_error(|visitor| {
            // handle empty list simply
            if list.datum().count() == 0 {
                write!(visitor.fmt, "()")?;
                return Ok(());
            }

            let list_of_lists = visitor.fmt.alternate()
                && list
                    .datum()
                    .all(|c| c.kind() == Some(magus::DatumKind::List))
                && !list.has_dot();
            let non_symbol_list = visitor.fmt.alternate()
                && list.datum().count() > 5
                && !list.has_dot()
                && list.datum().nth(0).unwrap().kind() != Some(magus::DatumKind::Symbol);
            if list_of_lists {
                visitor.indent += 1;
                visitor.write_new_line()?;
            }
            write!(visitor.fmt, "(")?;
            if list_of_lists || non_symbol_list {
                visitor.indent += 1;
                visitor.write_new_line()?;
            }

            // should we newline between each datum?
            let new_line_between = visitor.fmt.alternate()
                && (list.datum().count() > 5 || list_of_lists || non_symbol_list)
                && !list.has_dot();
            let mut iter = list.datum();
            // write the first element
            if let Some(e) = iter.next() {
                visitor.visit_datum(&e);
            }

            if new_line_between && !(list_of_lists || non_symbol_list) {
                visitor.indent += 1;
            }
            let len = list.datum().count();
            for (idx, following) in iter.enumerate() {
                if new_line_between {
                    visitor.write_new_line()?;
                } else {
                    write!(visitor.fmt, " ")?;
                }
                // idx + 1 b/c we skipped the first element when we are enumerating
                // the iterator
                // len - 1 is the index of the last valid element
                if list.has_dot() && idx + 1 == len - 1 {
                    write!(visitor.fmt, ". ")?;
                }

                visitor.visit_datum(&following);
            }
            if new_line_between {
                visitor.indent -= 1;
            }
            if list_of_lists || non_symbol_list {
                visitor.write_new_line()?;
            }
            write!(visitor.fmt, ")")
        });
    }

    fn visit_vector(&mut self, vector: &magus::Vector) {
        self.handle_error(|visitor| {
            // handle empty vector simply
            if vector.datum().count() == 0 {
                write!(visitor.fmt, "#()")?;
                return Ok(());
            }

            write!(visitor.fmt, "#(")?;
            // should we newline between each datum?
            let new_line_between = visitor.fmt.alternate() && vector.datum().count() > 5;
            let mut iter = vector.datum();
            // write the first element
            if let Some(e) = iter.next() {
                visitor.visit_datum(&e);
            }
            visitor.indent += 1;
            for following in iter {
                if new_line_between {
                    visitor.write_new_line()?;
                } else {
                    write!(visitor.fmt, " ")?;
                }

                visitor.visit_datum(&following);
            }
            visitor.indent -= 1;
            write!(visitor.fmt, ")")
        });
    }

    fn visit_bytevector(&mut self, bytevector: &magus::Bytevector) {
        self.handle_error(|visitor| {
            write!(visitor.fmt, "#u8(")?;
            // should we newline between each datum?
            let new_line_between = visitor.fmt.alternate() && bytevector.datum().count() > 5;
            let mut iter = bytevector.bytes();
            let byte_text = |inp: Option<u8>| {
                if let Some(byte) = inp {
                    Paint::new(format!("#x{:02x}", byte)).primary()
                } else {
                    Paint::new("#ERR".to_string()).red()
                }
            };
            // write the first element
            if let Some(e) = iter.next() {
                write!(visitor.fmt, "{}", byte_text(e))?;
            }
            visitor.indent += 1;
            for following in iter {
                if new_line_between {
                    visitor.write_new_line()?;
                } else {
                    write!(visitor.fmt, " ")?;
                }

                write!(visitor.fmt, "{}", byte_text(following))?;
            }
            visitor.indent -= 1;
            write!(visitor.fmt, ")")
        });
    }

    fn visit_labeled(&mut self, labeled: &magus::LabeledDatum) {
        self.handle_error(|visitor| {
            if let Some(label) = labeled.label() {
                write!(visitor.fmt, "#{label}=")?;
                if let Some(datum) = labeled.datum().next() {
                    visitor.visit_datum(&datum);
                    Ok(())
                } else {
                    write!(visitor.fmt, "{}", "#ERR".red())
                }
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_label_ref(&mut self, label_ref: &magus::LabelRef) {
        self.handle_error(|visitor| {
            if let Some(trigger) = label_ref.trigger() {
                write!(visitor.fmt, "#{trigger}#")
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_abbreviation(&mut self, abbreviation: &magus::Abbreviation) {
        self.handle_error(|visitor| {
            if let Some(abbrev) = abbreviation.kind() {
                write!(visitor.fmt, "{abbrev}")?;
                if let Some(datum) = abbreviation.datum().next() {
                    visitor.visit_datum(&datum);
                    Ok(())
                } else {
                    write!(visitor.fmt, "{}", "#ERR".red())
                }
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_symbol(&mut self, symbol: &magus::Symbol) {
        self.handle_error(|visitor| {
            if let Some(identifier) = symbol.identifier(false) {
                // rough rules for unpiped identifiers
                if identifier
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~".contains(c))
                    || ["+", "-"].contains(&identifier.as_ref())
                    || (identifier.starts_with(['+', '-'])
                        && identifier
                            .chars()
                            .skip(1)
                            .take(1)
                            .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-@".contains(c))
                        && identifier.chars().skip(2).all(|c| {
                            c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)
                        }))
                    || (identifier.starts_with(['+', '-'])
                        && identifier.chars().skip(1).take(1).all(|c| c == '.')
                        && identifier
                            .chars()
                            .skip(2)
                            .take(1)
                            .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-.@".contains(c))
                        && identifier.chars().skip(3).all(|c| {
                            c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)
                        }))
                    || (identifier.starts_with('.')
                        && identifier
                            .chars()
                            .skip(1)
                            .take(1)
                            .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-.@".contains(c))
                        && identifier.chars().skip(2).all(|c| {
                            c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)
                        }))
                {
                    write!(visitor.fmt, "{}", identifier)
                } else {
                    write!(visitor.fmt, "|{}|", identifier)
                }
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_char(&mut self, char: &magus::Character) {
        self.handle_error(|visitor| {
            if let Some(char) = char.char() {
                write!(visitor.fmt, "#\\{}", char)
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_bool(&mut self, bool: &magus::Boolean) {
        self.handle_error(|visitor| {
            if let Some(bool) = bool.bool() {
                write!(visitor.fmt, "{}", if bool { "#t" } else { "#f" })
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_number(&mut self, number: &magus::Number) {
        self.handle_error(|visitor| {
            if let Some(num) = number.number() {
                write!(visitor.fmt, "{num}")
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_string(&mut self, string: &magus::StringToken) {
        self.handle_error(|visitor| {
            if let Some(string) = string.string() {
                write!(visitor.fmt, "\"{}\"", string)
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }
}

pub struct DisplayDatum<'a>(pub &'a Datum);

impl fmt::Display for DisplayDatum<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        let mut printer = DatumPrintImpl::new(f);
        printer.visit_datum(self.0);

        if let Some(res) = printer.result.take() {
            res
        } else {
            Ok(())
        }
    }
}
