use core::fmt;
use magus::{
    value::escape_write_char, ContainsDatum, Datum, DatumVisitor, GAstNode, GAstToken,
    MagusSyntaxNode,
};
use std::borrow::Cow;
use yansi::Paint;

struct DatumPrintImpl<'a, 'f> {
    fmt: &'a mut fmt::Formatter<'f>,

    result: Option<fmt::Result>,
    align: usize,
}

impl<'a, 'f> DatumPrintImpl<'a, 'f> {
    fn new(fmt: &'a mut fmt::Formatter<'f>) -> Self {
        Self {
            fmt,
            result: None,
            align: 0,
        }
    }

    fn handle_error(&mut self, func: impl FnOnce(&mut Self) -> fmt::Result) {
        if self.result.is_none() || self.result.is_some_and(|res| res.is_ok()) {
            // call the function, and set the result to it
            self.result = Some((func)(self));
        }
    }

    fn write_new_line(&mut self) -> fmt::Result {
        write!(self.fmt, "\n{}", " ".repeat(self.align))
    }

    fn identifier_string(identifier: &str) -> Cow<'_, str> {
        // rough rules for unpiped identifiers
        if identifier
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~".contains(c))
            || identifier.chars().take(1).all(|c| c.is_ascii_alphabetic())
                && identifier
                    .chars()
                    .skip(1)
                    .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-@".contains(c))
            || ["+", "-"].contains(&identifier)
            || (identifier.starts_with(['+', '-'])
                && identifier
                    .chars()
                    .skip(1)
                    .take(1)
                    .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-@".contains(c))
                && identifier
                    .chars()
                    .skip(2)
                    .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)))
            || (identifier.starts_with(['+', '-'])
                && identifier.chars().skip(1).take(1).all(|c| c == '.')
                && identifier
                    .chars()
                    .skip(2)
                    .take(1)
                    .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-.@".contains(c))
                && identifier
                    .chars()
                    .skip(3)
                    .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)))
            || (identifier.starts_with('.')
                && identifier
                    .chars()
                    .skip(1)
                    .take(1)
                    .all(|c| c.is_ascii_alphabetic() || r"!$%&*/:<=>?^_~+\-.@".contains(c))
                && identifier
                    .chars()
                    .skip(2)
                    .all(|c| c.is_ascii_alphanumeric() || r"!$%&*/:<=>?^_~+\-.@".contains(c)))
        {
            Cow::Borrowed(identifier)
        } else {
            Cow::Owned(format!("|{identifier}|"))
        }
    }

    // These formatting procedures are based of the description of how Cyclone formats
    // Scheme code as laid out here: https://justinethier.github.io/cyclone/docs/Scheme-code-conventions.html

    /// Find if a datum node is followed by a newline or a datum
    ///
    /// # Returns
    /// `true` if followed by newline, `false` otherwise
    fn followed_by_newline(node: &MagusSyntaxNode) -> bool {
        node.siblings_with_tokens(magus::rowan::Direction::Next)
            .skip(1)
            .find_map(|elem| match elem.kind() {
                magus::SyntaxKind::LINEEND => Some(true),
                magus::SyntaxKind::DATUM => Some(false),
                _ => None,
            })
            .unwrap_or(false)
    }

    /// Find if a datum node is preceded by a newline or a datum
    ///
    /// # Returns
    /// `true` if preceded by newline, `false` otherwise
    fn preceded_by_newline(node: &MagusSyntaxNode) -> bool {
        node.siblings_with_tokens(magus::rowan::Direction::Prev)
            .skip(1)
            .find_map(|elem| match elem.kind() {
                magus::SyntaxKind::LINEEND => Some(true),
                magus::SyntaxKind::DATUM
                | magus::SyntaxKind::START_BYTEVECTOR
                | magus::SyntaxKind::START_VECTOR => Some(false),
                _ => None,
            })
            .unwrap_or(false)
    }

    /// Override the default formatting heuristic (forcing 2 space / 1 space formatting)
    /// for certain operators
    fn operator_override(node: &MagusSyntaxNode) -> bool {
        if let Some(symbol) = magus::Datum::cast(node.clone())
            .as_ref()
            .and_then(magus::Datum::as_symbol)
        {
            matches!(symbol
                .identifier(false)
                .as_ref()
                .map(|ident| ident.as_ref()), Some(ident) if [
                        "define",
                        "lambda",
                        "define-library",
                        "define-syntax",
                        "begin",
                        "when",
                        "unless",
                    ]
                    .contains(&ident))
        } else {
            false
        }
    }

    fn write_list(&mut self, list: &magus::List) -> fmt::Result {
        // Special handling for operators and operands
        write!(self.fmt, "(")?;
        let Some(operator) = list.datum().next() else {
            return write!(self.fmt, ")");
        };
        // If the operator is alphanumeric and consists of more than 3 characters
        // Then align with the second letter, otherwise align with the first
        let newline_operand1 = Self::followed_by_newline(operator.syntax());
        let alignment = if newline_operand1 || Self::operator_override(operator.syntax()) {
            if let Some(sym) = operator.as_symbol() {
                let op_text = sym.syntax().text().to_string();
                let op_text = Self::identifier_string(op_text.as_str());
                if op_text.chars().count() > 3
                    && op_text.chars().enumerate().all(|(idx, c)| {
                        if idx == 0 {
                            c.is_alphabetic()
                        } else {
                            c.is_alphanumeric() || "!$%&*/:<=>?@^_-".contains(c)
                        }
                    })
                {
                    2
                } else {
                    1
                }
            } else {
                1
            }
        } else {
            let op_text = operator.syntax().text().to_string();
            2 + op_text.len()
        };
        self.align += alignment;
        let use_newlines = newline_operand1 && list.datum().count() > 10;
        self.visit_datum(&operator);
        let list_len = list.datum().count();
        for (idx, operand) in list.datum().skip(1).enumerate() {
            let preserve_newline = Self::preceded_by_newline(operand.syntax());
            if use_newlines || preserve_newline {
                self.write_new_line()
            } else {
                write!(self.fmt, " ")
            }?;
            // + 2 to factor in the skipping of the first element
            if list.dot().is_some_and(|_| idx + 2 == list_len) {
                write!(self.fmt, ". ")?;
            }
            self.visit_datum(&operand);
        }
        if use_newlines {
            self.write_new_line()?;
        }
        self.align -= alignment;
        write!(self.fmt, ")")
    }

    fn write_vector(&mut self, vector_start: &'static str, v: &impl ContainsDatum) -> fmt::Result {
        write!(self.fmt, "{}", vector_start)?;
        // vectors are all rendered with single space delimit, unless the first datum is followed by a newline,
        // then all are separated by newlines
        let Some(first_elem) = v.datum().next() else {
            // nothing to render, an empty vector
            return write!(self.fmt, ")");
        };
        // Since we are preceded on the same line as the vector start
        // simply add the length of that to our align (it's all ASCII compatible, so we dont need segmenting)
        self.align += vector_start.chars().count();
        self.visit_datum(&first_elem);
        if self.result.is_some_and(|r| r.is_err()) {
            return self.result.take().unwrap();
        }
        // Figure out if we use spaces or newlines
        //
        // Check tokens until we either find a LINEEND (use newlines) or a datum (dont)
        let use_newlines = Self::followed_by_newline(first_elem.syntax());
        for d in v.datum().skip(1) {
            let preserve_newline = Self::preceded_by_newline(d.syntax());
            if use_newlines || preserve_newline {
                self.write_new_line()
            } else {
                write!(self.fmt, " ")
            }?;
            self.visit_datum(&d);
        }
        if use_newlines {
            self.write_new_line()?;
        }
        self.align -= vector_start.chars().count();
        write!(self.fmt, ")")
    }
}

impl DatumVisitor for DatumPrintImpl<'_, '_> {
    fn visit_list(&mut self, list: &magus::List) {
        self.handle_error(|visitor| visitor.write_list(list));
    }

    fn visit_vector(&mut self, vector: &magus::Vector) {
        self.handle_error(|visitor| visitor.write_vector("#(", vector));
    }

    fn visit_bytevector(&mut self, bytevector: &magus::Bytevector) {
        self.handle_error(|visitor| visitor.write_vector("#u8(", bytevector));
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
                write!(
                    visitor.fmt,
                    "{}",
                    Self::identifier_string(identifier.as_ref())
                )
            } else {
                write!(visitor.fmt, "{}", "#ERR".red())
            }
        });
    }

    fn visit_char(&mut self, char: &magus::Character) {
        self.handle_error(|visitor| {
            if let Some(char) = char.char() {
                write!(
                    visitor.fmt,
                    "#\\{}",
                    escape_write_char(char, true)
                        .into_iter()
                        .collect::<Box<str>>()
                )
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
                write!(
                    visitor.fmt,
                    "\"{}\"",
                    string
                        .chars()
                        .flat_map(|c| escape_write_char(c, false))
                        .collect::<Box<str>>()
                )
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
