use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use chumsky::prelude::*;

#[derive(Debug, Clone)]
pub struct DatatestFile {
    /// All contents are expected to consist of nonoverlapping spans
    /// sorted in ascending order.
    comments: Vec<(Box<str>, SimpleSpan<usize>)>,
    pub errors: Vec<Box<str>>,
    pub processed: Vec<Box<str>>,
    source: Box<str>,
}

impl DatatestFile {
    pub fn source(&self) -> &str {
        self.source.as_ref()
    }
}

impl Display for DatatestFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // switch back together the contents of a datatest file.
        // each line is a span, and they *do not* (or should not) overlap, so simply sort the
        // spans on their start (if 2 items share the same start, sort the
        // comment before the line), then split each section with "---"
        let mut idx = 0;
        let mut in_error_section = true;
        let mut errors = self.errors.iter();
        let mut processed = self.processed.iter();
        let mut comments = self
            .comments
            .iter()
            .map(|(st, sp)| (sp.start, (st, sp)))
            .collect::<HashMap<_, _>>();
        loop {
            let mut handled = HashSet::new();
            while let Some((k, (st, sp))) = comments
                .iter()
                .find(|(si, _)| (**si).saturating_sub(1) <= idx && !handled.contains(*si))
            {
                writeln!(f, ";{st}")?;
                idx = sp.end;
                handled.insert(*k);
            }
            for k in handled {
                comments.remove(&k);
            }

            if in_error_section {
                if let Some(err) = errors.next() {
                    writeln!(f, "{err}")?;
                    idx += err.len();
                } else {
                    writeln!(f, "---")?;
                    idx += 4;
                    in_error_section = false;
                }
            } else if let Some(proc) = processed.next() {
                writeln!(f, "{proc}")?;
                idx += proc.len();
            } else {
                // output the rest of the comments
                let mut remainder = comments.into_iter().collect::<Vec<_>>();
                remainder.sort_by_key(|(si, _)| *si);
                for (_, (st, _)) in remainder {
                    writeln!(f, ";{st}")?;
                }
                writeln!(f, "---")?;
                break;
            }
        }

        write!(f, "{}", self.source)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DatatestElement {
    /// a line beginning in ";"
    Comment(Box<str>),
    /// lines at the start of the test
    Error(Box<str>),
    /// lines after the first --- line
    Processed(Box<str>),
}
impl DatatestElement {
    fn payload(self) -> Box<str> {
        match self {
            Self::Comment(p) => p,
            Self::Error(p) => p,
            Self::Processed(p) => p,
        }
    }
}
type ElemPair = (DatatestElement, SimpleSpan<usize>);

#[derive(Debug, PartialEq, Eq)]
enum CommentOrLine {
    Comment(Box<str>),
    Line(Box<str>),
}

fn section_separator<'a>() -> impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char, SimpleSpan>>> {
    just("---\n").to(())
}

fn comment_or_line<'a>(
) -> impl Parser<'a, &'a str, CommentOrLine, extra::Err<Rich<'a, char, SimpleSpan>>> {
    choice((
        just(';').ignore_then(
            none_of(['\n'])
                .repeated()
                .collect::<String>()
                .map(|b| CommentOrLine::Comment(Box::from(b.as_str()))),
        ),
        none_of(['\n'])
            .repeated()
            .collect::<String>()
            .map(|b| CommentOrLine::Line(Box::from(b.as_str()))),
    ))
}

fn section<'a>(
    data_map: impl Fn(Box<str>) -> DatatestElement + 'a,
) -> impl Parser<'a, &'a str, Vec<ElemPair>, extra::Err<Rich<'a, char, SimpleSpan>>> {
    just("---")
        .not()
        .rewind()
        .ignore_then(comment_or_line().then_ignore(just("\n")))
        .map_with(move |cm, e| match cm {
            CommentOrLine::Comment(c) => (DatatestElement::Comment(c), e.span()),
            CommentOrLine::Line(d) => ((data_map)(d), e.span()),
        })
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(section_separator())
}

// TODO Error handling!!!
fn parser<'a>() -> impl Parser<'a, &'a str, DatatestFile, extra::Err<Rich<'a, char, SimpleSpan>>> {
    group((
        section(DatatestElement::Error),
        section(DatatestElement::Processed),
        any().repeated().collect::<String>().then_ignore(end()),
    ))
    .map(|(es, ps, src)| {
        let (comments_err, errors) = es
            .into_iter()
            .partition::<Vec<_>, _>(|(e, _)| matches!(e, DatatestElement::Comment(_)));
        let (comments_proc, processed) = ps
            .into_iter()
            .partition::<Vec<_>, _>(|(e, _)| matches!(e, DatatestElement::Comment(_)));
        DatatestFile {
            comments: comments_err
                .into_iter()
                .chain(comments_proc)
                .map(|(de, s)| (de.payload(), s))
                .collect(),
            errors: errors.into_iter().map(|(de, _)| de.payload()).collect(),
            processed: processed.into_iter().map(|(de, _)| de.payload()).collect(),
            source: Box::from(src.as_str()),
        }
    })
}

pub fn parse(
    input: &impl AsRef<str>,
) -> (
    Option<DatatestFile>,
    Vec<chumsky::error::Rich<'_, char, SimpleSpan>>,
) {
    parser().parse(input.as_ref()).into_output_errors()
}

#[cfg(test)]
mod tests {
    use assert2::assert;

    use crate::{comment_or_line, parser, CommentOrLine};

    use chumsky::prelude::*;

    #[test]
    fn parse_file() {
        let (output, errs) = parser().parse("; Interpreter output TODO\n---\n; Compiler output TODO\n---\n(define y 3) (+ y x) (define x 4) (+ x #!fold-case Y X)\n").into_output_errors();
        dbg!(output);
        assert!(errs.is_empty());
    }

    #[test]
    fn parse_line() {
        let (output, errs) = comment_or_line().parse("ajsidjsidjs").into_output_errors();
        assert!(errs.is_empty());
        assert!(output == Some(CommentOrLine::Line(Box::from("ajsidjsidjs"))));

        let (output, errs) = comment_or_line()
            .separated_by(just("\n"))
            .allow_trailing()
            .collect::<Vec<_>>()
            .parse("ajsidjsidjs\n")
            .into_output_errors();
        assert!(errs.is_empty());
        assert!(
            output
                == Some(vec![
                    CommentOrLine::Line(Box::from("ajsidjsidjs")),
                    CommentOrLine::Line(Box::from(""))
                ])
        );
    }

    #[test]
    fn parse_comment() {
        let (output, errs) = comment_or_line()
            .parse("; ajsidjsidjs")
            .into_output_errors();
        assert!(errs.is_empty());
        assert!(output == Some(CommentOrLine::Comment(Box::from(" ajsidjsidjs"))));
        let (output, errs) = comment_or_line()
            .separated_by(just("\n"))
            .allow_trailing()
            .collect::<Vec<_>>()
            .parse("; ajsidjsidjs\n")
            .into_output_errors();
        assert!(errs.is_empty());
        assert!(
            output
                == Some(vec![
                    CommentOrLine::Comment(Box::from(" ajsidjsidjs")),
                    CommentOrLine::Line(Box::from(""))
                ])
        );
    }
}
