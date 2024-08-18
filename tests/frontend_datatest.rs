use datatest_stable::Utf8Path;
use magus::{general_parser::general_parse, lexer::Token};

#[derive(thiserror::Error, Debug)]
#[error("error(s) occured in general parse datatest at {0}")]
pub struct DatatestError(Box<Utf8Path>);

struct LenInfo {
    error_count: usize,
    process_count: usize,
}

type DatatestFile<'a> = (
    Vec<&'a str>,
    Vec<&'a str>,
    String,
    Vec<(usize, &'a str)>,
    LenInfo,
);

// from https://matklad.github.io/2021/05/31/how-to-test.html
// add an env var that instead of outputting the errors of a test
// updates the file a test is at to correspond to the test.
fn stitch_to_datatest(
    error_section: impl AsRef<str>,
    processed_section: impl AsRef<str>,
    source: impl AsRef<str>,
    mut comments: Vec<(usize, &str)>,
    len_info: LenInfo,
) -> String {
    // reverse for nice processing
    comments.reverse();

    let error_lines = error_section.as_ref().lines().collect::<Vec<_>>();
    let processed_lines = processed_section.as_ref().lines().collect::<Vec<_>>();
    let source_lines = source.as_ref().lines().collect::<Vec<_>>();
    let mut lines = Vec::with_capacity(
        error_lines.len() + processed_lines.len() + source_lines.len() + comments.len(),
    );
    let error_len = error_lines.len();
    let process_len = processed_lines.len();
    fn comment_line_check<'a>(
        lines: &[&str],
        comments: &mut Vec<(usize, &'a str)>,
        len_info: &LenInfo,
        error_len: usize,
        process_len: usize,
    ) -> Option<&'a str> {
        if let Some(cl) = comments.last().map(|(li, _)| li) {
            // use len_info to calculate a new line
            let al = if *cl < len_info.error_count {
                // factor in diff between new and old errors
                cl.saturating_add_signed(error_len as isize - len_info.error_count as isize)
            } else {
                // factor in diff between new and old errors and new and old processed
                cl.saturating_add_signed(
                    process_len as isize - len_info.process_count as isize
                        + (len_info.error_count as isize - error_len as isize),
                )
            };
            if lines.len() >= al {
                let Some((_, line)) = comments.pop() else {
                    unreachable!()
                };
                return Some(line);
            }
        }
        None
    }

    if error_lines.is_empty() {
        while let Some(line) =
            comment_line_check(&lines, &mut comments, &len_info, error_len, process_len)
        {
            lines.push(line);
        }
    }
    for error_line in error_lines {
        while let Some(line) =
            comment_line_check(&lines, &mut comments, &len_info, error_len, process_len)
        {
            lines.push(line);
        }
        lines.push(error_line);
    }
    lines.push("---");
    for processed_line in processed_lines {
        while let Some(line) =
            comment_line_check(&lines, &mut comments, &len_info, error_len, process_len)
        {
            lines.push(line);
        }
        lines.push(processed_line);
    }
    // Push in the remaining comments
    lines.extend(comments.into_iter().map(|(_, line)| line));
    lines.push("---");
    // We know source cannot contain comments
    lines.extend(source_lines);

    lines.join("\n")
}

fn read_datatest(test_contents: &str) -> DatatestFile {
    let mut error_section = vec![];
    let mut processed_section = vec![];
    let mut source = String::new();
    let mut comments = vec![];

    #[derive(PartialEq, Eq, Debug, Default)]
    enum Section {
        #[default]
        Error,
        Processed,
        Source,
    }
    let mut section = Section::default();
    let mut error_count = 0;
    let mut process_count = 0;

    for (idx, line) in test_contents.lines().enumerate() {
        if line.trim_start().starts_with(';') && section != Section::Source {
            // comment
            comments.push((idx, line));
            match section {
                Section::Error => {
                    error_count += 1;
                }
                Section::Processed => {
                    process_count += 1;
                }
                _ => {}
            };
            continue;
        }

        match section {
            Section::Error => match line {
                s if s.trim_end() == "---" => {
                    section = Section::Processed;
                }
                esl => {
                    error_count += 1;
                    error_section.push(esl)
                }
            },
            Section::Processed => match line {
                s if s.trim_end() == "---" => {
                    section = Section::Source;
                }
                psl => {
                    process_count += 1;
                    processed_section.push(psl);
                }
            },
            Section::Source => {
                // this should be really streamlined, and just copy and add newline
                if !source.is_empty() {
                    source.push('\n');
                }
                source.push_str(line);
            }
        }
    }

    (
        error_section,
        processed_section,
        source,
        comments,
        LenInfo {
            error_count,
            process_count,
        },
    )
}

fn general_parser_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (error_debugs, sn_output_lines, source, comments, li) = read_datatest(&contents);
    let sn_output = sn_output_lines.join("\n");

    // parse the source and run the test
    let gast = general_parse(&source);
    let gast_sn_output = format!("{:#?}", gast.syntax());
    let sn_error = if sn_output.trim() != gast_sn_output.trim() {
        println!(
            "error in {path}: mismatched CST\n\nGot:\n{}\n\nExpected:\n{}",
            gast_sn_output.trim(),
            sn_output.trim()
        );
        true
    } else {
        false
    };

    let ed_error = if error_debugs.len() != gast.errors().len() {
        println!(
            "error in {path}: expected {} errors, found {}",
            error_debugs.len(),
            gast.errors().len()
        );
        true
    } else if let Some((debug, error)) = error_debugs
        .iter()
        .zip(gast.errors().iter())
        .find(|(d, err)| d.trim() != format!("{:?}", err))
    {
        println!("error in {path}: error mismatch\n\nGot:\n{error:?}\n\nExpected:\n{debug}");
        true
    } else {
        false
    };

    if std::env::var("DATATEST_EXPECT").is_ok() {
        std::fs::write(
            path,
            stitch_to_datatest(
                gast.errors()
                    .iter()
                    .map(|err| format!("{err:?}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                &gast_sn_output,
                source,
                comments,
                li,
            ),
        )?;
        Ok(())
    } else if sn_error || ed_error {
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

fn lexer_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (mut errors, mut tokens, source, comments, li) = read_datatest(&contents);
    // Reverse error and tokens lines to make them more processable
    errors.reverse();
    tokens.reverse();
    let mut mismatches = Vec::with_capacity(errors.len() + tokens.len());

    for (token, span) in Token::lexer(&source) {
        match token {
            Ok(tok) => {
                let got = format!("[{span:?}] {tok:?}");
                let expected = tokens.pop();
                if !expected.is_some_and(|expect| expect.trim() == got) {
                    mismatches.push((expected, got));
                }
            }
            Err(err) => {
                let got = format!("[{span:?}] {err}");
                let expected = errors.pop();
                if !expected.is_some_and(|expect| expect.trim() == got) {
                    mismatches.push((expected, got));
                }
            }
        }
    }

    if std::env::var("DATATEST_EXPECT").is_ok() {
        // TODO preserve comment position relative to other lines
        let (errors, tokens): (Vec<_>, _) = Token::lexer(&source).partition(|(t, _)| t.is_err());
        std::fs::write(
            path,
            stitch_to_datatest(
                errors
                    .into_iter()
                    .map(|(e, span)| format!("[{span:?}] {}", e.unwrap_err()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                tokens
                    .into_iter()
                    .map(|(t, span)| format!("[{span:?}] {:?}", t.unwrap()))
                    .collect::<Vec<_>>()
                    .join("\n"),
                source,
                comments,
                li,
            ),
        )?;
        Ok(())
    } else if !mismatches.is_empty() {
        println!("Lexer test {path} failed:\n  got -> expected");
        for (expected, got) in mismatches {
            println!("  {got} -> {}", expected.unwrap_or("<not present>"))
        }
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

datatest_stable::harness! {
    general_parser_test, "test_data", r"^.*\.gpd",
    lexer_test, "test_data", r"^.*\.lxd",
}
