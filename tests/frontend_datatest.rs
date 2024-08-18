use datatest_stable::Utf8Path;
use magus::general_parser::general_parse;

#[derive(thiserror::Error, Debug)]
#[error("error(s) occured in general parse datatest at {0}")]
pub struct DatatestError(Box<Utf8Path>);

fn read_datatest(test_contents: &str) -> (Vec<&str>, Vec<&str>, String) {
    let mut error_section = vec![];
    let mut processed_section = vec![];
    let mut source = String::new();

    #[derive(PartialEq, Eq, Debug, Default)]
    enum Section {
        #[default]
        Error,
        Processed,
        Source,
    }
    let mut section = Section::default();

    for line in test_contents.lines() {
        if line.trim_start().starts_with(';') && section != Section::Source {
            // comment
            continue;
        }

        match section {
            Section::Error => match line {
                s if s.trim_end() == "---" => {
                    section = Section::Processed;
                }
                esl => error_section.push(esl),
            },
            Section::Processed => match line {
                s if s.trim_end() == "---" => {
                    section = Section::Source;
                }
                psl => {
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

    (error_section, processed_section, source)
}

fn general_parser_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (error_debugs, sn_output_lines, source) = read_datatest(&contents);
    let sn_output = sn_output_lines.join("\n");

    // parse the source and run the test
    let gast = general_parse(source);
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

    if sn_error || ed_error {
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

fn lexer_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    todo!()
}

datatest_stable::harness! {
    general_parser_test, "test_data", r".*\.gpd",
    lexer_test, "test_data", r".*\.lxd",
}
