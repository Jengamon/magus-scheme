use datatest_parse::parse;
use datatest_stable::Utf8Path;
use magus::{general_parser::general_parse, lexer::Token};

#[derive(thiserror::Error, Debug)]
#[error("error(s) occured in general parse datatest at {0}")]
pub struct DatatestError(Box<Utf8Path>);

fn scheme_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (data, errs) = datatest_parse::parse(&contents);
    println!("{data:#?} {errs:?}");
    if let Some(data) = data {
        println!("{data}");
    }
    Ok(())
}

fn general_parser_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (data, errs) = datatest_parse::parse(&contents);
    let Some(data) = data else {
        println!(
            "Failed to parse datatest:\n{}",
            errs.into_iter()
                .map(|e| format!("\t- {e}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
        Err(DatatestError(Box::from(path)))?
    };
    let sn_output = data.processed.join("\n");

    // parse the source and run the test
    let gast = general_parse(data.source());
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

    let ed_error = if data.errors.len() != gast.errors().len() {
        println!(
            "error in {path}: expected {} errors, found {}",
            data.errors.len(),
            gast.errors().len()
        );
        true
    } else if let Some((debug, error)) = data
        .errors
        .iter()
        .zip(gast.errors().iter())
        .find(|(d, err)| d.trim() != format!("{:?}", err))
    {
        println!("error in {path}: error mismatch\n\nGot:\n{error:?}\n\nExpected:\n{debug}");
        true
    } else {
        false
    };

    // from https://matklad.github.io/2021/05/31/how-to-test.html
    // add an env var that instead of outputting the errors of a test
    // updates the file a test is at to correspond to the test.
    if std::env::var("DATATEST_EXPECT").is_ok() {
        let mut data = data;
        data.errors = gast
            .errors()
            .iter()
            .map(|err| format!("{err:?}"))
            .map(|s| Box::from(s.as_str()))
            .collect();
        data.processed = gast_sn_output.split('\n').map(Box::from).collect();
        // remove the last string as it will always be a newline, and we don't want it
        data.processed.pop();
        std::fs::write(path, data.to_string())?;
        Ok(())
    } else if sn_error || ed_error {
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

fn lexer_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
    let (data, errs) = datatest_parse::parse(&contents);
    let Some(data) = data else {
        println!(
            "Failed to parse datatest:\n{}",
            errs.into_iter()
                .map(|e| format!("\t- {e}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
        Err(DatatestError(Box::from(path)))?
    };
    // Reverse error and tokens lines to make them more processable
    let mut errors: Vec<_> = data.errors.iter().rev().collect();
    let mut tokens: Vec<_> = data.processed.iter().rev().collect();
    let mut mismatches = Vec::with_capacity(errors.len() + tokens.len());

    for (token, span) in Token::lexer(data.source()) {
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

    // from https://matklad.github.io/2021/05/31/how-to-test.html
    // add an env var that instead of outputting the errors of a test
    // updates the file a test is at to correspond to the test.
    if std::env::var("DATATEST_EXPECT").is_ok() {
        let mut data = data;
        let (errors, tokens): (Vec<_>, _) =
            Token::lexer(data.source()).partition(|(t, _)| t.is_err());
        data.errors = errors
            .into_iter()
            .map(|(e, span)| Box::from(format!("[{span:?}] {}", e.unwrap_err())))
            .collect();
        data.processed = tokens
            .into_iter()
            .map(|(t, span)| Box::from(format!("[{span:?}] {:?}", t.unwrap())))
            .collect();
        std::fs::write(path, data.to_string())?;
        Ok(())
    } else if !mismatches.is_empty() {
        println!("Lexer test {path} failed:\n  got -> expected");
        for (expected, got) in mismatches {
            println!(
                "  {got} -> {}",
                expected.map(|s| s.as_ref()).unwrap_or("<not present>")
            )
        }
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

datatest_stable::harness! {
    general_parser_test, "test_data", r"^.*\.gpd",
    lexer_test, "test_data", r"^.*\.lxd",
    scheme_test, "test_data", r"^.*\.sct",
}
