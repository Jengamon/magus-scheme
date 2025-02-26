use datatest_stable::Utf8Path;
use magus::{
    compiler::{Compiler, LibraryName, ParseProgram, World},
    general_parser::general_parse,
    interpreter::NullIncluder,
    lexer::Token,
    library_name, stdlib,
};

#[derive(thiserror::Error, Debug)]
#[error("error(s) occured in general parse datatest at {0}")]
pub struct DatatestError(Box<Utf8Path>);

fn scheme_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
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
    println!("{data}\n\n{data:#?}");
    Ok(())
}

/// Tests the compiler
///
/// - Error section is the error expected (if any)
/// - Processed section is the debug output of the resulting chunk (if successfully compiled)
///
/// This test enables:
/// (scheme base)
fn compile_test(path: &Utf8Path, contents: String) -> datatest_stable::Result<()> {
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
    let err_output = data.errors.join("\n");
    let debug_output = data.processed.join("\n");

    // Compile the source to run the test
    let mut interner = lasso::Rodeo::new();
    let mut test_world = World::default();
    test_world.insert(
        LibraryName::from_iter(library_name!(interner => scheme base)),
        stdlib::base::Base,
    )?;
    let mut chunk_text = None;
    let mut error_text = None;
    let maybe_error: Option<anyhow::Error> = gc_arena::arena::rootless_mutate(|mc| {
        let includer = NullIncluder;
        let programs =
            (format!("{path}.scm"), data.source()).parse_program(mc, &mut interner, false)?;
        let mut compiler = Compiler::new(mc, &mut interner);
        match compiler.compile(mc, &mut interner, &test_world, &includer, programs) {
            Ok(chunk) => {
                let chunk_debug = format!("{chunk:#?}");
                chunk_text = Some(chunk_debug.clone());
                error_text = None;
                if !debug_output.is_empty() && debug_output.trim() != chunk_debug.trim() {
                    return Err(anyhow::anyhow!(
                        "expected chunk:\n{debug_output}\n\ncompilation result:\n{chunk_debug}"
                    ));
                } else if !err_output.trim().is_empty() {
                    return Err(anyhow::anyhow!(
                        "expected an error, but compilation succeeded"
                    ));
                }
            }
            Err(e) => {
                chunk_text = None;
                error_text = Some(e.to_string());
                if !err_output.is_empty() && err_output.trim() != e.to_string().trim() {
                    return Err(anyhow::anyhow!(
                        "expected error:\n{err_output}\n\ncompilation result:\n{e}"
                    ));
                } else if !debug_output.trim().is_empty() {
                    return Err(anyhow::anyhow!("expected a chunk, but compilation failed"));
                }
            }
        };
        Ok(())
    })
    .err();

    let errored = if let Some(err) = maybe_error {
        println!("error in {path}: {err}");
        true
    } else {
        false
    };

    // from https://matklad.github.io/2021/05/31/how-to-test.html
    // add an env var that instead of outputting the errors of a test
    // updates the file a test is at to correspond to the test.
    if std::env::var("DATATEST_EXPECT").is_ok() {
        let mut data = data;
        data.errors = error_text
            .unwrap_or_default()
            .split('\n')
            .map(Box::from)
            .collect();
        data.processed = chunk_text
            .unwrap_or_default()
            .split('\n')
            .map(Box::from)
            .collect();
        std::fs::write(path, data.to_string())?;
        Ok(())
    } else if errored {
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
}

/// Tests the general parser
///
/// - Error section is the errors expected to happen
/// - Processed section is the debug output of the resulting syntax node
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

/// Tests the lexer
///
/// - Error section is the list of errors expected
/// - Processed section is the tokens in source order
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
                if expected.is_none_or(|expect| expect.trim() != got) {
                    mismatches.push((expected, got));
                }
            }
            Err(err) => {
                let got = format!("[{span:?}] {err}");
                let expected = errors.pop();
                if expected.is_none_or(|expect| expect.trim() != got) {
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

// btw test search is recursive (yay!)
datatest_stable::harness! {
    {test = general_parser_test, root = "test_data", pattern = r"^.*\.gpd"},
    {test = lexer_test, root = "test_data", pattern = r"^.*\.lxd"},
    // disable these tests for now, b/c datatest_stable or nextest don't like when there are no tests
    // {test = compile_test, root = "test_data", pattern = r"^.*\.csd"},
    // {test = scheme_test, root = "test_data", pattern = r"^.*\.sct"},
}
