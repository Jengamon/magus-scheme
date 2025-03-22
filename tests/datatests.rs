use datatest_stable::Utf8Path;
use magus::{
    ExternalCompilerContext, Fuel, Value,
    compiler::{Compiler, LibraryDefinitionContext, LibraryName, ParseProgram, World},
    general_parser::general_parse,
    interpreter::{Interpreter, NullIncluder, ValuePointers},
    lexer::Token,
    library_name, stdlib,
};
use similar::{ChangeTag, TextDiff};

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

    let mut interp = Interpreter::default();
    let mut test_world = World::default();
    // test_world.insert(
    //     LibraryName::from_iter(library_name!(interp.interner_mut() => scheme base)),
    //     stdlib::base::Base::default(),
    // )?;
    let includer = NullIncluder;
    let comp = interp.new_compiler();
    stdlib::base::register_module(&mut interp, &comp, &mut test_world, Some(10_000))?;
    let file_name = format!("{path}.scm");
    let chunk =
        interp.compiler_context::<anyhow::Error>(&comp, |mc, comp, value_pointers, interner| {
            let programs =
                (file_name.as_str(), data.source()).parse_program(mc, interner, false)?;
            let mut ecc = ExternalCompilerContext {
                world: &test_world,
                includer: &includer,
                interner,
            };
            let library_def = LibraryDefinitionContext {
                max_fuel: None,
                value_pointers,
            };
            Ok(comp.compile(mc, &mut ecc, &library_def, programs)?)
        })?;
    let mut fuel = Fuel::with(1_000_000);
    let thread = interp.new_thread(&chunk);
    // Run thread until out-of-fuel or finished
    let mut is_finished = false;
    while fuel.remaining() > 0 && !is_finished {
        interp.run(&thread, |ctx, _arena, interner| {
            if ctx.thread.borrow().is_finished() {
                is_finished = true;
            } else {
                ctx.thread
                    .borrow_mut(&ctx)
                    .step(ctx, interner, &test_world, &includer, &mut fuel);
            }
        });
    }

    if !is_finished {
        // we didn't finish, so we ran out of fuel
        println!("test ran out of fuel");
        Err(DatatestError(Box::from(path)))?
    }

    let file_name_spur = interp.interner_mut().get_or_intern(file_name);
    let results = interp.try_run(&thread, |ctx, _arena, interner| {
        match ctx.thread.borrow().result().expect("finished execution") {
            Ok(v) => Ok(v
                .into_iter()
                .map(|v| Value::resolve_into(v, interner.clone(), ctx.null_value).to_string())
                .map(|s| Box::from(s.as_str()))
                .collect::<Vec<_>>()),
            // Alternate display, which removes pointer data (for UI tests)
            Err(e) => Err(format!(
                "{:#}",
                e.display(interner, [(file_name_spur, data.source())])
            )
            .split('\n')
            .map(Box::from)
            .collect::<Vec<_>>()),
        }
    });
    let errored = match results.as_ref() {
        Ok(res) => {
            if res != &data.processed {
                let new = res.iter().map(|s| s.as_ref()).collect::<Vec<_>>();
                let old = data
                    .processed
                    .iter()
                    .map(|s| s.as_ref())
                    .collect::<Vec<_>>();
                let diff = TextDiff::from_slices(old.as_slice(), new.as_slice());
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                        ChangeTag::Equal => " ",
                    };
                    println!("{}{}", sign, change);
                }
                true
            } else if !data.errors.is_empty() {
                println!("errors should be empty if successful result");
                true
            } else {
                false
            }
        }
        Err(errs) => {
            if errs != &data.errors {
                let new = errs.iter().map(|s| s.as_ref()).collect::<Vec<_>>();
                let old = data.errors.iter().map(|s| s.as_ref()).collect::<Vec<_>>();
                let diff = TextDiff::from_slices(old.as_slice(), new.as_slice());
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                        ChangeTag::Equal => " ",
                    };
                    println!("{}{}", sign, change);
                }
                true
            } else if !data.processed.is_empty() {
                println!("values should be empty if error result");
                true
            } else {
                false
            }
        }
    };

    // TODO We finished, so compare results (and if DATATEST_EXPECT, write the results to test file)
    // from https://matklad.github.io/2021/05/31/how-to-test.html
    // add an env var that instead of outputting the errors of a test
    // updates the file a test is at to correspond to the test.
    if std::env::var("DATATEST_EXPECT").is_ok() {
        let mut data = data;
        match results {
            Ok(res) => {
                data.errors = vec![];
                data.processed = res;
            }
            Err(e) => {
                data.processed = vec![];
                data.errors = e;
            }
        }
        std::fs::write(path, data.to_string())?;
        Ok(())
    } else if errored {
        Err(DatatestError(Box::from(path)))?
    } else {
        Ok(())
    }
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

    if debug_output.is_empty() && err_output.trim().is_empty() {
        Err(anyhow::anyhow!("no expectation").context(DatatestError(Box::from(path))))?;
    }

    let mut interner = lasso::Rodeo::new();
    let mut test_world = World::default();
    // TODO Convert to use Interpreter to get easy `register_module`
    test_world.insert(
        LibraryName::from_iter(library_name!(interner => scheme base)),
        stdlib::base::Base::default(),
    )?;
    let mut chunk_text = None;
    let mut error_text = None;
    let maybe_error: Option<anyhow::Error> = gc_arena::arena::rootless_mutate(|mc| {
        let includer = NullIncluder;
        let programs =
            (format!("{path}.scm"), data.source()).parse_program(mc, &mut interner, false)?;
        let mut compiler = Compiler::new(mc);
        let mut ecc = ExternalCompilerContext {
            world: &test_world,
            includer: &includer,
            interner: &mut interner,
        };
        let value_pointers = ValuePointers::fake(mc);
        let library_def = LibraryDefinitionContext {
            max_fuel: None,
            value_pointers,
        };
        match compiler.compile(mc, &mut ecc, &library_def, programs) {
            Ok(chunk) => {
                let chunk_debug = format!("{chunk:#?}");
                chunk_text = Some(chunk_debug.clone());
                error_text = None;
                // Compile the source to run the test
                if !debug_output.is_empty() && debug_output.trim() != chunk_debug.trim() {
                    let diff = TextDiff::from_lines(&debug_output, &chunk_debug);
                    let mut output = String::new();
                    for change in diff.iter_all_changes() {
                        let sign = match change.tag() {
                            ChangeTag::Delete => "-",
                            ChangeTag::Insert => "+",
                            ChangeTag::Equal => " ",
                        };
                        output += &format!("{}{}", sign, change);
                    }
                    return Err(anyhow::anyhow!(output));
                } else if !err_output.trim().is_empty() {
                    return Err(anyhow::anyhow!(
                        "expected an error, but compilation succeeded\n\n{}",
                        chunk_text.as_ref().unwrap(),
                    ));
                }
            }
            Err(e) => {
                chunk_text = None;
                error_text = Some(e.to_string());
                if !err_output.is_empty() && err_output.trim() != e.to_string().trim() {
                    let e = e.to_string();
                    let diff = TextDiff::from_lines(&err_output, &e);
                    let mut output = String::new();
                    for change in diff.iter_all_changes() {
                        let sign = match change.tag() {
                            ChangeTag::Delete => "-",
                            ChangeTag::Insert => "+",
                            ChangeTag::Equal => " ",
                        };
                        output += &format!("{}{}", sign, change);
                    }
                    return Err(anyhow::anyhow!(output));
                } else if !debug_output.trim().is_empty() {
                    return Err(anyhow::anyhow!(
                        "expected a chunk, but compilation failed:\n\n{}",
                        error_text.as_ref().unwrap()
                    ));
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
    {test = compile_test, root = "test_data", pattern = r"^.*\.csd"},
    {test = scheme_test, root = "test_data", pattern = r"^.*\.sct"},
}
