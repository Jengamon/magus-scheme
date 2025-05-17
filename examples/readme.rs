use std::io::Cursor;

use magus::value::ModeWrite;

fn main() -> anyhow::Result<()> {
    use magus::ParseProgram;
    let code: &str = include_str!("input.scm");

    let mut interpreter = magus::Interpreter::default();
    let mut world = magus::World::default();
    let includer = magus::NullIncluder;
    let compiler = interpreter.new_compiler();

    let input = Cursor::new(Vec::new());
    let output = Cursor::new(Vec::new());
    let error = Cursor::new(Vec::new());

    let thread = interpreter.new_thread(input, output, error);
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        magus::stdlib::base::Base {
            additional_features: std::sync::Arc::new([]),
        },
        None,
        |thread, vp| magus::LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: None,
            thread,
        },
    )?;
    interpreter.register_module(
        &thread,
        &compiler,
        &mut world,
        magus::stdlib::write::Write,
        None,
        |thread, vp| magus::LibraryDefinitionContext {
            max_fuel: Some(10_000),
            value_pointers: vp,
            additional_features: None,
            thread,
        },
    )?;
    let chunk = interpreter.compiler_context::<anyhow::Error>(
        &thread,
        &compiler,
        |mc, compiler, value_pointers, thread, interner| {
            let programs = ("input.scm", code).parse_program(mc, interner, false)?;
            let mut ecc = magus::ExternalCompilerContext {
                world: &world,
                includer: &includer,
                interner,
            };
            let library_def = magus::LibraryDefinitionContext {
                max_fuel: Some(10_000),
                value_pointers,
                additional_features: None,
                thread,
            };
            Ok(compiler.compile(mc, &mut ecc, &library_def, programs)?)
        },
    )?;
    let mut fuel = magus::Fuel::with(1_000_000);
    interpreter.run(&thread, |ctx, arena, _| {
        let chunk = arena.chunk(&chunk);
        ctx.thread.borrow_mut(&ctx).include(&ctx, chunk, false);
    });
    // Run thread until out-of-fuel or finished
    let mut is_finished = false;
    while fuel.remaining() > 0 && !is_finished {
        interpreter.run(&thread, |ctx, _arena, interner| {
            if ctx.thread.borrow().is_finished() {
                is_finished = true;
            } else {
                ctx.thread
                    .borrow_mut(&ctx)
                    .step(ctx, interner, &world, &includer, &mut fuel);
            }
        });
    }

    let file_name = interpreter.interner_mut().get_or_intern_static("input.scm");
    interpreter.try_run(&thread, |ctx, _arena, interner| {
        println!(
            "STDOUT: `{}`",
            String::from_utf8_lossy(
                ctx.thread
                    .borrow_mut(&ctx)
                    .default_output_port()
                    .close()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .as_any_mut()
                    .downcast_ref::<Cursor<Vec<u8>>>()
                    .unwrap()
                    .clone()
                    .into_inner()
                    .as_slice()
            )
        );
        match ctx.thread.borrow().result() {
            Some(Ok(res)) => {
                for result in res {
                    eprintln!(
                        "{}",
                        magus::Value::resolve_into::<_, ModeWrite>(
                            result,
                            interner.clone(),
                            ctx.null_value
                        )
                    )
                }
                Ok(())
            }
            Some(Err(e)) => Err(anyhow::anyhow!(
                "{}",
                e.display(interner, [(file_name, code)])
            ))?,
            None => {
                eprintln!(">> NO RESULTS <<");
                Ok(())
            }
        }
    })
}
