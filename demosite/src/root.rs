use std::{ops::Deref, sync::Arc};

use crate::code::CodeEditor;
use leptos::prelude::*;
use web_sys::MouseEvent;

#[derive(Debug, Clone)]
enum RuntimeInner {
    Ready {
        compiler: magus::CompilerHandle,
        thread: magus::ThreadHandle,
        chunk: Option<magus::ChunkHandle>,
    },
    CompileError(Arc<anyhow::Error>),
    Finished(magus::ValueHandle),
}

#[derive(Debug, Clone)]
struct Runtime {
    id: ulid::Ulid,
    inner: RuntimeInner,
}

impl Runtime {
    pub fn new(interpreter: &mut magus::Interpreter, world: &magus::World) -> Self {
        let compiler = interpreter.new_compiler();
        let thread = interpreter.new_thread();

        fn lib_def_func<'a, 'gc>(
            thread: magus::ThreadPtr<'gc>,
            vp: magus::ValuePointers<'gc>,
        ) -> magus::LibraryDefinitionContext<'a, 'gc> {
            magus::LibraryDefinitionContext {
                max_fuel: Some(10_000),
                value_pointers: vp,
                additional_features: None,
                thread,
            }
        }

        interpreter
            .register_local_module(
                &thread,
                &compiler,
                world,
                &magus::stdlib::base::Base {
                    additional_features: std::sync::Arc::new([]),
                },
                None,
                lib_def_func,
            )
            .unwrap();
        interpreter
            .register_local_module(
                &thread,
                &compiler,
                world,
                &magus::stdlib::cxr::Cxr,
                None,
                lib_def_func,
            )
            .unwrap();

        Self {
            id: ulid::Ulid::new(),
            inner: RuntimeInner::Ready {
                compiler,
                thread,
                chunk: None,
            },
        }
    }

    fn is_running(&self, interpreter: &mut magus::Interpreter) -> bool {
        matches!(&self.inner, RuntimeInner::Ready { thread, chunk: Some(_), .. } if interpreter.try_run(thread, |ctx, _, _| {
            !ctx.thread.borrow().is_finished()
        }))
    }

    fn submit_code(
        &mut self,
        input: impl Into<String>,
        interpreter: &mut magus::Interpreter,
        world: &magus::World,
        includer: &dyn magus::Includer,
    ) {
        use magus::ParseProgram;
        match &mut self.inner {
            RuntimeInner::Ready {
                compiler,
                thread,
                chunk,
            } if chunk.is_none() => {
                let cchunk = interpreter.compiler_context::<anyhow::Error>(
                    thread,
                    compiler,
                    |mc, compiler, value_pointers, thread, interner| {
                        let programs =
                            ("input.scm", input.into()).parse_program(mc, interner, false)?;
                        let mut ecc = magus::ExternalCompilerContext {
                            world,
                            includer,
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
                );

                match cchunk {
                    Ok(cchunk) => {
                        *chunk = Some(cchunk.clone());
                        // TODO on thread, include our chunk
                    }
                    Err(e) => {
                        self.inner = RuntimeInner::CompileError(Arc::new(e));
                    }
                }
            }
            _ => {
                log::warn!("cannot run code twice!")
            }
        }
    }
}

fn init_interpreter() -> (magus::Interpreter, magus::World) {
    let mut interpreter = magus::Interpreter::new();
    let mut world = magus::World::new();

    interpreter
        .register_native_module(
            &mut world,
            &magus::stdlib::base::Base {
                additional_features: std::sync::Arc::new([]),
            },
            None,
        )
        .unwrap();

    (interpreter, world)
}

/// Root of the application
#[component]
pub fn App() -> impl IntoView {
    let (interpreter, world) = init_interpreter();
    let interpreter = StoredValue::new_local(interpreter);
    let world = StoredValue::new(world);
    let includer = StoredValue::new(magus::NullIncluder);

    let (code, set_code) = signal(String::new());
    let (threads, set_threads) = signal(Vec::<Runtime>::new());

    let code_in_new_thread = move |ev: MouseEvent| {
        let mut interpreter = interpreter.write_value();
        let world = world.read_value();
        let includer = includer.read_value();
        let mut rt = Runtime::new(&mut interpreter, &world);
        rt.submit_code(
            event_target_value(&ev),
            &mut interpreter,
            &world,
            includer.deref(),
        );
        set_threads.update(|threads| threads.push(rt));
    };

    view! {
        <div class="flex h-screen">
            <CodeEditor code=code set_code=set_code />
            <div class="w-1/2 flex-col">
                <button class="btn" on:click=code_in_new_thread>
                    "Run Code"
                </button>
                <For each=move || threads.get() key=|thread| thread.id let(thread)>
                    <p>{move || format!("{thread:?}")}</p>
                </For>
            </div>
        </div>
    }
}
