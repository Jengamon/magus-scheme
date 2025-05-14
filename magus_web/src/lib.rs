//! Bindings to use magus on the web!
use std::{cell::RefCell, rc::Rc, sync::Arc};

use magus::ParseProgram;
use wasm_bindgen::prelude::*;

#[derive(Default)]
struct ModuleRegistry {
    base: Option<magus::stdlib::base::Base>,
    cxr: Option<magus::stdlib::cxr::Cxr>,
    lazy: Option<magus::stdlib::lazy::Lazy>,
    inexact: Option<magus::stdlib::inexact::Inexact>,

    srfi_1: Option<magus::stdlib::srfi::list::Srfi1>,
}

impl ModuleRegistry {
    fn register_enabled_local_modules(
        &self,
        thread: &magus::ThreadHandle,
        compiler: &magus::CompilerHandle,
        world: &magus::World,
        interpreter: &mut magus::Interpreter,
        additional_features: &[Arc<str>],
    ) -> anyhow::Result<()> {
        let addtl_features = || {
            if additional_features.is_empty() {
                None
            } else {
                Some(additional_features)
            }
        };

        macro_rules! module_register {
            ($name:ident) => {
                if let Some($name) = self.$name.as_ref() {
                    interpreter.register_local_module(
                        thread,
                        compiler,
                        world,
                        $name,
                        None,
                        |thread, vp| magus::LibraryDefinitionContext {
                            max_fuel: Some(1_000),
                            additional_features: addtl_features(),
                            value_pointers: vp,
                            thread,
                        },
                    )?;
                }
            };
        }

        module_register!(base);
        module_register!(inexact);
        module_register!(lazy);
        module_register!(cxr);
        module_register!(srfi_1);

        Ok(())
    }
}

#[wasm_bindgen]
pub struct MagusInterpreter {
    interpreter: Rc<RefCell<magus::Interpreter>>,
    world: Rc<RefCell<magus::World>>,
    registry: ModuleRegistry,
    additional_features: Arc<[Arc<str>]>,
}

#[wasm_bindgen]
impl MagusInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new(additional_features: Vec<String>) -> Self {
        MagusInterpreter {
            interpreter: Rc::new(RefCell::new(magus::Interpreter::default())),
            world: Rc::new(RefCell::new(magus::World::default())),
            registry: ModuleRegistry::default(),
            additional_features: additional_features
                .into_iter()
                .map(|s| Arc::from(s.as_str()))
                .collect(),
        }
    }

    pub fn enable_base(&mut self) -> Result<(), String> {
        if self.registry.base.is_some() {
            // don't enable again
            return Ok(());
        }

        let base = magus::stdlib::base::Base {
            additional_features: Arc::clone(&self.additional_features),
        };

        self.interpreter
            .borrow_mut()
            .register_native_module(&mut self.world.borrow_mut(), &base, None)
            .map_err(|e| e.to_string())?;
        self.registry.base = Some(base);
        Ok(())
    }

    pub fn new_thread(&mut self) -> Result<MagusThread, String> {
        let compiler = self.interpreter.borrow_mut().new_compiler();
        let thread = self.interpreter.borrow_mut().new_thread();

        self.registry
            .register_enabled_local_modules(
                &thread,
                &compiler,
                &self.world.borrow(),
                &mut self.interpreter.borrow_mut(),
                &self.additional_features,
            )
            .map_err(|e| e.to_string())?;

        Ok(MagusThread {
            interpreter: Rc::clone(&self.interpreter),
            world: Rc::clone(&self.world),
            additional_features: Arc::clone(&self.additional_features),
            current_source: None,
            compiler,
            thread,
        })
    }
}

#[wasm_bindgen]
pub struct MagusThread {
    interpreter: Rc<RefCell<magus::Interpreter>>,
    world: Rc<RefCell<magus::World>>,
    additional_features: Arc<[Arc<str>]>,
    compiler: magus::CompilerHandle,
    thread: magus::ThreadHandle,

    current_source: Option<(magus::lasso::Spur, Rc<str>)>,
}

fn magus_to_js<'gc>(
    ptr: magus::ValuePtr<'gc>,
    null_ptr: magus::ValuePtr<'gc>,
    resolver: &magus::lasso::Rodeo,
) -> JsValue {
    match *ptr.borrow() {
        magus::Value::Void => JsValue::null(),
        magus::Value::String(s) => JsValue::from_str(s.borrow().as_str()),
        magus::Value::Symbol(s) => JsValue::from_str(resolver.resolve(&s.0)),
        magus::Value::Number(n) => JsValue::from_f64(n.to_inexact()),
        magus::Value::Inexact(f) => JsValue::from_f64(f),
        magus::Value::Bool(b) => JsValue::from_bool(b),
        magus::Value::Cons(c) => {
            if c.is_list(ptr, null_ptr) {
                let values = c
                    .list_values(ptr, null_ptr)
                    .into_iter()
                    .map(|v| magus_to_js(v, null_ptr, resolver))
                    .collect::<Vec<_>>();
                values.into()
            } else {
                let car = c
                    .car
                    .map(|v| magus_to_js(v, null_ptr, resolver))
                    .unwrap_or(JsValue::null());
                let cdr = c
                    .cdr
                    .map(|v| magus_to_js(v, null_ptr, resolver))
                    .unwrap_or(JsValue::null());
                vec![car, cdr].into()
            }
        }
        _ => todo!(),
    }
}

#[wasm_bindgen]
impl MagusThread {
    pub fn compile(
        &mut self,
        filename: String,
        source: String,
        case_insensitive: bool,
        library_fuel_cap: i32,
    ) -> Result<MagusChunk, String> {
        let chunk = self
            .interpreter
            .borrow_mut()
            .compiler_context::<anyhow::Error>(
                &self.thread,
                &self.compiler,
                |mc, compiler, vp, thread, interner| {
                    let programs = (filename.clone(), source.clone()).parse_program(
                        mc,
                        interner,
                        case_insensitive,
                    )?;
                    let mut ecc = magus::ExternalCompilerContext {
                        world: &self.world.borrow(),
                        interner,
                        includer: &magus::NullIncluder,
                    };
                    let library_def = magus::LibraryDefinitionContext {
                        max_fuel: Some(library_fuel_cap),
                        additional_features: if self.additional_features.is_empty() {
                            None
                        } else {
                            Some(&self.additional_features)
                        },
                        value_pointers: vp,
                        thread,
                    };
                    Ok(compiler.compile(mc, &mut ecc, &library_def, programs)?)
                },
            )
            .map_err(|e| e.to_string())?;

        let source_filename = self
            .interpreter
            .borrow_mut()
            .interner_mut()
            .get_or_intern(filename);

        Ok(MagusChunk {
            thread: self.thread.clone(),
            chunk,
            source: Rc::from(source.as_str()),
            source_filename,
        })
    }

    pub fn is_finished(&self) -> bool {
        self.interpreter.borrow_mut().try_enter(|_, arena, _| {
            let thread = arena.thread(&self.thread);
            thread.borrow().is_finished()
        })
    }

    pub fn result(&self) -> Result<JsValue, String> {
        self.interpreter
            .borrow_mut()
            .try_enter(|_mc, arena, interner| {
                let thread = arena.thread(&self.thread);
                match thread.borrow().result() {
                    Some(Ok(res)) => {
                        let results = res.collect::<Vec<_>>();
                        Ok(if results.is_empty() {
                            JsValue::null()
                        } else if results.len() == 1 {
                            magus_to_js(results[0], arena.null_ptr(), interner)
                        } else {
                            results
                                .into_iter()
                                .map(|v| magus_to_js(v, arena.null_ptr(), interner))
                                .collect::<Vec<_>>()
                                .into()
                        })
                    }
                    Some(Err(e)) => Err(e
                        .display(
                            interner,
                            self.current_source
                                .as_ref()
                                // TODO Maybe keep all them chunks around?
                                // or at least once supporting inclusion, included sources and
                                // the main source
                                .map(|(s, src)| (*s, &**src)),
                        )
                        .to_string()),
                    None => Ok(JsValue::null()),
                }
            })
    }

    /// Load a compiled chunk into a thread (it must be compiled from this thread)
    pub fn load(&mut self, chunk: &MagusChunk) -> Result<(), String> {
        if chunk.thread != self.thread {
            return Err("chunk not from thread".to_string());
        }

        self.interpreter.borrow_mut().enter(|mc, arena, _| {
            let thread = arena.thread(&self.thread);
            let chunk = arena.chunk(&chunk.chunk);
            thread.borrow_mut(mc).include(mc, chunk, false);
        });

        self.current_source = Some((chunk.source_filename, Rc::clone(&chunk.source)));

        Ok(())
    }

    /// Run the current code in thread, either using up all the given fuel or
    /// being interrupted
    ///
    /// Returns if code is still under execution
    pub fn run(&mut self, fuel: &mut MagusFuel) -> bool {
        self.interpreter
            .borrow_mut()
            .run(&self.thread, |ctx, _, interner| {
                ctx.thread.borrow_mut(&ctx).step(
                    ctx,
                    interner,
                    &self.world.borrow(),
                    &magus::NullIncluder,
                    &mut fuel.fuel,
                );
            });

        !self.is_finished()
    }

    /// Reset and clear a thread
    pub fn reset(&mut self) {
        self.interpreter
            .borrow_mut()
            .run(&self.thread, |ctx, _, _| {
                ctx.thread.borrow_mut(&ctx).reset();
            });
    }
}

#[wasm_bindgen]
pub struct MagusChunk {
    thread: magus::ThreadHandle,
    chunk: magus::ChunkHandle,
    source_filename: magus::lasso::Spur,
    source: Rc<str>,
}

#[wasm_bindgen]
pub struct MagusFuel {
    fuel: magus::Fuel,
}

impl Default for MagusFuel {
    fn default() -> Self {
        Self {
            fuel: magus::Fuel::empty(),
        }
    }
}

#[wasm_bindgen]
impl MagusFuel {
    #[wasm_bindgen(constructor)]
    pub fn with(fuel: i32) -> Self {
        Self {
            fuel: magus::Fuel::with(fuel),
        }
    }

    pub fn refill(&mut self, fuel: i32, max_fuel: i32) {
        self.fuel.refill(fuel, max_fuel);
    }

    pub fn remaining(&self) -> i32 {
        self.fuel.remaining()
    }
}
