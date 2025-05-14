//! Bindings to use magus on the web!
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

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

    /// Enable `(scheme base)`
    pub fn enable_base(&mut self) -> Result<(), String> {
        if self.registry.base.is_some() {
            // don't reenable
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

    /// Enable `(scheme cxr)`
    pub fn enable_cxr(&mut self) -> Result<(), String> {
        if self.registry.cxr.is_some() {
            // don't reenable
            return Ok(());
        }

        let cxr = magus::stdlib::cxr::Cxr;

        self.interpreter
            .borrow_mut()
            .register_native_module(&mut self.world.borrow_mut(), &cxr, None)
            .map_err(|e| e.to_string())?;
        self.registry.cxr = Some(cxr);
        Ok(())
    }

    /// Enable `(scheme inexact)`
    pub fn enable_inexact(&mut self) -> Result<(), String> {
        if self.registry.inexact.is_some() {
            // don't reenable
            return Ok(());
        }

        let inexact = magus::stdlib::inexact::Inexact;

        self.interpreter
            .borrow_mut()
            .register_native_module(&mut self.world.borrow_mut(), &inexact, None)
            .map_err(|e| e.to_string())?;
        self.registry.inexact = Some(inexact);
        Ok(())
    }

    pub fn symbol_dump(&self) -> js_sys::Map {
        let map = js_sys::Map::new();
        for (k, v) in self.interpreter.borrow().interner().strings().enumerate() {
            map.set(&(k + 1).into(), &v.into());
        }
        map
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

struct MagusToJs<'gc> {
    ptr_lib: HashMap<usize, JsValue>,
    null_ptr: magus::ValuePtr<'gc>,
}

impl<'gc> MagusToJs<'gc> {
    fn new(null_ptr: magus::ValuePtr<'gc>) -> Self {
        Self {
            ptr_lib: HashMap::new(),
            null_ptr,
        }
    }

    fn ptr_to_usize(ptr: magus::ValuePtr<'gc>) -> usize {
        (&raw const *ptr.borrow()).addr()
    }

    fn memo(&mut self, ptr: magus::ValuePtr<'gc>, val: JsValue) {
        self.ptr_lib.insert(Self::ptr_to_usize(ptr), val);
    }

    fn _produce(&mut self, ptr: magus::ValuePtr<'gc>, resolver: &magus::lasso::Rodeo) -> JsValue {
        match *ptr.borrow() {
            _ if magus::gc_arena::Gc::ptr_eq(ptr, self.null_ptr) => JsValue::null(),
            magus::Value::Void => JsValue::null(),
            magus::Value::String(s) => JsValue::from_str(s.borrow().as_str()),
            magus::Value::Symbol(s) => JsValue::from_str(resolver.resolve(&s.0)),
            magus::Value::Number(n) => JsValue::from_f64(n.to_inexact()),
            magus::Value::Inexact(f) => JsValue::from_f64(f),
            magus::Value::Bool(b) => JsValue::from_bool(b),
            magus::Value::Vector(v) => {
                let arr = js_sys::Array::new_with_length(v.vec.len() as u32);
                for (i, val) in v.vec.iter().copied().enumerate() {
                    let value = if magus::gc_arena::Gc::ptr_eq(val, ptr) {
                        arr.clone().into()
                    } else {
                        self._produce(val, resolver)
                    };
                    self.memo(val, value.clone());
                    arr.set(i as u32, value);
                }
                arr.into()
            }
            magus::Value::Cons(c) => {
                // These can be self-referential!
                if c.is_list(ptr, self.null_ptr) {
                    let values = c
                        .list_values(ptr, self.null_ptr)
                        .into_iter()
                        .collect::<Vec<_>>();
                    let arr = js_sys::Array::new_with_length(values.len() as u32);
                    for (i, val) in values.into_iter().enumerate() {
                        let value = if magus::gc_arena::Gc::ptr_eq(val, ptr) {
                            arr.clone().into()
                        } else {
                            self._produce(val, resolver)
                        };
                        self.memo(val, value.clone());
                        arr.set(i as u32, value);
                    }
                    arr.into()
                } else {
                    let arr = js_sys::Array::new_with_length(2);
                    let car = c
                        .car
                        .map(|v| {
                            if magus::gc_arena::Gc::ptr_eq(v, ptr) {
                                arr.clone().into()
                            } else {
                                self._produce(v, resolver)
                            }
                        })
                        .unwrap_or(JsValue::null());
                    if let Some(ptr) = c.car {
                        self.memo(ptr, car.clone());
                    }
                    arr.set(0, car);
                    let cdr = c
                        .cdr
                        .map(|v| {
                            if magus::gc_arena::Gc::ptr_eq(v, ptr) {
                                arr.clone().into()
                            } else {
                                self._produce(v, resolver)
                            }
                        })
                        .unwrap_or(JsValue::null());
                    if let Some(ptr) = c.cdr {
                        self.memo(ptr, cdr.clone());
                    }
                    arr.set(1, cdr);
                    arr.into()
                }
            }
            _ => todo!(),
        }
    }

    fn produce(&mut self, ptr: magus::ValuePtr<'gc>, resolver: &magus::lasso::Rodeo) -> JsValue {
        let value = self._produce(ptr, resolver);
        self.memo(ptr, value.clone());
        value
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
            interpreter: Rc::clone(&self.interpreter),
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
                        let mut converter = MagusToJs::new(arena.null_ptr());
                        Ok(if results.is_empty() {
                            JsValue::null()
                        } else if results.len() == 1 {
                            converter.produce(results[0], interner)
                        } else {
                            results
                                .into_iter()
                                .map(|v| converter.produce(v, interner))
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
    interpreter: Rc<RefCell<magus::Interpreter>>,
    thread: magus::ThreadHandle,
    chunk: magus::ChunkHandle,
    source_filename: magus::lasso::Spur,
    source: Rc<str>,
}

#[wasm_bindgen]
impl MagusChunk {
    /// Dump mmemonics for instructions of this chunk
    pub fn instructions(&self) -> Vec<String> {
        self.interpreter.borrow_mut().try_enter(|_, arena, _| {
            let chunk = arena.chunk(&self.chunk);
            chunk.code.iter().map(ToString::to_string).collect()
        })
    }
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
