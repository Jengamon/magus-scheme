//! Executes Chunks (while integrating macros)

use std::{ops::Deref, sync::Arc};

use anyhow::Context as _;
use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable};
use slotmap::{SecondaryMap, SlotMap, new_key_type};
use thread::{ThreadConfig, ThreadPtr};

use crate::{
    ExternalCompilerContext, LibraryName, World, bytecode,
    compiler::{self, LibraryDeclaration, LibraryDefinitionContext, ParseProgram as _},
    environment::StackEnvironmentPtr,
    handle_type,
    value::{ConsCell, Value, ValuePtr},
};

pub mod thread;

new_key_type! { struct ThreadKey; struct ChunkKey; struct CompilerKey; struct ValueKey; struct EnvironmentKey; }
#[derive(Debug)]
struct Stash<'gc> {
    // we'll use SlotMap over HopSlotMap for now, as most of the time
    // we aren't expecting to iterate a *lot* over threads nor have many deleted threads.
    // evaluate this assumption later.
    threads: SlotMap<ThreadKey, thread::ThreadPtr<'gc>>,
    chunks: SlotMap<ChunkKey, bytecode::ChunkPtr<'gc>>,
    compilers: SlotMap<CompilerKey, compiler::Compiler<'gc>>,
    values: SlotMap<ValueKey, ValuePtr<'gc>>,
    envs: SlotMap<EnvironmentKey, StackEnvironmentPtr<'gc>>,
}

impl Stash<'_> {
    fn new() -> Self {
        Self {
            threads: SlotMap::with_key(),
            chunks: SlotMap::with_key(),
            compilers: SlotMap::with_key(),
            values: SlotMap::with_key(),
            envs: SlotMap::with_key(),
        }
    }
}

#[allow(unsafe_code)]
unsafe impl<'gc> Collect<'gc> for Stash<'gc> {
    fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
        macro_rules! trace_slotmap {
            ($field:ident) => {
                for value in self.$field.values() {
                    value.trace(cc);
                }
            };
        }

        trace_slotmap!(threads);
        trace_slotmap!(chunks);
        trace_slotmap!(compilers);
        trace_slotmap!(values);
        trace_slotmap!(envs);
    }
}

handle_type!(pub ThreadHandle => ThreadKey);
handle_type!(pub ChunkHandle => ChunkKey);
handle_type!(pub CompilerHandle => CompilerKey);
handle_type!(pub ValueHandle => ValueKey);
handle_type!(pub EnvironmentHandle => EnvironmentKey);

/// Gc arena type
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Arena<'gc> {
    // This has an actual meaning, it is Gc::ptr_eq to mean "a null cons"
    null_value: ValuePtr<'gc>,
    // These are for convenience
    true_value: ValuePtr<'gc>,
    false_value: ValuePtr<'gc>,

    stash: Stash<'gc>,
    #[collect(require_static)]
    chunk_knobs: SecondaryMap<ChunkKey, Arc<()>>,
    #[collect(require_static)]
    value_knobs: SecondaryMap<ValueKey, Arc<()>>,
}

impl<'gc> Arena<'gc> {
    fn create_chunk_handle(&mut self, chunk: bytecode::ChunkPtr<'gc>) -> ChunkHandle {
        let knob = Arc::new(());
        let key = self.stash.chunks.insert(chunk);
        ChunkHandle { _knob: knob, key }
    }

    pub fn value_pointers(&self) -> ValuePointers<'gc> {
        ValuePointers {
            null_value: self.null_value,
            true_value: self.true_value,
            false_value: self.false_value,
        }
    }

    // TODO These functions should return the thing, not an Option
    // a failure to find means that somehow a handle outlived the existence of the thing,
    // and thats not very nice
    pub fn get_chunk(&self, handle: &ChunkHandle) -> Option<bytecode::ChunkPtr<'gc>> {
        self.stash.chunks.get(handle.key).copied()
    }

    pub fn stash_value(&mut self, value: ValuePtr<'gc>) -> ValueHandle {
        let knob = Arc::new(());
        let key = self.stash.values.insert(value);
        ValueHandle { _knob: knob, key }
    }

    pub fn get_value(&self, handle: &ValueHandle) -> Option<ValuePtr<'gc>> {
        self.stash.values.get(handle.key).copied()
    }

    pub fn compiler(&self, handle: &CompilerHandle) -> Option<&compiler::Compiler<'gc>> {
        self.stash.compilers.get(handle.key)
    }

    pub fn compiler_mut(
        &mut self,
        handle: &CompilerHandle,
    ) -> Option<&mut compiler::Compiler<'gc>> {
        self.stash.compilers.get_mut(handle.key)
    }

    pub fn thread(&self, handle: &ThreadHandle) -> ThreadPtr<'gc> {
        self.stash
            .threads
            .get(handle.key)
            .copied()
            .expect("handle exists to freed thread")
    }
}

/// Context for execution
#[derive(Clone, Copy)]
pub struct Context<'a, 'gc> {
    pub mc: &'a Mutation<'gc>,
    pub thread: thread::ThreadPtr<'gc>,
    pub null_value: ValuePtr<'gc>,
    pub true_value: ValuePtr<'gc>,
    pub false_value: ValuePtr<'gc>,
}

impl Context<'_, '_> {
    #[cfg(test)]
    pub(crate) fn new_test_context<'a, 'gc>(
        mc: &'a Mutation<'gc>,
        thread: thread::ThreadPtr<'gc>,
    ) -> Context<'a, 'gc> {
        Context {
            mc,
            thread,
            null_value: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
            true_value: Gc::new(mc, RefLock::new(Value::Bool(true))),
            false_value: Gc::new(mc, RefLock::new(Value::Bool(false))),
        }
    }
}

impl<'gc> Deref for Context<'_, 'gc> {
    type Target = Mutation<'gc>;
    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

/// The mechanism behind `include` and `include-ci`
pub trait Includer {
    fn include(&self, filename: &str) -> anyhow::Result<Box<str>>;
}

/// Default includer that always results in an error
pub struct NullIncluder;
impl Includer for NullIncluder {
    fn include(&self, filename: &str) -> anyhow::Result<Box<str>> {
        Err(anyhow::anyhow!(
            "cannot include `{filename}`: null includer"
        ))
    }
}

/// A way to nicely register libraries into the compiler of an interpreter
pub trait Registerable {
    /// The name that the module itself want to use
    fn name(interner: &mut lasso::Rodeo) -> LibraryName;
    /// If Some, there is a native component of the library
    fn native(&self) -> Option<Arc<dyn compiler::Module + Send + Sync + 'static>>;
    /// If Some, there is a Scheme component of the library
    ///
    /// Returns (source filename, source code)
    fn scheme(&self) -> Option<(&str, &str)>;
    /// Libraries used in the interpretation of Scheme code of a library
    fn scheme_native(
        &self,
        interner: &mut lasso::Rodeo,
    ) -> Vec<(
        LibraryName,
        std::sync::Arc<dyn compiler::Module + Send + Sync + 'static>,
    )>;
    /// Libraries used in the interpretation of Scheme code that should be
    /// registered before this one. Used to mark depenence on another module
    /// (where [`Self::scheme_native`] is either self dependence or a private module)
    fn scheme_dependency(&self, interner: &mut lasso::Rodeo) -> Vec<LibraryName> {
        let _ = interner;
        Vec::new()
    }
}

/// Pointers used to represent `'()`, `#t` and `#f`
///
/// Only the `'()` pointer's value has semantic meaning.
/// the `#t` and `#f` pointers are for convenience (and to avoid repeated allocation)
#[derive(Debug, Clone, Copy)]
pub struct ValuePointers<'gc> {
    pub(crate) null_value: ValuePtr<'gc>,
    pub(crate) true_value: ValuePtr<'gc>,
    pub(crate) false_value: ValuePtr<'gc>,
}

#[allow(clippy::needless_lifetimes)]
impl<'gc> ValuePointers<'gc> {
    /// Create "fake" values for testing. Should *only* be used for testing.
    pub fn fake(mc: &'gc Mutation<'gc>) -> ValuePointers<'gc> {
        Self {
            null_value: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
            true_value: Gc::new(mc, RefLock::new(Value::Bool(true))),
            false_value: Gc::new(mc, RefLock::new(Value::Bool(false))),
        }
    }
}

/// Entrypoint of execution.
pub struct Interpreter {
    arena: gc_arena::Arena<Rootable![Arena<'_>]>,
    thread_knobs: SecondaryMap<ThreadKey, Arc<()>>,
    compiler_knobs: SecondaryMap<CompilerKey, Arc<()>>,
    interner: lasso::Rodeo,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            arena: gc_arena::Arena::new(|mc| Arena {
                null_value: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
                true_value: Gc::new(mc, RefLock::new(Value::Bool(true))),
                false_value: Gc::new(mc, RefLock::new(Value::Bool(false))),
                chunk_knobs: SecondaryMap::new(),
                value_knobs: SecondaryMap::new(),
                stash: Stash::new(),
            }),
            thread_knobs: SecondaryMap::new(),
            compiler_knobs: SecondaryMap::new(),
            interner: lasso::Rodeo::new(),
        }
    }
}

impl Interpreter {
    /// Checks for any threads where the thread count is 0, and drops those pointers from the threadmap,
    /// eventually freeing the thread when the next collection occurs.
    fn check_for_dropped(&mut self) {
        let threads_to_drop = self
            .thread_knobs
            .iter()
            // The strong count is 1 when the Arc in this map is the *only* reference it its value, which means that any external
            // handle has been dropped
            .filter_map(|(key, knob)| (Arc::strong_count(knob) == 1).then_some(key))
            .collect::<fxhash::FxHashSet<_>>();
        let compilers_to_drop = self
            .compiler_knobs
            .iter()
            // The strong count is 1 when the Arc in this map is the *only* reference it its value, which means that any external
            // handle has been dropped
            .filter_map(|(key, knob)| (Arc::strong_count(knob) == 1).then_some(key))
            .collect::<fxhash::FxHashSet<_>>();

        self.arena.mutate_root(|_mc, arena| {
            let chunks_to_drop = arena
                .chunk_knobs
                .iter()
                .filter_map(|(key, knob)| (Arc::strong_count(knob) == 1).then_some(key))
                .collect::<fxhash::FxHashSet<_>>();
            let values_to_drop = arena
                .value_knobs
                .iter()
                .filter_map(|(key, knob)| (Arc::strong_count(knob) == 1).then_some(key))
                .collect::<fxhash::FxHashSet<_>>();

            for key in threads_to_drop {
                arena.stash.threads.remove(key);
                self.thread_knobs.remove(key);
            }

            for key in compilers_to_drop {
                arena.stash.compilers.remove(key);
                self.compiler_knobs.remove(key);
            }

            for key in chunks_to_drop {
                arena.stash.chunks.remove(key);
                arena.chunk_knobs.remove(key);
            }

            for key in values_to_drop {
                arena.stash.values.remove(key);
                arena.value_knobs.remove(key);
            }
        });

        if self.arena.metrics().allocation_debt() > 10_000.0 {
            // once we have 10 kb allocated, start trying to collect memory
            // (this is a failsafe)
            // TODO provide a way to configure this cap
            self.arena.collect_debt();
        }
    }

    // Expose collection methods
    pub fn mark_debt(&mut self) {
        self.arena.mark_debt();
    }

    pub fn finish_marking(&mut self) {
        self.arena.finish_marking();
    }

    pub fn cycle_debt(&mut self) {
        self.arena.cycle_debt();
    }

    pub fn collect_debt(&mut self) {
        self.arena.collect_debt();
    }

    pub fn finish_cycle(&mut self) {
        self.arena.finish_cycle();
    }

    // GC observability, so you can terminate a program if it's lost in the sauce
    pub fn metrics(&self) -> &gc_arena::metrics::Metrics {
        self.arena.metrics()
    }

    pub fn collection_phase(&self) -> gc_arena::arena::CollectionPhase {
        self.arena.collection_phase()
    }

    // Symbols, and immutable strings tend to be stored here
    pub fn interner(&self) -> &lasso::Rodeo {
        &self.interner
    }

    pub fn interner_mut(&mut self) -> &mut lasso::Rodeo {
        &mut self.interner
    }

    pub fn new_compiler(&mut self) -> CompilerHandle {
        let knob = Arc::new(());
        self.arena.mutate_root(|mc, arena| {
            let new_compiler = compiler::Compiler::new(mc);
            let key = arena.stash.compilers.insert(new_compiler);
            self.compiler_knobs.insert(key, knob.clone());
            CompilerHandle { _knob: knob, key }
        })
    }

    pub fn compiler_context<E>(
        &mut self,
        thread: &ThreadHandle,
        handle: &CompilerHandle,
        func: impl for<'a> FnOnce(
            &Mutation<'a>,
            &mut compiler::Compiler<'a>,
            ValuePointers<'a>,
            ThreadPtr<'a>,
            &mut lasso::Rodeo,
        ) -> Result<bytecode::ChunkPtr<'a>, E>,
    ) -> Result<ChunkHandle, E> {
        self.check_for_dropped();
        self.arena.mutate_root(|mc, arena| {
            let pointers = arena.value_pointers();
            let compiler = arena
                .stash
                .compilers
                .get_mut(handle.key)
                .expect("compiler was dropped when a handle still exists");
            let thread = arena
                .stash
                .threads
                .get(thread.key)
                .copied()
                .expect("handle to freed thread");
            let chunk = (func)(mc, compiler, pointers, thread, &mut self.interner)?;
            Ok(arena.create_chunk_handle(chunk))
        })
    }

    /// Register a module to a compiler under a certain name
    pub fn register_module<'a, R: Registerable>(
        &'a mut self,
        thread: &ThreadHandle,
        handle: &CompilerHandle,
        world: &mut World,
        module: R,
        rename: Option<LibraryName>,
        library_def_fn: impl for<'gc> FnOnce(
            ThreadPtr<'gc>,
            ValuePointers<'gc>,
        ) -> LibraryDefinitionContext<'a, 'gc>,
    ) -> anyhow::Result<()> {
        let library_name = rename.unwrap_or_else(|| R::name(&mut self.interner));
        let maybe_native = module.native();
        if let Some(native) = maybe_native {
            // There is a native library component
            world
                .insert_arc(library_name.clone(), native)
                .context(format!(
                    "failed to register module {}",
                    std::any::type_name::<R>()
                ))?;
        }

        if let Some(source_data) = module.scheme() {
            let mut dependencies = module.scheme_dependency(&mut self.interner);
            // There is a local scheme component
            let world = {
                let mut new_world = World::default();
                for (name, module) in module.scheme_native(&mut self.interner) {
                    new_world.insert_arc(name, module).context(format!(
                        "failed to register module {}",
                        std::any::type_name::<R>()
                    ))?;
                }
                // Register dependencies (from the old world)
                for name in dependencies.clone().into_iter() {
                    if let Some(module) = world.library_arc(&name).cloned() {
                        new_world.insert_arc(name.clone(), module).context(format!(
                            "failed to register module {}",
                            std::any::type_name::<R>()
                        ))?;
                        dependencies.retain(|a| a != &name);
                    }
                }
                new_world
            };

            self.arena
                .mutate_root(|mc, arena| {
                    let programs = source_data.parse_program(mc, &mut self.interner, false)?;
                    let library_decls = programs
                        .into_iter()
                        .map(|p| LibraryDeclaration::convert(p, mc, &mut self.interner))
                        .collect::<Result<Vec<_>, _>>()?;
                    let value_pointers = arena.value_pointers();
                    let thread = arena
                        .stash
                        .threads
                        .get(thread.key)
                        .copied()
                        .expect("handle to freed thread");
                    let compiler = arena
                        .compiler_mut(handle)
                        .ok_or(anyhow::anyhow!("invalid compiler handle"))?;
                    // For any remaining dependency, it *has* to exist on the compiler's local_world
                    for name in dependencies {
                        if !compiler.has_local_library(&name) {
                            return Err(anyhow::anyhow!(
                                "failed to find dependency {}",
                                name.to_string(&self.interner)
                            ));
                        }
                    }
                    let mut ecc = ExternalCompilerContext {
                        world: &world,
                        // For module source code, the includer should *always* be NullIncluder
                        // so module source code *has* to be local, and can't include arbitrary
                        // files.
                        includer: &NullIncluder,
                        interner: &mut self.interner,
                    };
                    let library_def = library_def_fn(thread, value_pointers);
                    compiler.define_library(
                        mc,
                        &library_name,
                        &mut ecc,
                        false,
                        &library_def,
                        library_decls,
                    )?;
                    Ok::<_, anyhow::Error>(())
                })
                .context(format!(
                    "failed to register module {}",
                    std::any::type_name::<R>()
                ))?;
        }

        Ok(())
    }

    pub fn new_thread(&mut self) -> ThreadHandle {
        let knob = Arc::new(());
        self.arena.mutate_root(|mc, arena| {
            let new_thread = thread::Thread::default();
            let key = arena
                .stash
                .threads
                .insert(Gc::new(mc, RefLock::new(new_thread)));
            self.thread_knobs.insert(key, knob.clone());
            ThreadHandle { key, _knob: knob }
        })
    }

    pub fn new_thread_with_config(&mut self, config: ThreadConfig) -> ThreadHandle {
        let knob = Arc::new(());
        self.arena.mutate_root(|mc, arena| {
            let new_thread = thread::Thread::with_config(config);
            let key = arena
                .stash
                .threads
                .insert(Gc::new(mc, RefLock::new(new_thread)));
            self.thread_knobs.insert(key, knob.clone());
            ThreadHandle { key, _knob: knob }
        })
    }

    pub fn enter(
        &mut self,
        func: impl for<'a> FnOnce(&'a Mutation<'a>, &mut Arena<'a>, &mut lasso::Rodeo),
    ) {
        self.check_for_dropped();
        self.arena.mutate_root(|mc, arena| {
            (func)(mc, arena, &mut self.interner);
        })
    }

    pub fn try_enter<T>(
        &mut self,
        func: impl for<'a> FnOnce(&'a Mutation<'a>, &mut Arena<'a>, &mut lasso::Rodeo) -> T,
    ) -> T {
        self.check_for_dropped();
        self.arena
            .mutate_root(|mc, arena| (func)(mc, arena, &mut self.interner))
    }

    pub fn run(
        &mut self,
        handle: &ThreadHandle,
        func: impl for<'a> FnOnce(Context<'_, 'a>, &mut Arena<'a>, &mut lasso::Rodeo),
    ) {
        self.check_for_dropped();
        self.arena.mutate_root(|mc, arena| {
            let thread = arena
                .stash
                .threads
                .get(handle.key)
                .expect("thread was dropped when a handle still exists");
            let ctx = Context {
                mc,
                thread: *thread,
                null_value: arena.null_value,
                true_value: arena.true_value,
                false_value: arena.false_value,
            };
            (func)(ctx, arena, &mut self.interner);
        })
    }

    pub fn try_run<T>(
        &mut self,
        handle: &ThreadHandle,
        func: impl for<'a> FnOnce(Context<'_, 'a>, &mut Arena<'a>, &mut lasso::Rodeo) -> T,
    ) -> T {
        self.check_for_dropped();
        self.arena.mutate_root(|mc, arena| {
            let thread = arena
                .stash
                .threads
                .get(handle.key)
                .expect("thread was dropped when a handle still exists");
            let ctx = Context {
                mc,
                thread: *thread,
                null_value: arena.null_value,
                true_value: arena.true_value,
                false_value: arena.false_value,
            };
            (func)(ctx, arena, &mut self.interner)
        })
    }
}
