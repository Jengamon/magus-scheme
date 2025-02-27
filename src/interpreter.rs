//! Executes Chunks (while integrating macros)

use std::{ops::Deref, sync::Arc};

use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

use crate::{
    bytecode, compiler,
    value::{ConsCell, Value, ValuePtr},
};

pub mod thread;

new_key_type! { struct ThreadKey; struct ChunkKey; struct CompilerKey; struct ValueKey; }
#[derive(Debug)]
struct Stash<'gc> {
    // we'll use SlotMap over HopSlotMap for now, as most of the time
    // we aren't expecting to iterate a *lot* over threads nor have many deleted threads.
    // evaluate this assumption later.
    threads: SlotMap<ThreadKey, thread::ThreadPtr<'gc>>,
    chunks: SlotMap<ChunkKey, bytecode::ChunkPtr<'gc>>,
    compilers: SlotMap<CompilerKey, compiler::Compiler<'gc>>,
    values: SlotMap<ValueKey, ValuePtr<'gc>>,
}

impl Stash<'_> {
    fn new() -> Self {
        Self {
            threads: SlotMap::with_key(),
            chunks: SlotMap::with_key(),
            compilers: SlotMap::with_key(),
            values: SlotMap::with_key(),
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
    }
}

macro_rules! handler_type {
    ($v:vis $hn:ident => $k:ty) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        $v struct $hn {
            _knob: Arc<()>,
            key: $k,
        }
    };
}

handler_type!(pub ThreadHandle => ThreadKey);
handler_type!(pub ChunkHandle => ChunkKey);
handler_type!(pub CompilerHandle => CompilerKey);
handler_type!(pub ValueHandle => ValueKey);

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
}

/// Context for execution
#[derive(Clone, Copy)]
pub struct Context<'gc> {
    pub mc: &'gc Mutation<'gc>,
    pub thread: thread::ThreadPtr<'gc>,
    pub null_value: ValuePtr<'gc>,
    pub true_value: ValuePtr<'gc>,
    pub false_value: ValuePtr<'gc>,
}

impl Context<'_> {
    #[cfg(test)]
    pub(crate) fn new_test_context<'gc>(
        mc: &'gc Mutation<'gc>,
        thread: thread::ThreadPtr<'gc>,
    ) -> Context<'gc> {
        Context {
            mc,
            thread,
            null_value: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
            true_value: Gc::new(mc, RefLock::new(Value::Bool(true))),
            false_value: Gc::new(mc, RefLock::new(Value::Bool(false))),
        }
    }
}

impl<'gc> Deref for Context<'gc> {
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
        Err(anyhow::anyhow!("cannot include {filename}: null includer"))
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
            }

            for key in compilers_to_drop {
                arena.stash.compilers.remove(key);
            }

            for key in chunks_to_drop {
                arena.stash.chunks.remove(key);
            }

            for key in values_to_drop {
                arena.stash.values.remove(key);
            }
        })
    }

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
        handle: &CompilerHandle,
        func: impl for<'a> FnOnce(
            &Mutation<'a>,
            &mut compiler::Compiler<'a>,
            &mut lasso::Rodeo,
        ) -> Result<bytecode::ChunkPtr<'a>, E>,
    ) -> Result<ChunkHandle, E> {
        self.check_for_dropped();
        self.arena.mutate_root(|mc, arena| {
            let compiler = arena
                .stash
                .compilers
                .get_mut(handle.key)
                .expect("compiler was dropped when a handle still exists");
            let chunk = (func)(mc, compiler, &mut self.interner)?;
            Ok(arena.create_chunk_handle(chunk))
        })
    }

    pub fn new_thread(&mut self, code: &ChunkHandle) -> ThreadHandle {
        let knob = Arc::new(());
        self.arena.mutate_root(|mc, arena| {
            let chunk = arena
                .stash
                .chunks
                .get(code.key)
                .expect("chunk was deallocated");
            let new_thread = thread::Thread::new(mc, *chunk);
            let key = arena
                .stash
                .threads
                .insert(Gc::new(mc, RefLock::new(new_thread)));
            self.thread_knobs.insert(key, knob.clone());
            ThreadHandle { key, _knob: knob }
        })
    }

    pub fn new_empty_thread(&mut self) -> ThreadHandle {
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

    pub fn enter(
        &mut self,
        handle: &ThreadHandle,
        func: impl for<'a> FnOnce(Context<'a>, &mut Arena<'a>, &mut lasso::Rodeo),
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

    pub fn try_enter<T>(
        &mut self,
        handle: &ThreadHandle,
        func: impl for<'a> FnOnce(Context<'a>, &mut Arena<'a>, &mut lasso::Rodeo) -> T,
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
