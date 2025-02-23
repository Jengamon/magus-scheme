//! Executes Chunks (while integrating macros)

use std::{
    ops::Deref,
    sync::{Arc, atomic::AtomicUsize},
};

use gc_arena::{Collect, Gc, Mutation, RefLock, Rootable};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

use crate::value::{ConsCell, Value, ValuePtr};

pub mod thread;

new_key_type! { struct ThreadKey; }
#[derive(Debug)]
struct ThreadMap<'gc> {
    // we'll use SlotMap over HopSlotMap for now, as most of the time
    // we aren't expecting to iterate a *lot* over threads nor have many deleted threads.
    // evaluate this assumption later.
    slotmap: SlotMap<ThreadKey, thread::ThreadPtr<'gc>>,
}

impl ThreadMap<'_> {
    fn new() -> Self {
        Self {
            slotmap: SlotMap::with_key(),
        }
    }
}

unsafe impl<'gc> Collect<'gc> for ThreadMap<'gc> {
    fn trace<T: gc_arena::collect::Trace<'gc>>(&self, cc: &mut T) {
        for thread in self.slotmap.values() {
            thread.trace(cc);
        }
    }
}

#[derive(Debug, Clone)]
pub struct ThreadHandle {
    key: ThreadKey,
    count: Arc<AtomicUsize>,
}

impl PartialEq for ThreadHandle {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}
impl Eq for ThreadHandle {}

impl std::hash::Hash for ThreadHandle {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.hash(state);
    }
}

impl Drop for ThreadHandle {
    fn drop(&mut self) {
        let v = self.count.load(std::sync::atomic::Ordering::SeqCst);
        _ = self.count.compare_exchange(
            v,
            v.saturating_sub(1),
            std::sync::atomic::Ordering::SeqCst,
            std::sync::atomic::Ordering::SeqCst,
        );
    }
}

/// Gc arena type
#[derive(Debug, Collect)]
#[collect(no_drop)]
struct Arena<'gc> {
    // This has an actual meaning, it is Gc::ptr_eq to mean "a null cons"
    null_value: ValuePtr<'gc>,
    // These are for convenience
    true_value: ValuePtr<'gc>,
    false_value: ValuePtr<'gc>,

    threads: ThreadMap<'gc>,
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
struct NullIncluder;

impl Includer for NullIncluder {
    fn include(&self, filename: &str) -> anyhow::Result<Box<str>> {
        Err(anyhow::anyhow!("cannot include {filename}: null includer"))
    }
}

/// Entrypoint of execution.
pub struct Interpreter {
    arena: gc_arena::Arena<Rootable![Arena<'_>]>,
    key_counts: SecondaryMap<ThreadKey, Arc<AtomicUsize>>,
    interner: lasso::Rodeo,
    includer: Box<dyn Includer>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            arena: gc_arena::Arena::new(|mc| Arena {
                null_value: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
                true_value: Gc::new(mc, RefLock::new(Value::Bool(true))),
                false_value: Gc::new(mc, RefLock::new(Value::Bool(false))),
                threads: ThreadMap::new(),
            }),
            key_counts: SecondaryMap::new(),
            interner: lasso::Rodeo::new(),
            includer: Box::new(NullIncluder),
        }
    }
}

impl Interpreter {
    pub fn with_includer(includer: impl Includer + 'static) -> Self {
        Self {
            includer: Box::new(includer),
            ..Default::default()
        }
    }

    /// Checks for any threads where the thread count is 0, and drops those pointers from the threadmap,
    /// eventually freeing the thread when the next collection occurs.
    fn check_for_dropped(&mut self) {
        let keys_to_drop = self
            .key_counts
            .iter()
            .filter_map(|(key, count)| {
                (count.load(std::sync::atomic::Ordering::SeqCst) == 0).then_some(key)
            })
            .collect::<fxhash::FxHashSet<_>>();

        self.arena.mutate_root(|_mc, arena| {
            for key in keys_to_drop {
                arena.threads.slotmap.remove(key);
            }
        })
    }

    pub fn enter(&mut self, handle: ThreadHandle, f: impl FnOnce(Context<'_>, &mut lasso::Rodeo)) {
        self.check_for_dropped();
        self.arena.mutate(|mc, arena| {
            let thread = arena
                .threads
                .slotmap
                .get(handle.key)
                .expect("thread was dropped when a handle still exists");
            let ctx = Context {
                mc,
                thread: *thread,
                null_value: arena.null_value,
                true_value: arena.true_value,
                false_value: arena.false_value,
            };
            (f)(ctx, &mut self.interner);
        })
    }
}
