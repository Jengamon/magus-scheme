//! Provides utilities for printing values in Scheme

// We are given a value pointer, and we need to figure out all the values it *should* print, respecting modes
// that might be used to avoid getting stuck in loops (or not)

use std::ops::Deref;

use fxhash::FxHashMap;
use gc_arena::Gc;

use super::{ConsCell, ValuePtr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum PrintMode {
    /// do not avoid getting into loops, uses the least memory
    Simple,
    /// mark as a label something that *would* get into a loop
    #[default]
    StopLoop,
    /// mark as a label *anything* that is shared, uses the most memory
    Shared,
}

// Intermediary structure used to hold partial iteration state
#[derive(Debug)]
enum Intermediary<'gc> {
    Vec(Vec<ValuePtr<'gc>>),
    Cons(ConsCell<'gc>),
}

/// Specify how to display a given structure
#[derive(Debug, Clone)]
pub enum PrintEvent<'gc> {
    /// Print (
    StartList,
    /// Print #(
    StartVector,
    /// Print the label corresponding to the given element
    Labeled(ValuePtr<'gc>),
    /// This is *guaranteed* to not be a compound value (vector, cons)
    Element {
        value: ValuePtr<'gc>,
        is_labeled: bool,
    },
    /// Print )
    EndCompound,
}

// Might be useful more broadly...
// instead of storing a ValuePtr directly in a map, we want to override the Eq and PartialEq operators to use the *address* of the pointer rather than
// the value of the pointer
#[derive(Debug)]
struct ValuePtrAddress<'gc> {
    ptr: ValuePtr<'gc>,
}

impl<'gc> From<ValuePtr<'gc>> for ValuePtrAddress<'gc> {
    fn from(ptr: ValuePtr<'gc>) -> Self {
        Self { ptr }
    }
}

impl<'gc> PartialEq<ValuePtr<'gc>> for ValuePtrAddress<'gc> {
    fn eq(&self, other: &ValuePtr<'gc>) -> bool {
        Gc::ptr_eq(self.ptr, *other)
    }
}
impl<'gc> PartialEq for ValuePtrAddress<'gc> {
    fn eq(&self, other: &Self) -> bool {
        // Use pointer equality
        Gc::ptr_eq(self.ptr, other.ptr)
    }
}
impl<'gc> Eq for ValuePtrAddress<'gc> {}

impl<'gc> std::hash::Hash for ValuePtrAddress<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // we hash the *address* of the pointer referrent instead of the value so that
        // even non-Hash data can be Hash (I think)
        (&raw const *self.ptr.borrow()).addr().hash(state);
    }
}

/// Responsible for iterating and outputing the *contents* of the originally given ValuePtr, outputs "PrintEvents" to specify what to print
#[derive(Debug)]
pub struct PrintExplorer<'gc> {
    mode: PrintMode,
    value: ValuePtr<'gc>,

    // Used for preserving state *between* `Iterator::next` calls
    pstack: Vec<Intermediary<'gc>>,
    // Used for storing labeling info (value to be labeled -> has the value been output?)
    labels: FxHashMap<ValuePtrAddress<'gc>, bool>,
}

// PrintExplorer is 2-pass: first pass is the "scan" pass which goes through elements to determine what elements should be labeled, and
// the second pass with is the iterator output (and is delayed until then) which actually outputs the PrintEvents corresponding to
// what elements should be printed by a writer in-order. The second pass is delayed until requested (in the impl of Iterator::next)

impl<'gc> PrintExplorer<'gc> {
    // Returns None if the mode and the given value *would* cause an infinite loop (only should be causable by using Simple mode)
    pub fn new(
        &self,
        mode: PrintMode,
        self_ptr: ValuePtr<'gc>,
        null_ptr: ValuePtr<'gc>,
    ) -> Option<Self> {
        // we do the *first* pass here
        let mut labels = FxHashMap::default();

        // Used to iteratively explore our value
        let mut temp_pstack: Vec<Intermediary> = vec![];

        Some(Self {
            mode,
            value: self_ptr,

            pstack: vec![],
            labels,
        })
    }
}

impl<'gc> Iterator for PrintExplorer<'gc> {
    type Item = PrintEvent<'gc>;
    fn next(&mut self) -> Option<Self::Item> {
        // we do the *second* pass here
        None
    }
}
