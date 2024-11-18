use gc_arena::{Collect, Gc, Mutation, RefLock};
use rowan::TextRange;

use crate::{runtime::convert::IntoValue, value::ValuePtr};

use super::virtual_inst::CodeChunkPtr;

/// A value pointer enhanced with source tracking information
#[derive(Debug, Clone, Collect, Copy)]
#[collect(no_drop)]
pub struct StackValue<'gc> {
    pub(crate) value: ValuePtr<'gc>,
    #[collect(require_static)]
    pub(crate) range: Option<TextRange>,
    /// preservation of touch_count when used as data
    pub(crate) touch_count: Gc<'gc, RefLock<usize>>,
    /// If this value has been JIT'ed, what is the chunk
    /// If it hasn't, store if it is possible to JIT (if we haven't tried already)
    pub(crate) chunk: Result<CodeChunkPtr<'gc>, Gc<'gc, RefLock<bool>>>,
    /// Source id from VirtualInstruction
    pub(crate) source_id: Option<usize>,
}

impl<'gc> StackValue<'gc> {
    pub fn touch_count(&self) -> usize {
        *self.touch_count.borrow()
    }

    /// This is for stack values generated externally, not from anywhere in the source
    pub fn external<V>(mc: &Mutation<'gc>, v: V) -> Self
    where
        V: IntoValue<'gc>,
    {
        Self {
            value: Gc::new(mc, RefLock::new(v.into_value(mc))),
            range: None,
            source_id: None,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
        }
    }
}

/// Transparently access the value of this pointer
impl<'gc> std::ops::Deref for StackValue<'gc> {
    type Target = ValuePtr<'gc>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
