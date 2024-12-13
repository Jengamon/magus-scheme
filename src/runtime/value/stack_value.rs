use gc_arena::{Collect, Gc, Mutation, RefLock};
use rowan::TextRange;

use crate::runtime::convert::IntoValue;

use super::ValuePtr;

#[derive(Debug, Clone, Copy)]
pub enum SourceData {
    /// a value directly written in source
    Direct { source_id: usize, range: TextRange },
    /// a value generated from source at a location
    /// (whether lambda or macro)
    Computed { source_id: usize, range: TextRange },

    /// a value with no source marked
    External,
}

/// A value pointer enhanced with source tracking information
#[derive(Debug, Clone, Collect, Copy)]
#[collect(no_drop)]
pub struct StackValue<'gc> {
    pub(crate) value: ValuePtr<'gc>,
    #[collect(require_static)]
    pub(crate) source_data: SourceData,
}

impl<'gc> StackValue<'gc> {
    pub fn source_data(&self) -> SourceData {
        self.source_data
    }

    /// This is for stack values generated externally, not from anywhere in the source
    pub fn external<V>(mc: &Mutation<'gc>, v: V) -> Self
    where
        V: IntoValue<'gc>,
    {
        Self {
            value: Gc::new(mc, RefLock::new(v.into_value(mc))),
            source_data: SourceData::External,
        }
    }

    /// This is for stack values that should be marked as coming from a source (even if
    /// it doesn't directly)
    pub fn from_source<V>(mc: &Mutation<'gc>, v: V, source_id: usize, range: TextRange) -> Self
    where
        V: IntoValue<'gc>,
    {
        Self {
            value: Gc::new(mc, RefLock::new(v.into_value(mc))),
            source_data: SourceData::Computed { source_id, range },
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
