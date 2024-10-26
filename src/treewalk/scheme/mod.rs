//! Base objects for Scheme
//!
//! Some of these require closer access to the actual executor context.
//! (looking at you, call/cc), so are implemented here to keep those private.

// when macros are more dynamic, the code to implement them would be stored here

// we copy the module structure of the standard R7RS library

mod base;

pub use base::SchemeBase;
use gc_arena::{Collect, Gc, Mutation, RefLock};

#[derive(Collect)]
#[collect(no_drop)]
pub struct SchemeStd<'gc> {
    pub(crate) base: Gc<'gc, RefLock<Option<SchemeBase<'gc>>>>,
}

impl<'gc> SchemeStd<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            base: Gc::new(mc, RefLock::new(None)),
        }
    }

    pub fn base(&self, mc: &Mutation<'gc>) -> SchemeBase<'gc> {
        let mut base = self.base.borrow_mut(mc);
        if let Some(base) = *base {
            base
        } else {
            let nbase = SchemeBase::new(mc);
            *base = Some(nbase);
            nbase
        }
    }
}
