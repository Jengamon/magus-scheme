use std::cell::RefMut;

use gc_arena::Mutation;
use lasso::Rodeo;

use crate::value::{self, ValuePtr};

pub struct Context<'gc> {
    pub mutation: &'gc Mutation<'gc>,
    pub interner: RefMut<'gc, Rodeo>,
    pub null_ptr: ValuePtr<'gc>,
    // symbol for "quote" from the interner
    pub(crate) quote_sym: value::Symbol,
}
