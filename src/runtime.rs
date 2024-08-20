//! runtimes are responsible for reading worlds and executing on its contents (relative to a specific program)

use core::fmt;
use std::rc::Rc;

use gc_arena::{unsize, Collect, Gc, Mutation};
use lasso::Rodeo;

use crate::world::{fuel::Fuel, value::ValuePtr};

// first runtime will be the slowest: treewalk!
pub mod interpreter;

/// Errors as held in the program
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub enum Error<'gc> {
    Value(ValuePtr<'gc>),
    Static(Gc<'gc, BoxError>),
}

#[derive(Collect, Clone, Debug)]
#[collect(require_static)]
pub struct BoxError(pub Rc<dyn std::error::Error>);

// It is up to runtimes to implement procedures
#[derive(Collect, Clone, Copy, Debug)]
#[collect(no_drop)]
pub enum Procedure<'gc> {
    Treewalk(Treewalk<'gc>),
    NativeProcedure(NativeProcedure<'gc>),
}

/*
    (+ (+ 1 2) 3)

    we should transform this into

    evaluate 1 -> a
    evaluate 2 -> b
    evaluate + given a b -> c
    evaluate 3 -> d
    evaluate + given c d -> e
    result is e

*/

/// A frame meant to be interpreted by tree-walking
#[derive(Debug, Clone, Copy, Collect)]
#[collect(no_drop)]
pub struct Treewalk<'gc> {
    // a structure that holds a mapping of symbol to ValuePtr
    env: Gc<'gc, ()>,
}

#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct NativeProcedure<'gc>(Gc<'gc, dyn Callback>);
impl<'gc> NativeProcedure<'gc> {
    pub fn new<T: Callback + 'gc>(mc: &Mutation<'gc>, cb: T) -> Self {
        let cb = Gc::new(mc, cb);
        Self(unsize!(cb => dyn Callback))
    }
}

impl<'gc> fmt::Debug for NativeProcedure<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.0.name() {
            write!(f, "#<procedure {name} {:p}>", self.0)
        } else {
            write!(f, "#<procedure {:p}>", self.0)
        }
    }
}

/// A native callback is quite simple: it must map from
/// a Vec<Value<'gc>> -> Result<Value<'gc>, Error<'gc>>
pub trait Callback: Collect {
    fn name(&self) -> Option<&str> {
        None
    }

    fn call<'gc>(
        &mut self,
        mc: &Mutation<'gc>,
        fuel: &mut Fuel,
        interner: &mut Rodeo,
        args: Vec<ValuePtr<'gc>>,
    ) -> Result<ValuePtr<'gc>, Error<'gc>>;
}
