//! This is the runtime used to execute the R5RS-based script

use core::fmt;
use std::collections::HashMap;

use gc_arena::{Arena, Collect, Gc, Mutation, RefLock, Rootable, Static};
use value::{ConsCell, Value, ValuePtr, ValueVisitor, Vector};

use crate::{
    general_parse,
    general_parser::{gast, GeneralParserError},
    ContainsDatum, Datum, GAstNode as _, Module,
};

/*
macros and special forms are defined here:
special forms are macros that have access to the source of their expansion
and are given in their own unique environment with access to their parent environment

macros resemble piccolo::Sequences in that they must be resumable, but are
simpler in that they only have 3 returns:
Ok(Evaluating) - macro ran out of fuel for expansion, is interacting with something, etc.
Ok(List) - the list this macro should expand into, to be
Err(MacroError) - this macro failed evaluation for some reason

so yeah it's basically a future.
TODO Rip off piccolo::UserData (but w/o metatable stuff)
for UserStruct, then store macros and special forms in the environment
using that!
*/

pub mod any;
pub mod fuel;
pub mod userstruct;
pub mod value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub struct RuntimeKey(usize);

#[derive(Collect)]
#[collect(no_drop)]
pub struct RuntimeArena<'gc> {
    // pub(crate) interpreters: HashMap<RuntimeKey, Gc<'gc, RefLock<Interpreter<'gc>>>>,

    // value that is (eq? '())
    pub(crate) null_val: ValuePtr<'gc>,
}
pub(crate) type RuntimeRoot = Arena<Rootable![RuntimeArena<'_>]>;

impl<'gc> RuntimeArena<'gc> {
    pub fn null(&self) -> ValuePtr<'gc> {
        self.null_val
    }

    pub fn from_iter<
        T: IntoIterator<
            Item = ValuePtr<'gc>,
            IntoIter = impl DoubleEndedIterator<Item = ValuePtr<'gc>>,
        >,
    >(
        &self,
        mc: &Mutation<'gc>,
        iter: T,
    ) -> ValuePtr<'gc> {
        match ConsCell::from_iter(mc, iter) {
            ConsCell {
                car: None,
                cdr: None,
            } => self.null_val,
            cons => {
                let ptr = Gc::new(mc, RefLock::new(Value::Cons(cons)));
                Self::ensure_null(self, mc, ptr);
                ptr
            }
        }
    }

    // Convert ConsCell [ None None ] to root null_val
    pub fn ensure_null(&self, mc: &Mutation<'gc>, value_ptr: ValuePtr<'gc>) {
        let mut ensure_null = EnsureNullVisitor {
            mutation: mc,
            null: &self.null_val.borrow(),
        };
        ensure_null.visit_value(value_ptr);
    }
}

struct EnsureNullVisitor<'a, 'gc> {
    mutation: &'a Mutation<'gc>,
    null: &'a Value<'gc>,
}

impl<'a, 'gc> ValueVisitor<'gc> for EnsureNullVisitor<'a, 'gc> {
    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: ValuePtr<'gc>) {
        if cons.car.is_none() && cons.cdr.is_none() {
            *value.unlock(self.mutation).borrow_mut() = *self.null;
            return;
        }

        if let Some(car) = cons.car {
            self.visit_value(car);
        }

        if let Some(cdr) = cons.cdr {
            self.visit_value(cdr);
        }
    }

    fn visit_vector(&mut self, vec: Vector<'gc>, _value: ValuePtr<'gc>) {
        for elem in vec.vec.borrow().iter() {
            self.visit_value(*elem)
        }
    }
}

pub struct Runtime {
    // TODO Make World also have the gc-arenas for values and rc-refcell (hashmap?) for runtimes
    // so that runtimes can be interacted with stashed.
    // a world is the technical definition of our entire Scheme environment, so this
    // should be ok!
    // FIXME look at how piccolo does stashing
    // INFO actually maybe not? let's see??
    root: RuntimeRoot,
    pub(crate) rodeo: lasso::Rodeo,
}

impl Runtime {}

impl Default for Runtime {
    fn default() -> Self {
        Self {
            root: RuntimeRoot::new(|mc| RuntimeArena {
                // interpreters: HashMap::new(),
                // this value
                null_val: ValuePtr::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
            }),
            rodeo: lasso::Rodeo::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceBundle {
    pub filename: Box<str>,
    pub case_insensitive: bool,
    pub module: Module,
}

#[derive(thiserror::Error, Debug)]
#[error("{} errors in {filename}", errors.len())]
pub struct SourceBundleError {
    pub filename: Box<str>,
    pub code: Box<str>,
    pub errors: Vec<GeneralParserError>,
}

impl SourceBundle {
    pub fn new(
        filename: impl AsRef<str>,
        source: impl AsRef<str>,
        case_insensitive: bool,
    ) -> Result<Self, SourceBundleError> {
        let filename = Box::from(filename.as_ref());
        let source = source.as_ref();
        let gparse = general_parse(source);
        if !gparse.errors().is_empty() {
            let errors = gparse.into_errors();
            let code = Box::from(source);
            Err(SourceBundleError {
                code,
                errors,
                filename,
            })
        } else {
            // if no errors, (well, even if errors)
            // casting the root syntax node (the one returned by GAst::syntax)
            // and unwrapping it is always safe
            Ok(Self {
                filename,
                case_insensitive,
                module: Module::cast(gparse.syntax()).unwrap(),
            })
        }
    }
}
