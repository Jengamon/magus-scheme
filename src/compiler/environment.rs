use std::{
    cell::{Ref, RefMut},
    collections::HashMap,
};

use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::value::{Value, ValuePtr};

// TODO make more involved, so that this can be the type stored in Value
// for `environment`
// TODO Allow for "frozen" environments to implement `environment` b/c
// "The bindings of the environment represented by the specifier are immutable, as is the environment itself."
// It should interact with set!, define, and friends to prevent any modification of the environment.
/// Environemnts define the context for execution, with variable mappings
/// macro definitions, and up to 1 reference to a parent environment
#[derive(Collect, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Environment<'gc> {
    parent: Option<EnvironmentPtr<'gc>>,
    inner: Gc<'gc, RefLock<EnvironmentInner<'gc>>>,
    /// This will make all [`Self::define`]s fail as it makes the
    /// bindings immutable
    is_frozen: bool,
}
pub type EnvironmentPtr<'gc> = Gc<'gc, RefLock<Environment<'gc>>>;

#[derive(thiserror::Error, Debug)]
#[error("environment is frozen")]
pub struct FrozenError;

impl<'gc> Environment<'gc> {
    pub fn new(mc: &Mutation<'gc>, parent: Option<EnvironmentPtr<'gc>>) -> Self {
        Self {
            parent,
            inner: Gc::new(
                mc,
                RefLock::new(EnvironmentInner {
                    values: HashMap::default(),
                }),
            ),
            is_frozen: false,
        }
    }

    /// Sets the frozen flag. This is not a reversible operation.
    pub fn freeze(&mut self, mc: &Mutation<'gc>) {
        self.is_frozen = true;
        for binding in self.inner.borrow_mut(mc).values.values_mut() {
            binding.is_frozen = true;
        }
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<Binding<'gc>> {
        if let Some(value) = self.inner.borrow().values.get(name.as_ref()) {
            Some(*value)
        } else if let Some(parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    /// Creates a new binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment the definition is attempted in is frozen
    pub fn define(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl AsRef<str>,
        value: ValuePtr<'gc>,
        is_frozen: bool,
    ) -> Result<Option<Binding<'gc>>, FrozenError> {
        self.is_frozen
            .then(|| {
                self.inner
                    .borrow_mut(mc)
                    .values
                    .insert(Box::from(name.as_ref()), Binding { value, is_frozen })
            })
            .ok_or(FrozenError)
    }
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
struct EnvironmentInner<'gc> {
    pub values: HashMap<Box<str>, Binding<'gc>>,
}

/// Represents the value at a certain location in an environment.
///
/// A particular binding can be frozen to make sure that no change of its held value
/// is made through it.
#[derive(Collect, Debug, Clone, Copy)]
#[collect(no_drop)]
pub struct Binding<'gc> {
    value: ValuePtr<'gc>,
    is_frozen: bool,
}

impl<'gc> Binding<'gc> {
    pub fn read<T>(&self, func: impl FnOnce(Ref<Value<'gc>>) -> T) -> T {
        func(self.value.borrow())
    }

    pub fn write<T>(
        &self,
        mc: &Mutation<'gc>,
        func: impl FnOnce(RefMut<Value<'gc>>) -> T,
    ) -> Option<T> {
        if self.is_frozen {
            None
        } else {
            Some(func(self.value.borrow_mut(mc)))
        }
    }
}
