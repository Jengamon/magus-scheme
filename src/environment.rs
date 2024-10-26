use std::{
    cell::{Ref, RefMut},
    collections::HashMap,
};

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::Rodeo;

use crate::transformer::macro_to_ptr;
use crate::{transformer::Macro, treewalk::StackValue, value::Symbol};

// TODO make more involved, so that this can be the type stored in Value
// for `environment`
// TODO Allow for "frozen" environments to implement `environment` b/c
// "The bindings of the environment represented by the specifier are immutable, as is the environment itself."
// It should interact with set!, define, and friends to prevent any modification of the environment.
// TODO add separate macro scope, which can only contain transformer objects
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
pub enum FrozenError {
    #[error("environment is frozen")]
    Environment,
    #[error("binding is frozen")]
    Binding,
}

#[derive(thiserror::Error, Debug)]
pub enum RebindError {
    #[error("{0} is not defined")]
    NameNotFound(Box<str>),
    #[error(transparent)]
    Frozen(#[from] FrozenError),
}

impl<'gc> Environment<'gc> {
    pub fn new(mc: &Mutation<'gc>, parent: Option<EnvironmentPtr<'gc>>) -> Self {
        Self {
            parent,
            inner: Gc::new(
                mc,
                RefLock::new(EnvironmentInner {
                    values: HashMap::default(),
                    macros: HashMap::default(),
                }),
            ),
            is_frozen: false,
        }
    }

    /// Freezes this particular copy of the environment.
    ///
    /// Both `define` and `set!` are stopped by a shallow freeze, so it
    /// requires intentional manipulation on the Rust side in order to get through
    /// this freeze (to make constant, use [`Self::deep_freeze`]).
    pub fn freeze(&mut self) {
        self.is_frozen = true;
    }

    /// Sets the frozen flag for the environment and *all* its bindings.
    pub fn deep_freeze(&mut self, mc: &Mutation<'gc>) {
        self.is_frozen = true;
        for binding in self.inner.borrow_mut(mc).values.values_mut() {
            binding.is_frozen = true;
        }
        for binding in self.inner.borrow_mut(mc).macros.values_mut() {
            binding.is_frozen = true;
        }
    }

    pub fn get_macro(&self, name: impl Into<Symbol>) -> Option<MacroBinding<'gc>> {
        let name = name.into();
        if let Some(value) = self.inner.borrow().macros.get(&name) {
            Some(*value)
        } else if let Some(parent) = self.parent {
            parent.borrow().get_macro(name)
        } else {
            None
        }
    }

    pub fn get(&self, name: impl Into<Symbol>) -> Option<Binding<'gc>> {
        let name = name.into();
        if let Some(value) = self.inner.borrow().values.get(&name) {
            Some(*value)
        } else if let Some(parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn rebind_binding(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        binding: Binding<'gc>,
        interner: &Rodeo,
    ) -> Result<StackValue<'gc>, RebindError> {
        if self.is_frozen {
            return Err(FrozenError::Environment)?;
        }

        let name = name.into();
        if let Some(old_binding) = self.inner.borrow_mut(mc).values.get_mut(&name) {
            if old_binding.is_frozen {
                return Err(FrozenError::Binding)?;
            }

            let old_value = *old_binding.value.borrow();
            *old_binding = binding;
            Ok(old_value)
        } else {
            Err(RebindError::NameNotFound(Box::from(
                interner.resolve(&name.0),
            )))
        }
    }

    pub fn rebind(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        value: StackValue<'gc>,
        interner: &Rodeo,
    ) -> Result<StackValue<'gc>, RebindError> {
        if self.is_frozen {
            return Err(FrozenError::Environment)?;
        }

        let name = name.into();
        if let Some(binding) = self.inner.borrow_mut(mc).values.get_mut(&name) {
            if binding.is_frozen {
                return Err(FrozenError::Binding)?;
            }

            let old_value = *binding.value.borrow();
            *binding.value.borrow_mut(mc) = value;
            Ok(old_value)
        } else {
            Err(RebindError::NameNotFound(Box::from(
                interner.resolve(&name.0),
            )))
        }
    }

    /// Creates a new macro binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment is frozen
    pub fn define_macro<M: Macro<'gc> + 'gc>(
        &mut self,
        mc: &'gc Mutation<'gc>,
        name: impl Into<Symbol>,
        mcr: M,
    ) -> Result<Option<MacroBinding<'gc>>, FrozenError> {
        self.define_macro_binding(
            mc,
            name,
            MacroBinding {
                value: macro_to_ptr(mc, mcr),
                is_frozen: false,
            },
        )
    }

    /// Creates a new macro binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment is frozen
    pub fn define_macro_binding(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        binding: MacroBinding<'gc>,
    ) -> Result<Option<MacroBinding<'gc>>, FrozenError> {
        (!self.is_frozen)
            .then(|| {
                self.inner
                    .borrow_mut(mc)
                    .macros
                    .insert(name.into(), binding)
            })
            .ok_or(FrozenError::Environment)
    }

    /// Creates a new binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment is frozen
    pub fn define_binding(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        binding: Binding<'gc>,
    ) -> Result<Option<Binding<'gc>>, FrozenError> {
        (!self.is_frozen)
            .then(|| {
                self.inner
                    .borrow_mut(mc)
                    .values
                    .insert(name.into(), binding)
            })
            .ok_or(FrozenError::Environment)
    }

    /// Creates a new binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment is frozen
    pub fn define(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        value: StackValue<'gc>,
        is_frozen: bool,
    ) -> Result<Option<Binding<'gc>>, FrozenError> {
        self.define_binding(
            mc,
            name,
            Binding {
                value: Gc::new(mc, RefLock::new(value)),
                is_frozen,
            },
        )
    }
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
struct EnvironmentInner<'gc> {
    pub values: HashMap<Symbol, Binding<'gc>>,
    pub macros: HashMap<Symbol, MacroBinding<'gc>>,
}

pub type Binding<'gc> = GeneralBinding<'gc, StackValue<'gc>>;
pub type MacroBinding<'gc> = GeneralBinding<'gc, dyn Macro<'gc> + 'gc>;
// TODO MacroBinding<'gc>

/// Represents the value at a certain location in an environment.
///
/// A particular binding can be frozen to make sure that no change of its held value
/// is made through it. (makes the value *constant*)
#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct GeneralBinding<'gc, P: ?Sized> {
    value: Gc<'gc, RefLock<P>>,
    is_frozen: bool,
}

impl<'gc, P: ?Sized> Clone for GeneralBinding<'gc, P> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'gc, P: ?Sized> Copy for GeneralBinding<'gc, P> {}

impl<'gc, P: ?Sized> GeneralBinding<'gc, P> {
    pub fn get(&self) -> Gc<'gc, RefLock<P>> {
        self.value
    }

    pub fn read<T>(&self, func: impl FnOnce(Ref<P>) -> T) -> T {
        func(self.value.borrow())
    }

    /// Freeze this copy of a binding
    ///
    /// Note: you have to rebind or define as this new binding
    /// in order for other users to be frozen.
    pub fn freeze(&mut self) {
        self.is_frozen = true;
    }

    pub fn write<T>(
        &self,
        mc: &Mutation<'gc>,
        func: impl FnOnce(RefMut<P>) -> T,
    ) -> Result<T, FrozenError> {
        if self.is_frozen {
            Err(FrozenError::Binding)
        } else {
            Ok(func(self.value.borrow_mut(mc)))
        }
    }
}
