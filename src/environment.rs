use std::{
    cell::{Ref, RefMut},
    collections::HashMap,
};

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::Rodeo;

use crate::value::{Symbol, ValuePtr};

// Big important typedef
// pub type StackEnvironment<'gc> = Environment<'gc, StackValue<'gc>>;
// pub type StackEnvironmentPtr<'gc> = EnvironmentPtr<'gc, StackValue<'gc>>;

#[macro_export]
macro_rules! create_environment_pair {
    ($name:ident => $tp:ty) => {
        use paste::paste;
        paste! {type [< $name Environment >]<'gc> = $crate::environment::Environment<'gc, $tp>;}
        paste! {type [< $name EnvironmentPtr >]<'gc> = $crate::environment::EnvironmentPtr<'gc, $tp>;}
    };

    ($vis:vis $name:ident => $tp:ty) => {
        use paste::paste;
        paste! {$vis type [< $name Environment >]<'gc> = $crate::environment::Environment<'gc, $tp>;}
        paste! {$vis type [< $name EnvironmentPtr >]<'gc> = $crate::environment::EnvironmentPtr<'gc, $tp>;}
    };
}

create_environment_pair!(
    pub Stack => ValuePtr<'gc>
);

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
pub struct Environment<'gc, V: Collect<'gc>> {
    parent: Option<EnvironmentPtr<'gc, V>>,
    inner: Gc<'gc, RefLock<EnvironmentInner<'gc, V>>>,
    /// This will make all [`Self::define`]s fail as it makes the
    /// bindings immutable
    is_frozen: bool,
}
pub type EnvironmentPtr<'gc, V> = Gc<'gc, RefLock<Environment<'gc, V>>>;

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

impl<'gc, V: Collect<'gc> + Copy> Environment<'gc, V> {
    pub fn rebind_binding(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        binding: GeneralBinding<'gc, V>,
        interner: &Rodeo,
    ) -> Result<V, RebindError> {
        if self.is_frozen {
            return Err(FrozenError::Environment)?;
        }

        let name = name.into();
        if let Some(old_binding) = self.inner.borrow_mut(mc).values.get_mut(&name) {
            if old_binding.is_frozen {
                return Err(FrozenError::Binding)?;
            }

            let old_value = *old_binding.value.borrow();
            *old_binding.value.borrow_mut(mc) = *binding.value.borrow();
            old_binding.is_frozen = binding.is_frozen;
            Ok(old_value)
        } else if let Some(parent) = self.parent {
            // We failed to bind, so bind in the parent (if it can)
            parent
                .borrow_mut(mc)
                .rebind_binding(mc, name, binding, interner)
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
        value: V,
        interner: &Rodeo,
    ) -> Result<V, RebindError> {
        self.rebind_binding(
            mc,
            name,
            GeneralBinding {
                value: Gc::new(mc, RefLock::new(value)),
                is_frozen: false,
            },
            interner,
        )
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GetError {
    #[error("environment could not find the given symbol")]
    NameNotFound(Symbol),
    #[error("environment reached maximum parent search depth")]
    TooFar,
}

impl<'gc, V: Collect<'gc>> Environment<'gc, V> {
    pub fn new(mc: &Mutation<'gc>, parent: Option<EnvironmentPtr<'gc, V>>) -> Self {
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

    #[inline]
    // pub(crate) fn reparent(&mut self, new_parent: Option<EnvironmentPtr<'gc, V>>) {
    pub fn reparent(&mut self, new_parent: Option<EnvironmentPtr<'gc, V>>) {
        self.parent = new_parent;
    }

    /// Freezes this particular copy of the environment.
    ///
    /// Both `define` and `set!` are stopped by a shallow freeze, so it
    /// requires intentional manipulation on the Rust side in order to get through
    /// this freeze.
    #[inline]
    pub fn freeze(&mut self) {
        self.is_frozen = true;
    }

    /// Check if no names are defined by this environment
    pub fn is_empty(&self) -> bool {
        self.inner.borrow().values.is_empty()
    }

    /// Sets the frozen flag for all bindings in this environment.
    #[inline]
    pub fn inner_freeze(&mut self, mc: &Mutation<'gc>) {
        for binding in self.inner.borrow_mut(mc).values.values_mut() {
            binding.is_frozen = true;
        }
    }

    pub fn get(&self, name: impl Into<Symbol>) -> Result<GeneralBinding<'gc, V>, GetError> {
        self.get_internal(name, 0)
    }

    /// Maimum number of envs we can recurse into before we "hide" and say None
    const MAX_RECURSION: usize = 2048;

    fn get_internal(
        &self,
        name: impl Into<Symbol>,
        level: usize,
    ) -> Result<GeneralBinding<'gc, V>, GetError> {
        let name = name.into();
        if let Some(value) = self.inner.borrow().values.get(&name) {
            Ok(*value)
        } else if let Some(parent) = self.parent {
            if level >= Self::MAX_RECURSION {
                return Err(GetError::TooFar);
            }
            parent.borrow().get_internal(name, level + 1)
        } else {
            Err(GetError::NameNotFound(name))
        }
    }

    /// Creates a new binding in the current environment, replacing any binding that might already exist
    /// which is returned is successful.
    ///
    /// Fails if the environment is frozen
    pub fn define_binding(
        &mut self,
        mc: &Mutation<'gc>,
        name: impl Into<Symbol>,
        binding: GeneralBinding<'gc, V>,
    ) -> Result<Option<GeneralBinding<'gc, V>>, FrozenError> {
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
        value: V,
        is_frozen: bool,
    ) -> Result<Option<GeneralBinding<'gc, V>>, FrozenError> {
        self.define_binding(
            mc,
            name,
            GeneralBinding {
                value: Gc::new(mc, RefLock::new(value)),
                is_frozen,
            },
        )
    }
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
struct EnvironmentInner<'gc, V: Collect<'gc>> {
    pub values: HashMap<Symbol, GeneralBinding<'gc, V>>,
}

// pub type Binding<'gc> = GeneralBinding<'gc, StackValue<'gc>>;
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

impl<P: ?Sized> Clone for GeneralBinding<'_, P> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<P: ?Sized> Copy for GeneralBinding<'_, P> {}

impl<'gc, P: ?Sized> GeneralBinding<'gc, P> {
    pub fn get(&self) -> Gc<'gc, RefLock<P>> {
        self.value
    }

    pub fn read<T>(&self, func: impl FnOnce(Ref<P>) -> T) -> T {
        func(self.value.borrow())
    }

    /// Freeze this binding
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
