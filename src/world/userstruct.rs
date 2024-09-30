// We lift piccolo's UserData, and strip metatable stuff to
// produce our version of value userdata storage

use std::hash::{Hash, Hasher};

use gc_arena::{arena::Root, barrier, Collect, Mutation, Rootable, Static};

use super::any::Any;

#[derive(Debug, Clone, Copy, thiserror::Error)]
#[error("UserStruct type mismatch")]
pub struct BadUserStructType;

/// A garbage-collected pointer to a user-defined type.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
// Do we need associated data with this type?
// ASK Do we need this type?
pub struct UserStruct<'gc>(Any<'gc, ()>);

impl<'gc> PartialEq for UserStruct<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<'gc> Eq for UserStruct<'gc> {}

impl<'gc> Hash for UserStruct<'gc> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<'gc> UserStruct<'gc> {
    /// Create a new `UserData` from any GC value.
    ///
    /// In order to provide safe downcasting, an `R` type must be provided that implements
    /// [`trait@Rootable`]. Usually this type is constructed with the [`macro@Rootable`] macro.
    ///
    /// Downcasting GC types requires that you provide the **same** `R` type to [`UserData::is`] or
    /// [`UserData::downcast`], as the type is identified not by the `TypeId` of the value itself,
    /// but the `TypeId` of the `Rootable` impl (and it must be this way, for soundness). If you
    /// always use the [`macro@Rootable`] macro rather than a custom `Rootable` impl as the `R`
    /// type, then this happens automatically.
    pub fn new<R>(mc: &Mutation<'gc>, val: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a> + 'static,
        Root<'gc, R>: Sized + Collect,
    {
        UserStruct(Any::new::<R>(mc, val))
    }

    /// Create a new `UserData` type from a non-GC value.
    ///
    /// This equivalent to calling [`UserData::new`] with the given value wrapped in the [`Static`]
    /// wrapper provided by `gc-arena` and the `R` type set to `Static<T>`.
    ///
    /// This is provided as a convenience as an easier API than dealing with the [`trait@Rootable`]
    /// trait. In order to downcast values created with this method, you can use the *static*
    /// variants of `UserData` methods as a further convenience, like [`UserData::downcast_static`].
    pub fn new_static<T: 'static>(mc: &Mutation<'gc>, val: T) -> Self {
        Self::new::<Static<T>>(mc, Static(val))
    }
    /*
        pub fn from_inner(inner: Gc<'gc, UserDataInner<'gc>>) -> Self {
            Self(Any::from_inner(inner))
        }

        pub fn into_inner(self) -> Gc<'gc, UserDataInner<'gc>> {
            self.0.into_inner()
        }
    */
    /// Check if a `UserData` was created with the type `R` passed to [`UserData::new`].
    ///
    /// `UserData` is identified by the `TypeId` of the [`trait@Rootable`] impl, NOT the type
    /// itself. We must do things this way, because GC types are non-'static (so you cannot obtain
    /// their `TypeId` in the first place).
    pub fn is<R>(self) -> bool
    where
        R: for<'a> Rootable<'a> + 'static,
    {
        self.0.is::<R>()
    }

    /// Check if a `UserData` is of type `T` created with [`UserData::new_static`].
    ///
    /// This is equivalent to calling `this.is::<Static<T>>()`.
    pub fn is_static<T: 'static>(self) -> bool {
        self.is::<Static<T>>()
    }

    /// Downcast a GC `UserData` and get a reference to it.
    ///
    /// If [`UserData::is`] returns true for the provided type `R`, then this will return a
    /// reference to the held type, otherwise it will return `Err(BadUserStructType)`.
    pub fn downcast<R>(self) -> Result<&'gc Root<'gc, R>, BadUserStructType>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast::<R>().ok_or(BadUserStructType)
    }

    /// Downcast the `UserData` and get a reference to it wrapped in [`barrier::Write`].
    ///
    /// If the type matches, this also triggers a write barrier on the held `Gc` pointer, allowing
    /// you to safely mutate the held GC value through mechanisms provided by `gc_arena`.
    ///
    /// This is ONLY ever useful if the held type is a non-'static GC type, and usually only useful
    /// when the type is something like [`gc_arena::lock::RefLock`], where this would allow you to
    /// call [`gc_arena::barrier::Write::unlock`] on it to get safe mutability.
    pub fn downcast_write<R>(
        self,
        mc: &Mutation<'gc>,
    ) -> Result<&'gc barrier::Write<Root<'gc, R>>, BadUserStructType>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast_write::<R>(mc).ok_or(BadUserStructType)
    }

    /// Downcast a `'static` `UserData` and get a reference to it.
    ///
    /// If [`UserData::is_static`] returns true for the provided type `T`, then this will return a
    /// reference to the held type, otherwise it will return `Err(BadUserStructType)`.
    ///
    /// This is equivalent to calling `this.downcast::<Static<T>>()` (except this returns a
    /// reference to the inner [`Static::0`] field instead).
    pub fn downcast_static<T: 'static>(self) -> Result<&'gc T, BadUserStructType> {
        self.downcast::<Static<T>>().map(|r| &r.0)
    }
}
