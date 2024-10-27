//! Base objects for Scheme
//!
//! Some of these require closer access to the actual executor context.
//! (looking at you, call/cc), so are implemented here to keep those private.

// when macros are more dynamic, the code to implement them would be stored here

// we copy the module structure of the standard R7RS library

pub mod base;

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

#[macro_export(local_inner_macros)]
macro_rules! declare_lambdas {
    ($lib_name:ident => { $($lam_ident:ident as $lazy_access:ident => $lam_value:expr),* }) => {
        use gc_arena::{Collect, Gc, RefLock, Mutation};
        use $crate::runtime::lambda::LambdaPtr;

        #[derive(Collect, Clone, Copy)]
        #[collect(no_drop)]
        pub struct $lib_name<'gc> {
            $(
                pub(crate) $lam_ident: Gc<'gc, RefLock<Option<LambdaPtr<'gc>>>>
            ),*
        }

        impl<'gc> $lib_name<'gc> {
            pub fn new(mc: &Mutation<'gc>) -> Self {
                Self {
                    $(
                        $lam_ident: Gc::new(mc, RefLock::new(None))
                    ),*
                }
            }

            $(
                pub fn $lazy_access(&self, mc: &Mutation<'gc>) -> LambdaPtr<'gc> {
                    let mut lam = self.$lam_ident.borrow_mut(mc);
                    if let Some(lam) = *lam {
                        lam
                    } else {
                        let new_lam = Gc::new(
                            mc,
                            RefLock::new($lam_value(mc)),
                        );
                        *lam = Some(new_lam);
                        new_lam
                    }
                }
            )*
        }
    };
}
