//! SRFI implementations

/// SRFI 1
pub mod list {
    use std::{collections::HashSet, sync::Arc};

    use crate::{
        LibraryName, compiler::Module, interpreter::Registerable, library_name, stdlib::magus_impl,
    };

    pub struct Srfi1;

    impl Module for Srfi1 {
        fn all_symbols(
            &self,
            _interner: &mut lasso::Rodeo,
        ) -> std::collections::HashSet<lasso::Spur> {
            HashSet::new()
        }
    }

    const MODULE_SRC: &str = include_str!("srfi_list.scm");

    impl Registerable for Srfi1 {
        fn name(interner: &mut lasso::Rodeo) -> LibraryName {
            LibraryName::from_iter(library_name!(interner => srfi 1))
        }

        fn native(&self) -> Option<Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
            Some(Arc::new(Self))
        }

        fn scheme(&self) -> Option<(&str, &str)> {
            Some(("srfi_list.scm", MODULE_SRC))
        }

        fn scheme_native(
            &self,
            interner: &mut lasso::Rodeo,
        ) -> Vec<(
            LibraryName,
            std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
        )> {
            vec![(
                LibraryName::from_iter(library_name!(interner => magus impl)),
                Arc::new(magus_impl::MagusImpl),
            )]
        }

        fn scheme_dependency(&self, interner: &mut lasso::Rodeo) -> Vec<LibraryName> {
            [
                LibraryName::from_iter(library_name!(interner => scheme base)),
                LibraryName::from_iter(library_name!(interner => scheme cxr)),
            ]
            .into_iter()
            .collect()
        }
    }
}
