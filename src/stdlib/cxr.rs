use crate::{LibraryName, interpreter::Registerable, library_name};

pub const MODULE_SRC: &str = include_str!("scheme_cxr.scm");

pub struct Cxr;
impl Registerable for Cxr {
    fn name(interner: &mut lasso::Rodeo) -> crate::LibraryName {
        LibraryName::from_iter(library_name!(interner => scheme cxr))
    }

    fn native(
        &self,
    ) -> Option<std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
        None
    }

    fn scheme(&self) -> Option<(&str, &str)> {
        Some(("scheme_cxr.scm", MODULE_SRC))
    }

    fn scheme_native(
        &self,
        _interner: &mut lasso::Rodeo,
    ) -> Vec<(
        LibraryName,
        std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
    )> {
        Vec::new()
    }

    fn scheme_dependency(&self, interner: &mut lasso::Rodeo) -> Vec<LibraryName> {
        [library_name!(interner => scheme base)]
            .into_iter()
            .map(LibraryName::from_iter)
            .collect()
    }
}
