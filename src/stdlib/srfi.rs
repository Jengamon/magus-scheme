//! SRFI implementations

/// SRFI 1
pub mod list {
    use std::collections::HashSet;

    use crate::compiler::Module;

    pub struct Srfi1;

    impl Module for Srfi1 {
        fn all_symbols(
            &self,
            _interner: &mut lasso::Rodeo,
        ) -> std::collections::HashSet<lasso::Spur> {
            HashSet::new()
        }
    }

    const SRFI_LIST: &str = include_str!("srfi_list.scm");
    pub fn register_module() {}
}
