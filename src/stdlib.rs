use gc_arena::Collect;

use crate::compiler::ProgramPtr;

pub mod base;
pub mod srfi;

/// Data struct for formals
#[derive(Debug, Collect)]
#[collect(require_static)]
enum Formals {
    Empty,
    Single(lasso::Spur),
    List(Box<[lasso::Spur]>),
    Dotted {
        pre_dot: Box<[lasso::Spur]>,
        dot: lasso::Spur,
    },
}

#[derive(thiserror::Error, Debug)]
enum FormalsError {}

impl Formals {
    pub fn convert(ptr: ProgramPtr<'_>, interner: &mut lasso::Rodeo) -> Result<Self, FormalsError> {
        todo!()
    }
}
