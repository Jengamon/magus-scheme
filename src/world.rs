//! A World defines all modules that a R7RS script is allowed to reference, whether it is implemented as Scheme,
//! or natively
use crate::general_parser::Gast;

pub struct World {}

/// Modules correspond to a single source file
#[derive(Debug)]
pub enum Module {
    Scheme(Gast),
}
