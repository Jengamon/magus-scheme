use gc_arena::Collect;

use crate::{
    compiler::{ProgramData, ProgramPtr},
    runtime::lambda::Arity,
};

pub mod base;
pub mod lazy;
pub mod srfi;
pub mod write;
// For now, Scheme-only libraries are just exposed as strings here, but a *biiig* TODO is to find
// a nice way for modules to be registered by users
pub mod cxr {
    pub const MODULE_SRC: &str = include_str!("stdlib/scheme_cxr.scm");
}
// We support (scheme file) but if compiled for the wasm32-unknown-unknown
// - We add it to the features returned by (features) and to the features understood by the compiler
//   (ok, instead we use target-triple to add TARGET as a feature, so you can always see what platform you are compiling
//   for) (and how we suport the os/arch is by using target-lexicon on this triple)
// - TODO (scheme file) is not compiled. This will cause a break in compatibility, as (scheme r5rs) also
//   exports methods from this module. We go for a close compatibility as possible, but here we have to break
//   compatibility (however, we provide an easy way for code to check if this would be a problem)

/// Data struct for formals
#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum Formals {
    Empty,
    Single(lasso::Spur),
    List(Box<[lasso::Spur]>),
    Dotted {
        pre_dot: Box<[lasso::Spur]>,
        dot: lasso::Spur,
    },
}

impl Formals {
    // Arity calcuation (when used for `lambda`)
    pub fn arity(&self) -> Arity {
        match self {
            Self::Empty => Arity::Exact(0),
            Self::Single(_) => Arity::AtLeast(0),
            Self::List(l) => Arity::Exact(l.len()),
            Self::Dotted { pre_dot, .. } => Arity::AtLeast(pre_dot.len()),
        }
    }

    pub fn non_rest_params(&self) -> impl IntoIterator<Item = lasso::Spur> {
        match self {
            Self::Empty | Self::Single(_) => vec![],
            Self::List(l) => l.to_vec(),
            Self::Dotted { pre_dot, .. } => pre_dot.to_vec(),
        }
    }

    pub fn rest_param(&self) -> Option<lasso::Spur> {
        match self {
            Self::Single(s) => Some(*s),
            Self::Dotted { dot, .. } => Some(*dot),
            _ => None,
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("not a formals list")]
pub struct NotFormals;

impl Formals {
    pub fn convert(ptr: ProgramPtr<'_>, interner: &mut lasso::Rodeo) -> Result<Self, NotFormals> {
        match &ptr.data {
            ProgramData::EmptyList => Ok(Self::Empty),
            ProgramData::Symbol(s) => Ok(Self::Single(*s)),
            ProgramData::List { head, body } => {
                // convert head to a symbol, and list away!
                let mut list = vec![head.into_symbol(interner).ok_or(NotFormals)?];
                for p in body {
                    match &p.data {
                        ProgramData::Symbol(s) => {
                            list.push(*s);
                        }
                        _ => return Err(NotFormals),
                    }
                }
                Ok(Self::List(list.into()))
            }
            ProgramData::DottedList { pre_dot, dot } => {
                let pre_dot = pre_dot
                    .iter()
                    .map(|p| match &p.data {
                        ProgramData::Symbol(s) => Ok(*s),
                        _ => Err(NotFormals),
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let dot = match &dot.data {
                    ProgramData::Symbol(s) => Ok(*s),
                    _ => Err(NotFormals),
                }?;
                Ok(Self::Dotted {
                    pre_dot: pre_dot.into(),
                    dot,
                })
            }
            _ => Err(NotFormals),
        }
    }
}
