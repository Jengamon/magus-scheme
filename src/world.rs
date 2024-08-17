//! A World defines all modules that a R7RS script is allowed to reference, whether it is implemented as Scheme,
//! or natively

// These interner should be multi-thread safe
use core::fmt;
use std::{collections::HashMap, sync::Arc};

use crate::general_parser::GAst;

// QUESTION should the *entire* world be multi-thread safe?
// It *could* probably be, as there is no state to be shared...
// I think try Arc, and if it becomes a problem, think through it then.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
struct InternalLibraryName(Arc<[lasso::Spur]>);

#[derive(Hash, Debug, Clone)]
pub struct LibraryName(Arc<[Box<str>]>);

impl LibraryName {
    pub fn name(&self) -> &[Box<str>] {
        &self.0
    }
}

impl fmt::Display for LibraryName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, name) in self.0.iter().enumerate() {
            // Rough and fast rules for escaping
            // TODO handle peculiar identifiers without piping
            // r"[-+][a-zA-Z!$%&*/:<=>?^_~+\-@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*"
            // r"[-+]\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*"
            // r"\.[a-zA-Z!$%&*/:<=>?^_~+\-.@][0-9a-zA-Z!$%&*/:<=>?^_~+\-.@]*"
            // TODO This should not panic as we only have 1 interner per world, and LibraryName
            // should never leave that world, so all Spur's a particular world references should be valid in that world.
            if name.chars().enumerate().all(|(idx, c)| {
                if idx == 0 {
                    c.is_ascii_alphabetic() || "!$%&*/:<=>?^_~".contains(c)
                } else {
                    c.is_ascii_alphanumeric() || "!$%&*/:<=>?^_~+-.@".contains(c)
                }
            }) {
                write!(f, "{name}")?;
            } else {
                write!(f, "|{}|", name.replace('|', r"\|"))?;
            }
            if i < (self.0.len() - 1) {
                write!(f, " ")?;
            }
        }
        write!(f, ")")
    }
}

impl<T: IntoIterator<Item = impl AsRef<str>>> From<T> for LibraryName {
    fn from(value: T) -> Self {
        Self(value.into_iter().map(|s| Box::from(s.as_ref())).collect())
    }
}

#[derive(Default)]
pub struct World {
    /// interner
    rodeo: lasso::ThreadedRodeo,
    /// scripts that can be requested for execution
    // TODO switch to hashbrown or ahash? might be faster maybe probably?
    modules: HashMap<Box<str>, Module>,
    /// libraries that can be imported from
    libraries: HashMap<InternalLibraryName, SchemeLibrary>,
}

impl World {
    fn library_name(&self, name: impl IntoIterator<Item = impl AsRef<str>>) -> InternalLibraryName {
        InternalLibraryName(
            name.into_iter()
                .map(|s| self.rodeo.get_or_intern(s.as_ref()))
                .collect(),
        )
    }

    pub fn library(&self, name: impl Into<LibraryName>) -> Option<&SchemeLibrary> {
        self.libraries.get(&self.library_name(name.into().name()))
    }
}

// there is only 1 s-exp in the GAst, and it has "define-library" as it's first element. "(define-library ...)"
// FIXME Make this more of a "parsed" library, with the nodes separated into groups like "body", "imports", "exports" etc.
// and a source location
// (how I wiss traits had narrowing...)
#[derive(Debug)]
pub enum SchemeLibrary {
    Scheme(GAst),
    Native(()),
}

/// Modules correspond to a single source file that can be executed
///
/// A module cannot import from another module directly, but that other module must be defined as a library,
/// which will let the world store it in such a way that its definitions can be imported
// Native modules make 0 sense.
#[derive(Debug)]
pub struct Module(GAst);

#[cfg(test)]
mod tests {
    use assert2::check;

    use super::{InternalLibraryName, LibraryName, World};

    impl InternalLibraryName {
        fn display<T: lasso::Reader>(&self, reader: &T) -> LibraryName {
            LibraryName(
                self.0
                    .iter()
                    .map(|s| Box::from(reader.resolve(s)))
                    .collect(),
            )
        }
    }

    #[test]
    fn display_library_names() {
        let world = World::default();

        let ln: InternalLibraryName = world.library_name(["lib"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib)");

        let ln: InternalLibraryName = world.library_name(["lib", "apple"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib apple)");

        let ln: InternalLibraryName = world.library_name(["lib", "apple", "λ"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib apple |λ|)");

        let ln: InternalLibraryName = world.library_name(["lib", "apple", "λ", "pip|e"]);
        check!(ln.display(&world.rodeo).to_string() == r"(lib apple |λ| |pip\|e|)");
    }
}
