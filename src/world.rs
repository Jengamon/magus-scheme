//! A World defines all modules that a R7RS script is allowed to reference, whether it is implemented as Scheme,
//! or natively

use core::fmt;
use std::collections::HashMap;

use crate::general_parser::Module;

// QUESTION should the *entire* world be multi-thread safe?
// It *could* probably be, as there is no state to be shared...
// I think try Arc, and if it becomes a problem, think through it then.
#[derive(Hash, Debug, Clone, PartialEq, Eq)]
struct InternalLibraryName(Box<[lasso::Spur]>);

#[derive(Hash, Debug, Clone)]
pub struct LibraryName(Box<[Box<str>]>);

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
    rodeo: lasso::Rodeo,
    /// scripts that can be requested for execution
    // TODO switch to hashbrown or ahash? might be faster maybe probably?
    // we use Spurs b/c we have the interner
    modules: HashMap<lasso::Spur, Module>,
    /// libraries that can be imported from
    libraries: HashMap<InternalLibraryName, SchemeLibrary>,
}

#[derive(Debug, thiserror::Error)]
pub enum LoadSourceError {
    #[error("invalid general AST")]
    InvalidGAst,
    #[error("library already exists: {0}")]
    LibraryAlreadyExists(LibraryName),
}

impl World {
    fn library_name(
        &self,
        name: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> Option<InternalLibraryName> {
        Some(InternalLibraryName(
            name.into_iter()
                .map(|s| self.rodeo.get(s.as_ref()))
                .collect::<Option<_>>()?,
        ))
    }

    fn library_name_mut(
        &mut self,
        name: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> InternalLibraryName {
        InternalLibraryName(
            name.into_iter()
                .map(|s| self.rodeo.get_or_intern(s.as_ref()))
                .collect(),
        )
    }

    pub fn library(&self, name: impl Into<LibraryName>) -> Option<&SchemeLibrary> {
        self.libraries.get(&self.library_name(name.into().name())?)
    }

    pub fn load_source(
        &mut self,
        filename: impl AsRef<str>,
        module: Module,
    ) -> Result<(), LoadSourceError> {
        // TODO Detect if a given GAst is a library or a program by checking:
        // Libraries are scheme files where the only top-level datum is a s-expr where the first element is "define-library"
        //
        // parser might be a misnomer, and instead should be "evaluator" b/c it is when it comes to evaluation time
        // that understanding what macros are defined in what order is extremely useful.
        //
        // NOTE Nahh looking at the chibi-scheme codebase, it seems that libraries are their own separate files (*.sld)
        // that can (include ...) source files if they so please, so this will be our pattern too (except we'll just have *-lib.scm files)
        //
        // NOTE Actually we will allow multiple libraries within a module, but
        // all libraries *must* precede any part of the program
        todo!()
    }
}

// there is only 1 s-exp in the GAst, and it has "define-library" as it's first element. "(define-library ...)"
// FIXME Make this more of a "parsed" library, with the nodes separated into groups like "body", "imports", "exports" etc.
// and a source location
// (how I wiss traits had narrowing...)
#[derive(Debug)]
pub enum SchemeLibrary {
    Scheme { filename: lasso::Spur, data: () },
    Native(()),
}

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
        let mut world = World::default();

        let ln: InternalLibraryName = world.library_name_mut(["lib"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib apple)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple", "λ"]);
        check!(ln.display(&world.rodeo).to_string() == "(lib apple |λ|)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple", "λ", "pip|e"]);
        check!(ln.display(&world.rodeo).to_string() == r"(lib apple |λ| |pip\|e|)");
    }
}
