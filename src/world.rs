//! A World defines all modules that a R7RS script is allowed to reference, whether it is implemented as Scheme,
//! or natively

use core::fmt;
use std::collections::HashMap;

use gc_arena::{Arena, Collect, Gc, RefLock, Rootable};

use crate::{
    general_parse, general_parser::GeneralParserError, runtime::interpreter::Interpreter,
    GAstNode as _, Module,
};

/*
macros and special forms are defined here:
special forms are macros that have access to the source of their expansion
and are given in their own unique environment with access to their parent environment

macros resemble piccolo::Sequences in that they must be resumable, but are
simpler in that they only have 3 returns:
Ok(Evaluating) - macro ran out of fuel for expansion, is interacting with something, etc.
Ok(List) - the list this macro should expand into, to be
Err(MacroError) - this macro failed evaluation for some reason

so yeah it's basically a future.
TODO Rip off piccolo::UserData (but w/o metatable stuff)
for UserStruct, then store macros and special forms in the environment
using that!
*/

pub mod any;
pub mod fuel;
pub mod userstruct;
pub mod value;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Collect)]
#[collect(require_static)]
pub struct RuntimeKey(usize);

#[derive(Collect)]
#[collect(no_drop)]
pub(crate) struct WorldArena<'gc> {
    pub interpreters: HashMap<RuntimeKey, Gc<'gc, RefLock<Interpreter<'gc>>>>,
    // TODO Stash for stuff
    // we don't actually have to stash runtimes, b/c they are just in the root
}
pub(crate) type WorldRoot = Arena<Rootable![WorldArena<'_>]>;

pub struct World {
    // TODO Make World also have the gc-arenas for values and rc-refcell (hashmap?) for runtimes
    // so that runtimes can be interacted with stashed.
    // a world is the technical definition of our entire Scheme environment, so this
    // should be ok!
    // FIXME look at how piccolo does stashing
    // INFO actually maybe not? let's see??
    root: WorldRoot,
    /// interner
    /// scripts that can be "included" in execution
    // TODO switch to hashbrown or ahash? might be faster maybe probably?
    // we use Spurs b/c we have the interner
    pub(crate) modules: HashMap<lasso::Spur, InternalModule>,
    pub(crate) access: WorldAccess,
}

#[derive(Default)]
pub struct WorldAccess {
    // TODO FIX separate rodeo and libraries into auxillary struct so that
    // they can be borrowed by Callback w/o borrowing the entire world
    pub(crate) rodeo: lasso::Rodeo,
    /// libraries that can be imported from
    libraries: HashMap<InternalLibraryName, SchemeLibrary>,
}

impl Default for World {
    fn default() -> Self {
        Self {
            root: WorldRoot::new(|_mc| WorldArena {
                interpreters: HashMap::new(),
            }),
            modules: HashMap::default(),
            access: WorldAccess::default(),
        }
    }
}

#[derive(Debug)]
pub struct SourceBundle {
    pub filename: Box<str>,
    pub case_insensitive: bool,
    pub module: Module,
}

pub(crate) struct InternalModule {
    module: Module,
    filename: lasso::Spur,
}

impl InternalModule {
    pub fn filename_spur(&self) -> lasso::Spur {
        self.filename
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{} errors in {filename}", errors.len())]
pub struct SourceBundleError {
    pub filename: Box<str>,
    pub code: Box<str>,
    pub errors: Vec<GeneralParserError>,
}

impl SourceBundle {
    pub fn new(
        filename: impl AsRef<str>,
        source: impl AsRef<str>,
        case_insensitive: bool,
    ) -> Result<Self, SourceBundleError> {
        let filename = Box::from(filename.as_ref());
        let source = source.as_ref();
        let gparse = general_parse(source);
        if !gparse.errors().is_empty() {
            let errors = gparse.into_errors();
            let code = Box::from(source);
            Err(SourceBundleError {
                code,
                errors,
                filename,
            })
        } else {
            // if no errors, (well, even if errors)
            // casting the root syntax node (the one returned by GAst::syntax)
            // and unwrapping it is always safe
            Ok(Self {
                filename,
                case_insensitive,
                module: Module::cast(gparse.syntax()).unwrap(),
            })
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum LoadSourceError {
    #[error("library already exists: {0}")]
    LibraryAlreadyExists(LibraryName),
}

#[derive(Debug, thiserror::Error)]
pub enum IncludeSourceError {
    #[error("attempted to define library: {0}")]
    DefineLibrary(LibraryName),
}

impl World {
    fn library_name(
        &self,
        name: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> Option<InternalLibraryName> {
        Some(InternalLibraryName(
            name.into_iter()
                .map(|s| self.access.rodeo.get(s.as_ref()))
                .collect::<Option<_>>()?,
        ))
    }

    fn library_name_mut(
        &mut self,
        name: impl IntoIterator<Item = impl AsRef<str>>,
    ) -> InternalLibraryName {
        InternalLibraryName(
            name.into_iter()
                .map(|s| self.access.rodeo.get_or_intern(s.as_ref()))
                .collect(),
        )
    }

    pub fn library(&self, name: impl Into<LibraryName>) -> Option<&SchemeLibrary> {
        self.access
            .libraries
            .get(&self.library_name(name.into().name())?)
    }

    /// include source as something that can be included using `include` or `include-ci`
    ///
    /// these are *not* allowed to conflict with loaded source filenames
    pub fn include_sources(
        &mut self,
        includes: impl IntoIterator<Item = SourceBundle>,
    ) -> Result<(), IncludeSourceError> {
        // we don't define any libraries from these sources, so they *aren't*
        // allowed to define any libraries (using define-library at the top level
        // before any other statement (where they would be fine in a load_sources)
        // is an *error* here)
        //
        // (well, we *do* allow lists in the shape of (define-library ...), but
        // only if they can never be confused with a load_source's library definition)
        todo!()
    }

    pub fn load_sources(
        &mut self,
        source: impl IntoIterator<Item = SourceBundle>,
    ) -> Result<Vec<LibraryName>, LoadSourceError> {
        // NOTE Actually we will allow multiple libraries within a module, but
        // all libraries *must* precede any part of the program
        // let filename = self.rodeo.get_or_intern(filename.as_ref());
        // let mut module_datum = module.datum().peekable();
        // store the parsed library data (the decls, as well as the vec of Datum within Begin forms)
        let mut library_data = Vec::<()>::new();
        // we load a bunch of sources together
        // strip out and separate the defined libraries, then
        // for include, include-ci, and include-library-definitions (which is *not* a special form)
        // all reference files can only be referenced in the same iterator or a previous
        // load_sources call
        // collect the remaining datum into a program
        // return the library names registered (if any)
        todo!()
    }
}

// the runtime representation of library definitions.
// all exports, imports should be defined,
// and whether the body is Scheme source to be executed or a native definition
// of runtime objects
#[derive(Debug)]
pub struct SchemeLibrary {}

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
        check!(ln.display(&world.access.rodeo).to_string() == "(lib)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple"]);
        check!(ln.display(&world.access.rodeo).to_string() == "(lib apple)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple", "λ"]);
        check!(ln.display(&world.access.rodeo).to_string() == "(lib apple |λ|)");

        let ln: InternalLibraryName = world.library_name_mut(["lib", "apple", "λ", "pip|e"]);
        check!(ln.display(&world.access.rodeo).to_string() == r"(lib apple |λ| |pip\|e|)");
    }
}
