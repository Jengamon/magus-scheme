//! A World defines all modules that a R7RS script is allowed to reference, whether it is implemented as Scheme,
//! or natively

use core::fmt;
use std::collections::HashMap;

use gc_arena::{Arena, Collect, Gc, Mutation, RefLock, Rootable, Static};
use value::{ConsCell, Value, ValuePtr, ValueVisitor, Vector};

use crate::{
    general_parse,
    general_parser::{gast, GeneralParserError},
    runtime::interpreter::Interpreter,
    ContainsDatum, Datum, GAstNode as _, Module,
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

// the runtime representation of library definitions.
// all exports, imports should be defined,
// and whether the body is Scheme source to be executed or a native definition
// of runtime objects
#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct SchemeLibrary<'gc> {
    items: HashMap<Box<str>, ValuePtr<'gc>>,
}

impl<'gc> SchemeLibrary<'gc> {
    pub fn exports(&self) -> &HashMap<Box<str>, ValuePtr<'gc>> {
        &self.items
    }
}

// A GAst extension used for declaring libraries
pub struct SchemeLibraryDeclaration {
    filename: Box<str>,
    case_insensitive: bool,
    data: gast::List,
}

#[derive(Debug, thiserror::Error)]
pub enum LibraryDeclError {}

impl SchemeLibraryDeclaration {
    pub fn new(
        filename_spur: lasso::Spur,
        case_insensitive: bool,
        maybe_decl: gast::List,
    ) -> Option<Result<Self, LibraryDeclError>> {
        // A library declaration *never* has a dot.
        if maybe_decl.has_dot() {
            return None;
        }

        todo!()
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct WorldArena<'gc> {
    pub(crate) interpreters: HashMap<RuntimeKey, Gc<'gc, RefLock<Interpreter<'gc>>>>,

    // value that is (eq? '())
    pub(crate) null_val: ValuePtr<'gc>,

    /// libraries that can be imported from
    libraries: HashMap<Static<InternalLibraryName>, SchemeLibrary<'gc>>,
}
pub(crate) type WorldRoot = Arena<Rootable![WorldArena<'_>]>;

impl<'gc> WorldArena<'gc> {
    pub fn null(&self) -> ValuePtr<'gc> {
        self.null_val
    }

    pub fn from_iter<
        T: IntoIterator<
            Item = ValuePtr<'gc>,
            IntoIter = impl DoubleEndedIterator<Item = ValuePtr<'gc>>,
        >,
    >(
        &self,
        mc: &Mutation<'gc>,
        iter: T,
    ) -> ValuePtr<'gc> {
        match ConsCell::from_iter(mc, iter) {
            ConsCell {
                car: None,
                cdr: None,
            } => self.null_val,
            cons => {
                let ptr = Gc::new(mc, RefLock::new(Value::Cons(cons)));
                Self::ensure_null(self, mc, ptr);
                ptr
            }
        }
    }

    // Convert ConsCell [ None None ] to root null_val
    pub fn ensure_null(&self, mc: &Mutation<'gc>, value_ptr: ValuePtr<'gc>) {
        let mut ensure_null = EnsureNullVisitor {
            mutation: mc,
            null: &self.null_val.borrow(),
        };
        ensure_null.visit_value(value_ptr);
    }
}

struct EnsureNullVisitor<'a, 'gc> {
    mutation: &'a Mutation<'gc>,
    null: &'a Value<'gc>,
}

impl<'a, 'gc> ValueVisitor<'gc> for EnsureNullVisitor<'a, 'gc> {
    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: ValuePtr<'gc>) {
        if cons.car.is_none() && cons.cdr.is_none() {
            *value.unlock(self.mutation).borrow_mut() = *self.null;
            return;
        }

        if let Some(car) = cons.car {
            self.visit_value(car);
        }

        if let Some(cdr) = cons.cdr {
            self.visit_value(cdr);
        }
    }

    fn visit_vector(&mut self, vec: Vector<'gc>, _value: ValuePtr<'gc>) {
        for elem in vec.vec.borrow().iter() {
            self.visit_value(*elem)
        }
    }
}

pub struct World {
    // TODO Make World also have the gc-arenas for values and rc-refcell (hashmap?) for runtimes
    // so that runtimes can be interacted with stashed.
    // a world is the technical definition of our entire Scheme environment, so this
    // should be ok!
    // FIXME look at how piccolo does stashing
    // INFO actually maybe not? let's see??
    root: WorldRoot,
    pub(crate) rodeo: lasso::Rodeo,
    /// interner
    /// scripts that can be "included" in execution
    // TODO switch to hashbrown or ahash? might be faster maybe probably?
    // we use Spurs b/c we have the interner
    pub(crate) modules: HashMap<lasso::Spur, InternalModule>,

    /// sources for libraries that are to-be-compiled from Scheme
    scheme_libraries: Vec<SchemeLibraryDeclaration>,
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

    /// Returns duplicate source bundles as an error, as well
    /// as any errors when reading a library declarations
    pub fn load_sources(
        &mut self,
        sources: impl IntoIterator<Item = SourceBundle>,
    ) -> Result<(), (Vec<SourceBundle>, Vec<LibraryDeclError>)> {
        // NOTE Actually we will allow multiple libraries within a module, but
        // all libraries *must* precede any part of the program

        let mut duplicates = vec![];
        let mut library_errors = vec![];
        for source in sources {
            let filename_spur = self.rodeo.get_or_intern(&source.filename);
            let mut datum_iter = source.module.datum().peekable();
            while let Some(maybe_lib_decl) = datum_iter
                .peek()
                .and_then(|data| data.as_list())
                .and_then(|lst| {
                    SchemeLibraryDeclaration::new(filename_spur, source.case_insensitive, lst)
                })
            {
                match maybe_lib_decl {
                    Ok(lib_decl) => {
                        self.scheme_libraries.push(lib_decl);
                    }
                    Err(e) => {
                        library_errors.push(e);
                    }
                }
                _ = datum_iter.next();
            }

            // wrap the rest as a module declaration
            #[allow(clippy::map_entry)]
            if !self.modules.contains_key(&filename_spur) {
                self.modules.insert(
                    filename_spur,
                    InternalModule {
                        datum: datum_iter.collect(),
                        case_insensitive: source.case_insensitive,
                        filename: filename_spur,
                    },
                );
            } else {
                // so that we can just *move* source
                drop(datum_iter);
                duplicates.push(source);
            }
        }

        if duplicates.is_empty() && library_errors.is_empty() {
            Ok(())
        } else {
            Err((duplicates, library_errors))
        }
    }
}

impl Default for World {
    fn default() -> Self {
        Self {
            root: WorldRoot::new(|mc| WorldArena {
                interpreters: HashMap::new(),
                // this value
                null_val: ValuePtr::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
                libraries: HashMap::default(),
            }),
            scheme_libraries: Vec::new(),
            modules: HashMap::default(),
            rodeo: lasso::Rodeo::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceBundle {
    pub filename: Box<str>,
    pub case_insensitive: bool,
    pub module: Module,
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

pub(crate) struct InternalModule {
    datum: Vec<Datum>,
    case_insensitive: bool,
    filename: lasso::Spur,
}

impl InternalModule {
    pub fn filename_spur(&self) -> lasso::Spur {
        self.filename
    }
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
