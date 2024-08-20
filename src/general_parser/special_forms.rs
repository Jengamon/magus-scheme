use crate::ContainsDatum as _;

use super::gast::List;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecialForm {
    // quoting
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,

    // source inclusion
    Include,
    IncludeCi,

    // minimal structures
    Define,
    SetBang,
    If,
    Lambda,

    // library
    DefineLibrary,
    // TODO all auxilaries for define-library
    Import,

    // macro
    DefineSyntax,
    SyntaxRules,

    // record types
    DefineRecordType,
}

macro_rules! declare_check {
    ($ty:ty => $check:expr) => {
        impl $ty {
            pub(super) fn check(list: &List, case_insensitive: bool) -> bool {
                $check(list, case_insensitive)
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct DefineSyntax {}

declare_check!(DefineSyntax => |list: &List, ci: bool| {
    let datum = list.datum().skip(1).collect::<Vec<_>>();
    if datum.len() != 2 {
        return false
    }
    todo!()
});

/// Describes a library
// we only extract information *about* a library, like it's name, import and exports,
// we do not handle collecting together the elements of its body so that it can be left
// to World to handle using Scheme code, or a Rust interface.
#[derive(Debug, Clone)]
pub struct LibraryDefinition {}

declare_check!(LibraryDefinition => |list, ci| {
    todo!("check define-library")
});

/// Describes an import declaration
#[derive(Debug, Clone)]
pub struct ImportDeclaration {}

declare_check!(ImportDeclaration => |list, ci| {
   todo!("check import")
});
