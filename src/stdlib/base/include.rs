use fxhash::FxHashSet;

use crate::{
    LibraryName, Syntax, SyntaxContext, SyntaxReturn,
    compiler::{
        CompileError, ImportSet, LibraryDeclaration, ListHead, ParseProgram, ProgramData,
        ProgramPtr,
    },
};

fn include_files<'gc>(
    case_insensitive: bool,
    args: &[ProgramPtr<'gc>],
    ctx: &mut SyntaxContext<'_, '_, 'gc>,
    compiler: &mut crate::compiler::Compiler<'gc>,
) -> anyhow::Result<SyntaxReturn<'gc>> {
    if !args
        .iter()
        .all(|arg| matches!(arg.data, ProgramData::String(_)))
    {
        anyhow::bail!("include can only include filenames")
    }

    let mut included_source = vec![];

    for filename in args
        .iter()
        .copied()
        .filter_map(|p| {
            if let ProgramData::String(s) = p.data {
                Some(ctx.ecc.interner.resolve(&s))
            } else {
                None
            }
        })
        .map(Box::from)
        .collect::<Vec<_>>()
    {
        let source = ctx.ecc.includer.include(&filename)?;
        let mut programs = (filename.clone(), source)
            .parse_program(ctx.mc, ctx.ecc.interner, case_insensitive)?
            .into_iter()
            .peekable();
        // "re-implement" compile here...
        let mut imported = FxHashSet::default();
        loop {
            match programs.peek() {
                Some(p)
                    if matches!(
                        p.data,
                        ProgramData::List {
                            head: ListHead::Import,
                            ..
                        }
                    ) =>
                {
                    // import code, so read carefully
                    let program = programs.next().unwrap();
                    let ProgramData::List { body, .. } = &program.data else {
                        unreachable!();
                    };

                    let sets: Result<Vec<_>, _> = body
                        .iter()
                        .map(|p| ImportSet::convert(*p, ctx.ecc.interner))
                        .collect();

                    for set in sets? {
                        let imports =
                            compiler.import(ctx.mc, ctx.ecc.interner, ctx.ecc.world, &set, true)?;
                        if !imported.is_disjoint(&imports) {
                            Err(CompileError::DoubleImport(Box::from_iter(
                                imported.intersection(&imports).copied(),
                            )))?;
                        }
                        imported.extend(imports);
                    }
                }
                Some(p)
                    if matches!(
                        p.data,
                        ProgramData::List {
                            head: ListHead::DefineLibrary,
                            ..
                        }
                    ) =>
                {
                    // define-library code, so read carefully
                    let program = programs.next().unwrap();
                    let ProgramData::List { body, .. } = &program.data else {
                        unreachable!();
                    };

                    let name = body
                        .iter()
                        .next()
                        .and_then(|n| LibraryName::convert(*n, ctx.ecc.interner))
                        .ok_or(CompileError::NoLibraryName(program.source))?;

                    let library_decls = body
                        .iter()
                        .skip(1)
                        .map(|p| LibraryDeclaration::convert(*p, ctx.mc, ctx.ecc.interner))
                        .collect::<Result<Vec<_>, _>>()?;

                    // manually match and ignore OoF errors
                    match compiler.define_library(
                        ctx.mc,
                        &name,
                        ctx.ecc,
                        true,
                        ctx.library_def,
                        library_decls,
                    ) {
                        Ok(_) => {}
                        Err(e) => return Err(e.into()),
                    }
                }
                _ => break,
            }
        }

        for program in programs {
            included_source.extend(compiler.compile_code(ctx, program)?.into_bytecode())
        }
    }

    Ok(SyntaxReturn::Code(included_source.into_boxed_slice()))
}

#[derive(Debug)]
pub struct Include;

impl Syntax for Include {
    fn evaluate<'gc>(
        &self,
        ctx: &mut crate::SyntaxContext<'_, '_, 'gc>,
        compiler: &mut crate::compiler::Compiler<'gc>,
        _import_env: crate::environment::StackEnvironmentPtr<'gc>,
        args: &[crate::compiler::ProgramPtr<'gc>],
    ) -> anyhow::Result<crate::SyntaxReturn<'gc>> {
        include_files(false, args, ctx, compiler)
    }

    fn is_definition<'gc>(
        &self,
        _ptr: crate::compiler::ProgramPtr<'gc>,
        _compiler: &crate::compiler::Compiler<'gc>,
    ) -> bool {
        // include is considered a definition, so that a lambda cannot consist solely of an include
        // well, maybe not, a lambda *can* just be an include...
        false
    }
}

#[derive(Debug)]
pub struct IncludeCi;

impl Syntax for IncludeCi {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut crate::compiler::Compiler<'gc>,
        _import_env: crate::environment::StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        include_files(true, args, ctx, compiler)
    }
}
