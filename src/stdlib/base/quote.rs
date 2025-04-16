use std::{collections::HashSet, sync::Arc};

use crate::{
    bytecode::{Bytecode, Constant},
    compiler::{Compiler, ListHead, ProgramData, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
};

// TODO Introduce a separate label set for "requested labels"
// and the label check becomes: requested - labeled (difference)
fn quote_program<'gc>(
    ptr: ProgramPtr<'gc>,
    compiler: &mut Compiler<'gc>,
    ctx: &mut SyntaxContext<'_, 'gc>,
    labels: &mut HashSet<usize>,
    requested_labels: &mut HashSet<usize>,
) -> anyhow::Result<Vec<Bytecode>> {
    macro_rules! constant_eval {
        ($e:expr => $f:ident) => {
            vec![Bytecode::PushConst {
                index: ctx.add_constant(Constant::$f($e)),
            }]
        };
    }

    Ok(match &ptr.data {
        ProgramData::Integer(i) => constant_eval!(*i => Number),
        ProgramData::Rational(sign, numer, denom) if *denom != 0 => {
            let index = ctx.add_constant(Constant::Rational(*sign, *numer, *denom));
            vec![Bytecode::PushConst { index }]
        }
        ProgramData::Rational(_, _, _) => anyhow::bail!("ratio over 0 in source"),
        ProgramData::Inexact(f) => constant_eval!(*f => Inexact),
        ProgramData::Bytevector(bv) => constant_eval!(Arc::from(bv.as_ref()) => Bytevector),
        ProgramData::String(s) => {
            constant_eval!(Arc::from(ctx.interner.resolve(s)) => String)
        }
        ProgramData::Symbol(s) => {
            constant_eval!(*s => Symbol)
        }
        ProgramData::Bool(b) => {
            vec![Bytecode::PushBool { bool: *b }]
        }
        ProgramData::Char(c) => {
            constant_eval!(*c => Char)
        }
        ProgramData::Labeled { label, item } => {
            labels.insert(*label);
            let mut code = quote_program(*item, compiler, ctx, labels, requested_labels)?;

            code.push(Bytecode::FillHole { id: *label });
            code.push(Bytecode::MakeHole { id: *label });
            code
        }
        ProgramData::LabelRef(label) => {
            requested_labels.insert(*label);
            if let Some(val) = compiler.label_value(*label) {
                quote_program(val, compiler, ctx, labels, requested_labels)?
            } else {
                // This code will fail anyways with a "undefined label" failure
                vec![]
            }
        }
        ProgramData::Vector(v) => {
            let mut data = vec![];
            let length = v.len();
            for it in v.iter() {
                data.extend(quote_program(*it, compiler, ctx, labels, requested_labels)?);
            }
            data.push(Bytecode::MakeVector { length });
            data
        }
        ProgramData::EmptyList => vec![Bytecode::PushNull],
        ProgramData::List { head, body } => {
            let mut data = vec![Bytecode::PushNull];
            let body_chunks = body
                .iter()
                .map(|it| quote_program(*it, compiler, ctx, labels, requested_labels))
                .collect::<Vec<_>>();
            for it in body_chunks.into_iter().rev() {
                data.extend(it?);
                data.push(Bytecode::MakePair);
            }
            match head {
                ListHead::Program(p) => {
                    data.extend(quote_program(*p, compiler, ctx, labels, requested_labels)?);
                }
                ListHead::Import => {
                    let import = ctx.interner.get_or_intern_static("import");
                    data.push(Bytecode::PushConst {
                        index: ctx.add_constant(Constant::Symbol(import)),
                    });
                }
                ListHead::DefineLibrary => {
                    let define_library = ctx.interner.get_or_intern_static("define-library");
                    data.push(Bytecode::PushConst {
                        index: ctx.add_constant(Constant::Symbol(define_library)),
                    });
                }
            };
            data.push(Bytecode::MakePair);
            data
        }
        ProgramData::DottedList { pre_dot, dot } => {
            debug_assert!(!pre_dot.is_empty());
            let body_chunks = pre_dot
                .iter()
                .map(|it| quote_program(*it, compiler, ctx, labels, requested_labels))
                .collect::<Vec<_>>();
            let mut data = quote_program(*dot, compiler, ctx, labels, requested_labels)?;
            for it in body_chunks.into_iter().rev() {
                data.extend(it?);
                data.push(Bytecode::MakePair);
            }
            data
        }
    })
}

#[derive(Debug)]
pub struct Quote;

impl Syntax for Quote {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("quote expects exactly 1 argument"));
        }

        // Used for evaluating labeled datum
        let (labeled, requested) = compiler.label_data();
        let mut labels = HashSet::from_iter(labeled.clone());
        let mut requested_labels = HashSet::from_iter(requested.clone());
        let code = quote_program(args[0], compiler, ctx, &mut labels, &mut requested_labels)?;
        // Error if there are any undefined labels
        let undefined_labels = requested_labels.difference(&labels).collect::<HashSet<_>>();
        if !undefined_labels.is_empty() {
            anyhow::bail!("undefined labels: {undefined_labels:?}")
        }
        compiler.add_labeled(labels);
        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }
}

#[derive(Debug)]
pub struct Quasiquote;

fn quasiquote_program<'gc>(
    ptr: ProgramPtr<'gc>,
    compiler: &mut Compiler<'gc>,
    ctx: &mut SyntaxContext<'_, 'gc>,
    labels: &mut HashSet<usize>,
    requested_labels: &mut HashSet<usize>,
    level: &mut usize,
) -> anyhow::Result<Vec<Bytecode>> {
    macro_rules! evaluate {
        () => {
            compiler
                .compile_code(ctx, ptr)?
                .into_bytecode()
                .into_iter()
                .collect()
        };
    }

    let quasiquote = ctx.interner.get_or_intern_static("quasiquote");
    let unquote = ctx.interner.get_or_intern_static("unquote");
    let unquote_splicing = ctx.interner.get_or_intern_static("unquote-splicing");

    Ok(match &ptr.data {
        // constant data is always "evaluated"/quoted no matter the level
        // identifiers and lists have special reactions tho, where lists with heads `quasiquote`, `unquote`, `unquote-splicing` are
        // treated specially, and both react to the level (and thus vectors, and labelled datum *also* react to level b/c it might contain a list)

        // evaulate the symbol
        ProgramData::Symbol(_) if *level == 0 => evaluate!(),
        // Handle quasiquote changing lists
        ProgramData::List { head, body }
            if *level > 0
                && matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(s) if s == quasiquote))
                && body.len() == 1 =>
        {
            *level += 1;
            let res = if *level > 0 {
                let res =
                    quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: ctx.add_constant(Constant::Symbol(quasiquote)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?
            };
            *level -= 1;
            res
        }
        ProgramData::List { head, body }
            if *level > 0
                && matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(s) if s == unquote))
                && body.len() == 1 =>
        {
            *level -= 1;
            let res = if *level > 0 {
                let res =
                    quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: ctx.add_constant(Constant::Symbol(unquote)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?
            };
            *level += 1;
            res
        }
        ProgramData::List { head, body }
            if *level > 0
                && matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(s) if s == unquote_splicing))
                && body.len() == 1 =>
        {
            *level -= 1;
            let res = if *level > 0 {
                let res =
                    quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: ctx.add_constant(Constant::Symbol(unquote_splicing)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                let mut res =
                    quasiquote_program(body[0], compiler, ctx, labels, requested_labels, level)?;
                res.push(Bytecode::Splice);
                res
            };
            *level += 1;
            res
        }
        // evaluate the list if level == 0, otherwise, quote the list (done here so that level is passed through)
        ProgramData::List { head, body } => {
            if *level > 0 {
                let mut data = vec![Bytecode::PushNull];
                let body_chunks = body
                    .iter()
                    .map(|it| {
                        quasiquote_program(*it, compiler, ctx, labels, requested_labels, level)
                    })
                    .collect::<Vec<_>>();
                for it in body_chunks.into_iter().rev() {
                    let mut code = it?;
                    if code.last().is_some_and(|c| !matches!(c, Bytecode::Splice)) {
                        code.push(Bytecode::MakePair);
                    }
                    data.extend(code);
                }
                match head {
                    ListHead::Program(p) => {
                        data.extend(quasiquote_program(
                            *p,
                            compiler,
                            ctx,
                            labels,
                            requested_labels,
                            level,
                        )?);
                    }
                    ListHead::Import => {
                        let import = ctx.interner.get_or_intern_static("import");
                        data.push(Bytecode::PushConst {
                            index: ctx.add_constant(Constant::Symbol(import)),
                        });
                    }
                    ListHead::DefineLibrary => {
                        let define_library = ctx.interner.get_or_intern_static("define-library");
                        data.push(Bytecode::PushConst {
                            index: ctx.add_constant(Constant::Symbol(define_library)),
                        });
                    }
                };
                data.push(Bytecode::MakePair);
                data
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        // If level == 0 here, will most likely result in an error, but yeah...
        ProgramData::DottedList { pre_dot, dot } => {
            if *level > 0 {
                debug_assert!(!pre_dot.is_empty());
                let body_chunks = pre_dot
                    .iter()
                    .map(|it| {
                        quasiquote_program(*it, compiler, ctx, labels, requested_labels, level)
                    })
                    .collect::<Vec<_>>();
                let mut data =
                    quasiquote_program(*dot, compiler, ctx, labels, requested_labels, level)?;
                for it in body_chunks.into_iter().rev() {
                    data.extend(it?);
                    data.push(Bytecode::MakePair);
                }
                data
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        // ditto on passthrough
        ProgramData::Vector(v) => {
            if *level > 0 {
                let mut data = vec![];
                let length = v.len();
                for it in v.iter() {
                    data.extend(quasiquote_program(
                        *it,
                        compiler,
                        ctx,
                        labels,
                        requested_labels,
                        level,
                    )?);
                }
                data.push(Bytecode::MakeVector { length });
                data
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        // ditto on passthrough
        ProgramData::Labeled { label, item } => {
            labels.insert(*label);
            if *level > 0 {
                let mut code =
                    quasiquote_program(*item, compiler, ctx, labels, requested_labels, level)?;

                code.push(Bytecode::FillHole { id: *label });
                code.push(Bytecode::MakeHole { id: *label });
                code
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        ProgramData::LabelRef(label) => {
            requested_labels.insert(*label);
            if *level > 0 {
                if let Some(val) = compiler.label_value(*label) {
                    quasiquote_program(val, compiler, ctx, labels, requested_labels, level)?
                } else {
                    // This code will fail anyways with a "undefined label" failure
                    vec![]
                }
            } else {
                evaluate!()
            }
        }
        // handle constant data (data that cannot contain data affected by quasiquote)
        _ => quote_program(ptr, compiler, ctx, labels, requested_labels)?,
    })
}

impl Syntax for Quasiquote {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("quasiquote expects exactly 1 argument"));
        }

        // Used for labeled datum
        let (labeled, requested) = compiler.label_data();
        let mut labels = HashSet::from_iter(labeled.clone());
        let mut requested_labels = HashSet::from_iter(requested.clone());
        let mut level = 1;
        let code: Vec<_> = quasiquote_program(
            args[0],
            compiler,
            ctx,
            &mut labels,
            &mut requested_labels,
            &mut level,
        )?;
        // when quasiquote is finished, we should be at the level we started at if we implemented it correctly
        debug_assert!(level == 1);
        // Error if there are any undefined labels
        let undefined_labels = requested_labels.difference(&labels).collect::<HashSet<_>>();
        if !undefined_labels.is_empty() {
            anyhow::bail!("undefined labels: {undefined_labels:?}")
        }
        compiler.add_labeled(labels);
        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }
}
