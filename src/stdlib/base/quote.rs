use std::{collections::HashSet, sync::Arc};

use crate::{
    bytecode::{Bytecode, Constant},
    compiler::{Compiler, ListHead, ProgramData, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
};

struct QuoteContext<'a, 'b, 'c, 'gc> {
    compiler: &'c mut Compiler<'gc>,
    ctx: &'c mut SyntaxContext<'a, 'b, 'gc>,
    labels: &'c mut HashSet<usize>,
    requested_labels: &'c mut HashSet<usize>,

    /// contains the raw addresses of gc pointers that quotation has expanded
    expanded: &'c mut HashSet<usize>,
}

fn list_quote_prelude<'gc>(
    head: &ListHead<'gc>,
    body: &[ProgramPtr<'gc>],
    qctx: &mut QuoteContext<'_, '_, '_, 'gc>,
) -> Vec<Bytecode> {
    if let ListHead::Program(p) = head {
        Some(*p)
    } else {
        None
    }
    .into_iter()
    .chain(body.iter().copied())
    .filter_map(|p| {
        if let ProgramData::LabelRef(index) = &p.data {
            if let Some(data) = qctx.compiler.label_value(*index) {
                if !qctx.expanded.contains(&(&raw const *data).addr()) {
                    qctx.expanded.insert((&raw const *data).addr());
                    let mut code = quote_program(data, qctx).ok()?;
                    code.push(Bytecode::FillQuoteHole { id: *index });
                    Some(code)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    })
    .flatten()
    .collect::<Vec<_>>()
}

// TODO Introduce a separate label set for "requested labels"
// and the label check becomes: requested - labeled (difference)
fn quote_program<'gc>(
    ptr: ProgramPtr<'gc>,
    qctx: &mut QuoteContext<'_, '_, '_, 'gc>,
) -> anyhow::Result<Vec<Bytecode>> {
    macro_rules! constant_eval {
        ($e:expr => $f:ident) => {
            vec![Bytecode::PushConst {
                index: qctx.ctx.add_constant(Constant::$f($e)),
            }]
        };
    }

    Ok(match &ptr.data {
        ProgramData::Number(n) => constant_eval!(n.clone() => Number),
        // ProgramData::Integer(i) => constant_eval!(*i => Number),
        // ProgramData::Rational(sign, numer, denom) if *denom != 0 => {
        //     let index = ctx.add_constant(Constant::Rational(*sign, *numer, *denom));
        //     vec![Bytecode::PushConst { index }]
        // }
        // ProgramData::Rational(_, _, _) => anyhow::bail!("ratio over 0 in source"),
        ProgramData::Inexact(f) => constant_eval!(*f => Inexact),
        ProgramData::Bytevector(bv) => constant_eval!(Arc::from(bv.as_ref()) => Bytevector),
        ProgramData::String(s) => {
            constant_eval!(Arc::from(s.as_ref()) => String)
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
            qctx.labels.insert(*label);

            if !qctx.expanded.contains(&(&raw const *item).addr()) {
                qctx.expanded.insert((&raw const *item).addr());
                let mut code = quote_program(*item, qctx)?;
                code.extend([
                    Bytecode::FillQuoteHole { id: *label },
                    Bytecode::MakeQuoteHole { id: *label },
                ]);
                code
            } else {
                vec![Bytecode::MakeQuoteHole { id: *label }]
            }
        }
        ProgramData::LabelRef(label) => {
            qctx.requested_labels.insert(*label);

            vec![Bytecode::MakeQuoteHole { id: *label }]
        }
        ProgramData::Vector(v) => {
            let v_chunks = v
                .iter()
                .map(|it| quote_program(*it, qctx))
                .collect::<Vec<_>>();
            let length = v_chunks.len();
            let mut data = v_chunks
                .into_iter()
                .collect::<Result<Vec<_>, _>>()?
                .concat();
            data.push(Bytecode::MakeVector { length });
            data
        }
        ProgramData::EmptyList => vec![Bytecode::PushNull],
        ProgramData::List { head, body } => {
            // TODO Hoist labels here so that labels (and label refs) only need reference the created hole)
            // so that `test_data/quasiquote-labeled.sct` passes
            let prelude = list_quote_prelude(head, body, qctx);
            let mut data = vec![Bytecode::PushNull];
            let body_chunks = body
                .iter()
                .map(|it| quote_program(*it, qctx))
                .collect::<Vec<_>>();
            for it in body_chunks.into_iter().rev() {
                data.extend(it?);
                data.push(Bytecode::MakePair);
            }
            match head {
                ListHead::Program(p) => {
                    data.extend(quote_program(*p, qctx)?);
                }
                ListHead::Import => {
                    let import = qctx.ctx.ecc.interner.get_or_intern_static("import");
                    data.push(Bytecode::PushConst {
                        index: qctx.ctx.add_constant(Constant::Symbol(import)),
                    });
                }
                ListHead::DefineLibrary => {
                    let define_library =
                        qctx.ctx.ecc.interner.get_or_intern_static("define-library");
                    data.push(Bytecode::PushConst {
                        index: qctx.ctx.add_constant(Constant::Symbol(define_library)),
                    });
                }
            };
            data.push(Bytecode::MakePair);
            prelude.into_iter().chain(data).collect()
        }
        ProgramData::DottedList { pre_dot, dot } => {
            debug_assert!(!pre_dot.is_empty());
            // TODO Hoist labels here so that labels (and label refs) only need reference the created hole)
            // so that `test_data/quasiquote-labeled.sct` passes
            let body_chunks = pre_dot
                .iter()
                .map(|it| quote_program(*it, qctx))
                .collect::<Vec<_>>();
            let mut data = quote_program(*dot, qctx)?;
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
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
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
        let mut expanded = HashSet::new();
        let mut qctx = QuoteContext {
            compiler,
            ctx,
            labels: &mut labels,
            requested_labels: &mut requested_labels,
            expanded: &mut expanded,
        };
        let code = quote_program(args[0], &mut qctx)?;
        compiler.add_labeled(labels);
        compiler.add_label_refs(requested_labels);
        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }
}

#[derive(Debug)]
pub struct Quasiquote;

fn quasiquote_program<'gc>(
    ptr: ProgramPtr<'gc>,
    qctx: &mut QuoteContext<'_, '_, '_, 'gc>,
    level: &mut usize,
    in_list: bool,
) -> anyhow::Result<Vec<Bytecode>> {
    macro_rules! evaluate {
        () => {
            qctx.compiler
                .compile_code(qctx.ctx, ptr)?
                .into_bytecode()
                .into_iter()
                .collect()
        };
    }

    let quasiquote = qctx.ctx.ecc.interner.get_or_intern_static("quasiquote");
    let unquote = qctx.ctx.ecc.interner.get_or_intern_static("unquote");
    let unquote_splicing = qctx
        .ctx
        .ecc
        .interner
        .get_or_intern_static("unquote-splicing");

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
                let res = quasiquote_program(body[0], qctx, level, in_list)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: qctx.ctx.add_constant(Constant::Symbol(quasiquote)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                quasiquote_program(body[0], qctx, level, in_list)?
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
                let res = quasiquote_program(body[0], qctx, level, in_list)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: qctx.ctx.add_constant(Constant::Symbol(unquote)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                quasiquote_program(body[0], qctx, level, in_list)?
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
            if !in_list {
                *level += 1;
            }
            let res = if *level > 0 {
                let prelude = if let ProgramData::LabelRef(index) = &body[0].data {
                    if let Some(data) = qctx.compiler.label_value(*index) {
                        let mut code = quasiquote_program(data, qctx, level, in_list)?;
                        code.push(Bytecode::FillQuoteHole { id: *index });
                        code
                    } else {
                        vec![]
                    }
                } else {
                    vec![]
                };
                let res = quasiquote_program(body[0], qctx, level, in_list)?;
                [Bytecode::PushNull]
                    .into_iter()
                    .chain(prelude)
                    .chain(res)
                    .chain([
                        Bytecode::MakePair,
                        Bytecode::PushConst {
                            index: qctx.ctx.add_constant(Constant::Symbol(unquote_splicing)),
                        },
                        Bytecode::MakePair,
                    ])
                    .collect()
            } else {
                let mut res = quasiquote_program(body[0], qctx, level, in_list)?;
                res.push(Bytecode::Splice);
                res
            };
            if in_list {
                *level += 1;
            }
            res
        }
        // evaluate the list if level == 0, otherwise, quote the list (done here so that level is passed through)
        ProgramData::List { head, body } => {
            if *level > 0 {
                // TODO Hoist labels here so that labels (and label refs) only need reference the created hole)
                // so that `test_data/quasiquote-labeled.sct` passes
                let mut data = vec![Bytecode::PushNull];
                let body_chunks = body
                    .iter()
                    .map(|it| quasiquote_program(*it, qctx, level, true))
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
                        data.extend(quasiquote_program(*p, qctx, level, true)?);
                    }
                    ListHead::Import => {
                        let import = qctx.ctx.ecc.interner.get_or_intern_static("import");
                        data.push(Bytecode::PushConst {
                            index: qctx.ctx.add_constant(Constant::Symbol(import)),
                        });
                    }
                    ListHead::DefineLibrary => {
                        let define_library =
                            qctx.ctx.ecc.interner.get_or_intern_static("define-library");
                        data.push(Bytecode::PushConst {
                            index: qctx.ctx.add_constant(Constant::Symbol(define_library)),
                        });
                    }
                };
                if data.last().is_some_and(|c| !matches!(c, Bytecode::Splice)) {
                    data.push(Bytecode::MakePair);
                }
                data
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        // If level == 0 here, will most likely result in an error, but yeah...
        ProgramData::DottedList { pre_dot, dot } => {
            if *level > 0 {
                // TODO Hoist labels here so that labels (and label refs) only need reference the created hole)
                // so that `test_data/quasiquote-labeled.sct` passes
                debug_assert!(!pre_dot.is_empty());
                let body_chunks = pre_dot
                    .iter()
                    .map(|it| quasiquote_program(*it, qctx, level, true))
                    .collect::<Vec<_>>();
                let mut data = quasiquote_program(*dot, qctx, level, true)?;
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
                // TODO Hoist labels here so that labels (and label refs) only need reference the created hole)
                // so that `test_data/quasiquote-labeled.sct` passes
                let mut data = vec![Bytecode::PushNull];
                let v_chunks = v
                    .iter()
                    .map(|it| quasiquote_program(*it, qctx, level, true))
                    .collect::<Vec<_>>();
                for it in v_chunks.into_iter().rev() {
                    let mut it = it?;
                    if it.last().is_some_and(|c| !matches!(c, Bytecode::Splice)) {
                        it.push(Bytecode::MakePair);
                    }
                    data.extend(it);
                }
                data.push(Bytecode::ListToVector);
                data
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        // ditto on passthrough
        ProgramData::Labeled { label, item } => {
            qctx.labels.insert(*label);
            if *level > 0 {
                quasiquote_program(*item, qctx, level, in_list)?
            } else {
                // evaluate the list
                evaluate!()
            }
        }
        ProgramData::LabelRef(label) => {
            qctx.requested_labels.insert(*label);
            if *level > 0 {
                if let Some(val) = qctx.compiler.label_value(*label) {
                    quasiquote_program(val, qctx, level, in_list)?
                } else {
                    // This code will fail anyways with a "undefined label" failure
                    vec![]
                }
            } else if let Some(program) = qctx.compiler.label_value(*label) {
                // get the value of a label (provided by the compiler)
                let mut code: Vec<_> = qctx
                    .compiler
                    .compile_code(qctx.ctx, program)?
                    .into_bytecode()
                    .into_iter()
                    .collect();
                code.push(Bytecode::FillHole { id: *label });
                code.push(Bytecode::MakeHole { id: *label });
                code
            } else {
                evaluate!()
            }
        }
        // handle constant data (data that cannot contain data affected by quasiquote)
        _ => quote_program(ptr, qctx)?,
    })
}

impl Syntax for Quasiquote {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
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
        let mut expanded = HashSet::new();
        let mut qctx = QuoteContext {
            compiler,
            ctx,
            labels: &mut labels,
            requested_labels: &mut requested_labels,
            expanded: &mut expanded,
        };
        let code: Vec<_> = quasiquote_program(args[0], &mut qctx, &mut level, false)?;
        // when quasiquote is finished, we should be at the level we started at if we implemented it correctly
        debug_assert!(level == 1);
        compiler.add_labeled(labels);
        compiler.add_label_refs(requested_labels);
        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }
}
