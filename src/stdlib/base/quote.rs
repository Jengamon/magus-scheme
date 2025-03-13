use std::{collections::HashSet, sync::Arc};

use crate::{
    bytecode::{Bytecode, Constant},
    compiler::{Compiler, ListHead, ProgramData, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
};

#[derive(Debug)]
pub struct Quote;

impl Syntax for Quote {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        _compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.len() != 1 {
            return Err(anyhow::anyhow!("quote expects exactly 1 argument"));
        }

        fn quote_program<'gc>(
            ptr: ProgramPtr<'gc>,
            ctx: &mut SyntaxContext<'_, 'gc>,
            labels: &mut HashSet<usize>,
        ) -> anyhow::Result<Vec<Bytecode>> {
            macro_rules! constant_eval {
                ($e:expr => $f:ident) => {
                    vec![Bytecode::PushConst {
                        index: ctx.push_constant(Constant::$f($e)),
                    }]
                };
            }

            Ok(match &ptr.data {
                ProgramData::Integer(i) => constant_eval!(*i => Number),
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

                    let mut code = quote_program(*item, ctx, labels)?;
                    code.push(Bytecode::Duplicate);
                    code.push(Bytecode::FillHole { id: *label });
                    code
                }
                ProgramData::LabelRef(label) => {
                    if !labels.contains(label) {
                        return Err(anyhow::anyhow!("undefined label reference {label}"));
                    }
                    vec![Bytecode::MakeHole { id: *label }]
                }
                ProgramData::Vector(v) => {
                    let mut data = vec![];
                    let length = v.len();
                    for it in v.iter() {
                        data.extend(quote_program(*it, ctx, labels)?);
                    }
                    data.push(Bytecode::MakeVector { length });
                    data
                }
                ProgramData::EmptyList => vec![Bytecode::PushNull],
                ProgramData::List { head, body } => {
                    let mut data = vec![Bytecode::PushNull];
                    for it in body.iter().rev() {
                        data.extend(quote_program(*it, ctx, labels)?);
                        data.push(Bytecode::MakePair);
                    }
                    match head {
                        ListHead::Program(p) => {
                            data.extend(quote_program(*p, ctx, labels)?);
                        }
                        ListHead::Import => {
                            let import = ctx.interner.get_or_intern_static("import");
                            data.push(Bytecode::PushConst {
                                index: ctx.push_constant(Constant::Symbol(import)),
                            });
                        }
                        ListHead::DefineLibrary => {
                            let define_library =
                                ctx.interner.get_or_intern_static("define-library");
                            data.push(Bytecode::PushConst {
                                index: ctx.push_constant(Constant::Symbol(define_library)),
                            });
                        }
                    };
                    data.push(Bytecode::MakePair);
                    data
                }
                ProgramData::DottedList { pre_dot, dot } => {
                    debug_assert!(!pre_dot.is_empty());
                    let mut data = quote_program(*dot, ctx, labels)?;
                    for it in pre_dot.iter().rev() {
                        data.extend(quote_program(*it, ctx, labels)?);
                        data.push(Bytecode::MakePair);
                    }
                    data
                }
            })
        }

        // Used for evaluating labeled datum
        let mut labels = HashSet::default();
        let code = quote_program(args[0], ctx, &mut labels)?;
        Ok(SyntaxReturn::Code(Box::from(code.as_slice())))
    }
}
