//! Implementations for (scheme base)

use std::{collections::HashSet, sync::Arc};

use gc_arena::{Collect, Gc, RefLock, unsize};

use crate::{
    Value,
    bytecode::{Bytecode, Chunk, Constant},
    compiler::{
        ArcSyntax, Compiler, ListHead, Module, ProgramData, ProgramPtr, Syntax, SyntaxContext,
        SyntaxReturn,
    },
    environment::StackEnvironmentPtr,
    runtime::{
        convert::IntoValue,
        lambda::{self, NativeLambda},
    },
};

#[derive(Debug)]
pub struct Define {
    /// What symbol is this being defined under?
    self_sym: lasso::Spur,
}

impl Syntax for Define {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // FIXME Make a "base" error, and remove the ad-hocness of these impls (maybe? I like them ad-hoc rn)
        if args.len() != 2 {
            return Err(anyhow::anyhow!("define must be given exactly 2 arguments"));
        }

        // For now, only support (define <sym> <val>) form
        let Some(ProgramData::Symbol(name)) = args.first().map(|p| &p.data) else {
            return Err(anyhow::anyhow!(
                "define must be given a symbol as the first argument"
            ));
        };

        // Save peeps from themselves (im looking at you, me) and don't
        // allow overriding 1 symbol (generally the symbol that defines this macro)
        if *name == self.self_sym {
            return Err(anyhow::anyhow!("cannot define definition macro"));
        }

        let Some(value) = args.get(1).cloned() else {
            return Err(anyhow::anyhow!("define must be given 2 arguments"));
        };

        // Inform the compiler that a name is being defined in scope
        compiler.define_variable(*name);

        Ok(SyntaxReturn::Code(
            compiler
                .compile_code(ctx, value)?
                .into_bytecode()
                .into_iter()
                .chain([Bytecode::Define { symbol: *name }, Bytecode::PushVoid])
                .collect(),
        ))
    }
}

#[derive(Debug)]
pub struct SetBang;

impl Syntax for SetBang {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        let _ = (ctx, compiler, import_env, args);
        todo!()
    }
}

#[derive(Debug)]
pub struct If;

impl Syntax for If {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        let _ = (ctx, compiler, import_env, args);
        todo!()
    }
}

#[derive(Debug)]
pub struct Lambda;

impl Syntax for Lambda {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.is_empty() {
            return Err(anyhow::anyhow!("lambda needs at least 1 argument"));
        }
        let arg_list = args[0];
        // FIXME make sure that arg_list is a list of symbols (or dotted list of symbols)
        // FIXME use / provide a Formals parser (that takes a program as input)
        let symbols = match &arg_list.data {
            // the None below should actually be an error
            _ => None::<(Box<[lasso::Spur]>, Option<lasso::Spur>)>,
        };
        // Make a new hygenic env
        let chunk = compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
            // build a chunk
            let mut code = vec![];
            let mut labels = fxhash::FxHashMap::default();
            let mut definitions_allowed = true;
            for program in args.iter().skip(1) {
                if !Compiler::is_definition(ctx.interner, *program) && definitions_allowed {
                    definitions_allowed = false;
                } else if Compiler::is_definition(ctx.interner, *program) && !definitions_allowed {
                    return Err(anyhow::anyhow!(
                        "lambda body requires all definitions before all expressions"
                    ));
                }
                if let Some(source) = program.source {
                    labels.insert(code.len(), source);
                }
                code.extend(compiler.compile_code(ctx, *program)?.into_bytecode());
            }

            if definitions_allowed {
                return Err(anyhow::anyhow!(
                    "lambda body must have at least 1 expression"
                ));
            }

            Ok(Chunk::new(
                ctx,
                code,
                ctx.constants(),
                ctx.lambdas(),
                import_env,
                labels,
            ))
        });
        // TODO Optmization opportunity: if the source code for a lambda is the same, we
        // don't actually have to recompile the instructions, we would just be in a
        // different import env (and have change labels to match our labels)
        dbg!((arg_list, &chunk));
        let index = ctx.add_lambda(Gc::new(
            ctx,
            lambda::CompiledLambda::new(lambda::Arity::AtLeast(0), chunk?),
        ));

        Ok(SyntaxReturn::Code(Box::from([
            // TODO Add fetch arg instructions for the param list
            Bytecode::PushLambda { index },
        ])))
    }
}

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

#[derive(Collect, Debug)]
#[collect(require_static)]
pub struct CallCc;

impl NativeLambda for CallCc {
    fn arity(&self) -> lambda::Arity {
        lambda::Arity::Exact(1)
    }

    fn run<'gc>(
        &mut self,
        ctx: lambda::NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<lambda::LambdaReturn<'gc>, lambda::LambdaError> {
        // get the continuation of the stack frame right above us
        let cont = ctx.thread_ref.create_continuation(true);

        let Some(Value::Lambda(lambda)) = args.first().map(|p| *p.borrow()) else {
            return Err(
                anyhow::anyhow!("call-with-current-continuation must be given a lambda").into(),
            );
        };

        if ctx.get_arity(self, lambda).is_satisfied(1) {
            return Err(anyhow::anyhow!(
                "call-with-current-continuation must be given a 1-arity lambda"
            )
            .into());
        }

        Ok(lambda::LambdaReturn::TailCall {
            lambda,
            args: vec![Gc::new(&ctx, cont).into_value(&ctx).into_ptr(&ctx)],
            dynamic_wind: None,
        })
    }
}

/// (scheme base) module
pub struct Base;

impl Module for Base {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> HashSet<lasso::Spur> {
        [
            "call-with-current-continuation",
            "call/cc",
            "define",
            "lambda",
            "quote",
            "set!",
            "if",
        ]
        .into_iter()
        .map(|s| interner.get_or_intern_static(s))
        .collect()
    }

    fn value<'gc>(
        &self,
        mc: &gc_arena::Mutation<'gc>,
        symbol: &str,
    ) -> Option<crate::ValuePtr<'gc>> {
        match symbol {
            "call-with-current-continuation" | "call/cc" => Some(
                lambda::Lambda::Native(
                    unsize![Gc::new(mc, RefLock::new(CallCc)) => RefLock<dyn lambda::NativeLambda>],
                )
                .into_value(mc)
                .into_ptr(mc),
            ),
            _ => None,
        }
    }

    fn syntax(&self, interner: &mut lasso::Rodeo, symbol: lasso::Spur) -> Option<ArcSyntax> {
        match interner.resolve(&symbol) {
            "define" => Some(Arc::new(Define { self_sym: symbol })),
            "lambda" => Some(Arc::new(Lambda)),
            "set!" => Some(Arc::new(SetBang)),
            "if" => Some(Arc::new(If)),
            "quote" => Some(Arc::new(Quote)),
            _ => None,
        }
    }
}
