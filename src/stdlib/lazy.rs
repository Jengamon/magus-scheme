use std::sync::Arc;

use crate::{
    compiler::Module,
    runtime::{convert::IntoValue, lambda},
};

use gc_arena::{Gc, RefLock, unsize};

pub use procedures::{Force, IsPromise, MakePromise};
pub use syntax::{Delay, DelayForce};

mod syntax {
    use gc_arena::{Gc, RefLock, unsize};

    use crate::{
        Syntax, SyntaxReturn,
        bytecode::{Bytecode, Chunk},
        runtime::lambda::{Arity, CompiledLambda, Lambda, NativeLambda},
        value::Promise,
    };

    use super::procedures;

    #[derive(Debug)]
    pub struct Delay;

    impl Syntax for Delay {
        fn evaluate<'gc>(
            &self,
            ctx: &mut crate::SyntaxContext<'_, 'gc>,
            compiler: &mut crate::compiler::Compiler<'gc>,
            import_env: crate::environment::StackEnvironmentPtr<'gc>,
            args: &[crate::compiler::ProgramPtr<'gc>],
        ) -> anyhow::Result<SyntaxReturn<'gc>> {
            if args.len() != 1 {
                anyhow::bail!("delay expects exactly 1 argument");
            }

            let promise_lambda =
                compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
                    compiler.define_parameters(ctx.interner, vec![], None)?;
                    let code = compiler.compile_code(ctx, args[0])?.into_bytecode();
                    let mut labels = if let Some(source) = args[0].source {
                        [(0, source)].into_iter().collect()
                    } else {
                        fxhash::FxHashMap::default()
                    };
                    let prelude: Vec<_> = compiler.lambda_prelude().into_iter().collect();
                    // adjust code labels for arguments code
                    for k in labels.keys().copied().collect::<Vec<_>>() {
                        let v = labels.remove(&k).expect("[ICE] mislabeled data");
                        labels.insert(k + prelude.len(), v);
                    }
                    let code: Vec<_> = prelude
                        .into_iter()
                        .chain(code)
                        .chain(compiler.lambda_postlude())
                        .collect();
                    let compiled = Gc::new(
                        ctx,
                        CompiledLambda::new(
                            Arity::Exact(0),
                            Chunk::new(
                                ctx,
                                code,
                                ctx.constants(),
                                ctx.lambdas(),
                                ctx.promises(),
                                ctx.upvalues(),
                                import_env,
                                labels,
                            ),
                        ),
                    );
                    Ok::<_, anyhow::Error>(Lambda::Compiled(compiled))
                })?;

            let prom = Gc::new(ctx, RefLock::new(Promise::Unevaled(promise_lambda)));
            let index = ctx.add_promise(prom);
            Ok(SyntaxReturn::Code(Box::from([Bytecode::PushPromise {
                index,
            }])))
        }
    }

    #[derive(Debug)]
    pub struct DelayForce;

    impl Syntax for DelayForce {
        fn evaluate<'gc>(
            &self,
            ctx: &mut crate::SyntaxContext<'_, 'gc>,
            compiler: &mut crate::compiler::Compiler<'gc>,
            import_env: crate::environment::StackEnvironmentPtr<'gc>,
            args: &[crate::compiler::ProgramPtr<'gc>],
        ) -> anyhow::Result<SyntaxReturn<'gc>> {
            if args.len() != 1 {
                anyhow::bail!("delay-force expects exactly 1 argument");
            }

            let delay_index = {
                let promise_lambda =
                    compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
                        compiler.define_parameters(ctx.interner, vec![], None)?;
                        let force_lambda = ctx.add_native_lambda(unsize!(Gc::new(ctx,
                                RefLock::new(procedures::Force)) => RefLock<dyn NativeLambda>));
                        let code = compiler.compile_code(ctx, args[0])?.into_bytecode();
                        let mut labels = if let Some(source) = args[0].source {
                            [(0, source)].into_iter().collect()
                        } else {
                            fxhash::FxHashMap::default()
                        };
                        let prelude: Vec<_> = compiler.lambda_prelude().into_iter().collect();
                        // adjust code labels for arguments code
                        for k in labels.keys().copied().collect::<Vec<_>>() {
                            let v = labels.remove(&k).expect("[ICE] mislabeled data");
                            labels.insert(k + prelude.len(), v);
                        }
                        let code: Vec<_> = prelude
                            .into_iter()
                            .chain(code)
                            .chain([
                                Bytecode::PushLambda {
                                    index: force_lambda,
                                },
                                Bytecode::Call { args: 1 },
                            ])
                            .chain(compiler.lambda_postlude())
                            .collect();
                        let compiled = Gc::new(
                            ctx,
                            CompiledLambda::new(
                                Arity::Exact(0),
                                Chunk::new(
                                    ctx,
                                    code,
                                    ctx.constants(),
                                    ctx.lambdas(),
                                    ctx.promises(),
                                    ctx.upvalues(),
                                    import_env,
                                    labels,
                                ),
                            ),
                        );
                        Ok::<_, anyhow::Error>(compiled)
                    })?;

                ctx.add_promise(Gc::new(
                    ctx,
                    RefLock::new(Promise::Unevaled(Lambda::Compiled(promise_lambda))),
                ))
            };

            Ok(SyntaxReturn::Code(Box::from([Bytecode::PushPromise {
                index: delay_index,
            }])))
        }
    }
}

mod procedures {
    use gc_arena::{Collect, Gc, RefLock};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaResult, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::Promise,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsPromise;

    impl NativeLambda for IsPromise {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            // TODO Do we allow the degenerate case of a cons cell with (None None)?
            // It does mean that we can just do a pointer comparison...
            //
            // I think no, for the stdlib, the only pair considered to be null is the thread null value
            let val = matches!(*args[0].borrow(), Value::Promise(_));

            Ok(LambdaReturn::Return(vec![Value::Bool(val).into_ptr(&ctx)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Force;

    impl NativeLambda for Force {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            let Value::Promise(prom) = *args[0].borrow() else {
                // value is not a promise, return as-is
                return Ok(LambdaReturn::Return(vec![args[0]]));
            };

            if let Promise::Evaled(val) = *prom.borrow() {
                Ok(LambdaReturn::Return(vec![val]))
            } else if ctx.stack.is_empty() {
                // evaluate the promise
                let Promise::Unevaled(lambda) = *prom.borrow() else {
                    unreachable!()
                };

                Ok(LambdaReturn::Call {
                    lambda,
                    args: vec![],
                    dynamic_wind: None,
                })
            } else {
                // store the promise value, and return it!
                let Some(val) = ctx.stack.last().copied() else {
                    unreachable!()
                };
                Promise::promise(&ctx, prom, val);
                Ok(LambdaReturn::Return(vec![val]))
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct MakePromise;

    impl NativeLambda for MakePromise {
        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run<'gc>(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            match *args[0].borrow() {
                Value::Promise(_) => Ok(LambdaReturn::Return(vec![args[0]])),
                _ => {
                    let new_prom =
                        Value::Promise(Gc::new(&ctx, RefLock::new(Promise::Evaled(args[0]))))
                            .into_ptr(&ctx);
                    Ok(LambdaReturn::Return(vec![new_prom]))
                }
            }
        }
    }
}

pub struct Lazy;

impl Module for Lazy {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        ["delay", "delay-force", "force", "promise?", "make-promise"]
            .into_iter()
            .map(|s| interner.get_or_intern_static(s))
            .collect()
    }

    fn syntax(
        &self,
        interner: &mut lasso::Rodeo,
        symbol: lasso::Spur,
    ) -> Option<crate::compiler::ArcSyntax> {
        match interner.resolve(&symbol) {
            "delay" => Some(Arc::new(Delay)),
            "delay-force" => Some(Arc::new(DelayForce)),
            _ => None,
        }
    }

    fn value<'gc>(
        &self,
        mc: &gc_arena::Mutation<'gc>,
        symbol: &str,
    ) -> Option<crate::ValuePtr<'gc>> {
        macro_rules! lambda {
            ($lmb:expr) => {
                 Some(
                    lambda::Lambda::Native(
                        unsize![Gc::new(mc, RefLock::new($lmb)) => RefLock<dyn lambda::NativeLambda>],
                    )
                    .into_value(mc)
                    .into_ptr(mc),
                )
            };
        }

        match symbol {
            "force" => lambda!(Force),
            "make-promise" => lambda!(MakePromise),
            "promise?" => lambda!(IsPromise),
            _ => None,
        }
    }
}
