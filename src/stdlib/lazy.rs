use std::sync::Arc;

use crate::{
    LibraryName,
    compiler::Module,
    interpreter::Registerable,
    library_name,
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
            ctx: &mut crate::SyntaxContext<'_, '_, 'gc>,
            compiler: &mut crate::compiler::Compiler<'gc>,
            import_env: crate::environment::StackEnvironmentPtr<'gc>,
            args: &[crate::compiler::ProgramPtr<'gc>],
        ) -> anyhow::Result<SyntaxReturn<'gc>> {
            if args.len() != 1 {
                anyhow::bail!("delay expects exactly 1 argument");
            }

            let promise_lambda =
                compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
                    compiler.define_parameters(ctx.ecc.interner, vec![], None)?;

                    if compiler.is_definition(args[0]) {
                        // Reject non-value delay
                        anyhow::bail!("delay: missing value(s) to return")
                    }
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
                            ctx,
                            Arity::Exact(0),
                            Chunk::new(
                                compiler,
                                ctx,
                                code,
                                ctx.constants(),
                                ctx.lambdas(),
                                ctx.macros(),
                                ctx.promises(),
                                ctx.upvalues(),
                                import_env,
                                labels,
                            ),
                            [],
                            None,
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
            ctx: &mut crate::SyntaxContext<'_, '_, 'gc>,
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
                        compiler.define_parameters(ctx.ecc.interner, vec![], None)?;

                        if compiler.is_definition(args[0]) {
                            // Reject non-value delay
                            anyhow::bail!("delay-force: missing value(s) to return")
                        }
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
                                ctx,
                                Arity::Exact(0),
                                Chunk::new(
                                    compiler,
                                    ctx,
                                    code,
                                    ctx.constants(),
                                    ctx.lambdas(),
                                    ctx.macros(),
                                    ctx.promises(),
                                    ctx.upvalues(),
                                    import_env,
                                    labels,
                                ),
                                [],
                                None,
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
        environment::StackEnvironment,
        runtime::lambda::{Arity, LambdaResult, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::Promise,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsPromise;

    impl<'gc> NativeLambda<'gc> for IsPromise {
        fn name(&self) -> &str {
            "promise?"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> LambdaResult<'gc> {
            let is_promise = matches!(*args[0].borrow(), Value::Promise(_));

            Ok(LambdaReturn::Return(vec![ctx.bool(is_promise)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Force;

    impl<'gc> NativeLambda<'gc> for Force {
        fn name(&self) -> &str {
            "force"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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

                // collect arguments that are currently defined (as they are *not* stored in the any environment)
                // and associate them with an environment so that the captured promise executes with knowledge of
                // argument values too (this is why REFR *also* still exists, it might reference something with
                // this magic [we just shouldn't go out of our way to try and find a value])
                let mut arg_env = StackEnvironment::new(&ctx, None);
                for frame in ctx.frames.iter().rev() {
                    if let Some((arg_names, rest_name)) = frame.arg_name_data() {
                        let args = frame.args();
                        for (name, value) in arg_names.iter().copied().zip(args) {
                            if arg_env.is_defined(name).is_none() {
                                arg_env.define(&ctx, name, *value, false).unwrap();
                            }
                        }

                        if let Some((name, value)) = rest_name.zip(frame.rest_arg()) {
                            if arg_env.is_defined(name).is_none() {
                                arg_env.define(&ctx, name, value, false).unwrap();
                            }
                        }
                    }

                    for (name, value) in frame.tail_called_args() {
                        arg_env.define(&ctx, name.0, *value, false).unwrap();
                    }
                }

                // Technically we want a new env where our local env is preserved, but
                // this parent env interferes
                let mut current_env = ctx.frames.last().map(|f| f.env_raw()).unwrap();
                let current_env_parent = current_env.parent();
                arg_env.reparent(current_env_parent);
                current_env.reparent(Some(Gc::new(&ctx, RefLock::new(arg_env))));

                Ok(LambdaReturn::Call {
                    lambda,
                    args: vec![],
                    dynamic_wind: None,
                    env: Some(Gc::new(&ctx, RefLock::new(current_env))),
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

    impl<'gc> NativeLambda<'gc> for MakePromise {
        fn name(&self) -> &str {
            "make-promise"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
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

impl Registerable for Lazy {
    fn name(interner: &mut lasso::Rodeo) -> crate::LibraryName {
        LibraryName::from_iter(library_name!(interner => scheme lazy))
    }

    fn native(&self) -> Option<Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
        // We are a ZST, so we can do this~
        Some(Arc::new(Self))
    }

    fn scheme(&self) -> Option<(&str, &str)> {
        None
    }

    fn scheme_native(
        &self,
        _interner: &mut lasso::Rodeo,
    ) -> Vec<(
        LibraryName,
        std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
    )> {
        Vec::new()
    }
}
