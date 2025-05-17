use gc_arena::{Collect, Gc, RefLock};

use crate::{
    Syntax, SyntaxContext, SyntaxReturn, Value,
    bytecode::{Bytecode, Chunk},
    compiler::{ListHead, ProgramData, ProgramPtr},
    runtime::lambda::{
        Arity, CompiledLambda, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext,
    },
    value::Parameter,
};

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct MakeParameter;

impl<'gc> NativeLambda<'gc> for MakeParameter {
    fn arity(&self) -> Arity {
        Arity::Bounded { min: 1, max: 2 }
    }

    fn run(
        &mut self,
        ctx: NativeLambdaContext<'_, 'gc>,
        args: &[crate::ValuePtr<'gc>],
    ) -> Result<LambdaReturn<'gc>, LambdaError> {
        let init = args[0];
        let convert = match args.get(1).map(|ptr| *ptr.borrow()) {
            Some(Value::Lambda(convert)) if convert.arity().is_satisfied(1) => Some(convert),
            Some(_) => {
                return Err(anyhow::anyhow!(
                    "make-parameter expects a 1-arity lambda as its second argument"
                ))?;
            }
            None => None,
        };

        Ok(LambdaReturn::Return(vec![
            Value::Parameter(Gc::new(
                &ctx,
                RefLock::new(if let Some(convert) = convert {
                    Parameter::with_convert(init, convert)
                } else {
                    Parameter::new(init)
                }),
            ))
            .into_ptr(&ctx),
        ]))
    }
}

#[derive(Debug)]
pub struct Parameterize;

impl Syntax for Parameterize {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut crate::compiler::Compiler<'gc>,
        import_env: crate::environment::StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<crate::SyntaxReturn<'gc>> {
        if args.is_empty() {
            anyhow::bail!("parameterize expects at least 1 argument");
        }

        let param_list: Vec<(ProgramPtr<'gc>, ProgramPtr<'gc>)> = match &args[0].data {
            ProgramData::List { head, body } if matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::List { .. })) =>
            {
                // we expect a list of lists
                let ListHead::Program(head) = head else {
                    unreachable!();
                };

                std::iter::once(*head)
                    .chain(body.iter().copied())
                    .map(|p| match &p.data {
                        ProgramData::List { head, body } if body.len() == 1 => Ok((
                            head.into_program(ctx.mc, ctx.ecc.interner, args[0].source),
                            body[0],
                        )),
                        _ => Err(anyhow::anyhow!(
                            "parameterize expects its first argument to be a list of 2-tuples"
                        )),
                    })
                    .collect::<Result<Vec<_>, anyhow::Error>>()?
            }
            ProgramData::EmptyList => vec![],
            _ => anyhow::bail!("parameterize expects its first argument to be a list of 2-tuples"),
        };

        let parameterize_chunk =
            compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
                compiler.define_parameters(ctx.ecc.interner, vec![], None)?;

                let parameter_code = param_list
                    .into_iter()
                    .map(|(param, value)| {
                        let value_code = compiler
                            .compile_code(ctx, value)?
                            .into_bytecode()
                            .into_iter()
                            .collect::<Vec<_>>();
                        let param_code = compiler
                            .compile_code(ctx, param)?
                            .into_bytecode()
                            .into_iter()
                            .collect::<Vec<_>>();

                        Ok::<_, anyhow::Error>(
                            value_code
                                .into_iter()
                                .chain(param_code)
                                .chain([Bytecode::Parameterize])
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>();

                let mut labels = fxhash::FxHashMap::default();
                let mut body_code = vec![];

                for program in args.iter().skip(1).copied() {
                    if let Some(source) = program.source {
                        labels.insert(body_code.len() + parameter_code.len(), source);
                    }
                    body_code.extend(compiler.compile_code(ctx, program)?.into_bytecode());
                }

                let prelude: Vec<_> = compiler.lambda_prelude().into_iter().collect();
                // adjust code labels for arguments code
                for k in labels.keys().copied().collect::<Vec<_>>() {
                    let v = labels.remove(&k).expect("[ICE] mislabeled data");
                    labels.insert(k + prelude.len(), v);
                }

                // get the code all nice and joind together
                let code: Vec<_> = prelude
                    .into_iter()
                    .chain(parameter_code)
                    .chain(body_code)
                    .chain(compiler.lambda_postlude())
                    .collect();

                Ok::<_, anyhow::Error>(Chunk::new(
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
                ))
            })?;

        let lambda = ctx.add_lambda(Gc::new(
            ctx,
            CompiledLambda::new(ctx, Arity::Exact(0), parameterize_chunk, [], None),
        ));

        Ok(SyntaxReturn::Code(Box::new([
            Bytecode::PushLambda { index: lambda },
            Bytecode::Call { args: 0 },
        ])))
    }
}
