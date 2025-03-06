use gc_arena::Gc;

use crate::{
    bytecode::Bytecode,
    compiler::{Compiler, ProgramData, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
    runtime::lambda::CompiledLambda,
    stdlib::{Formals, base::lambda_helper},
};

#[derive(Debug)]
pub struct Define;

impl Syntax for Define {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.len() < 2 {
            return Err(anyhow::anyhow!("define must be given 2 or more arguments"));
        }

        let formals = Formals::convert(args[0], ctx.interner)?;

        match formals {
            Formals::Empty => Err(anyhow::anyhow!("define must be given a name to define"))?,
            Formals::Single(name) => {
                // (define <sym> <val> form)

                if args.len() != 2 {
                    return Err(anyhow::anyhow!(
                        "define must be given exactly 2 arguments in this form"
                    ));
                }

                // Prevent defining macros
                if compiler.get_macro(name).is_some() {
                    return Err(anyhow::anyhow!("cannot define macro"));
                }

                let Some(value) = args.get(1).cloned() else {
                    return Err(anyhow::anyhow!("define must be given 2 arguments"));
                };

                // Inform the compiler that a name is being defined in scope
                compiler.define_variable(name);

                Ok(SyntaxReturn::Code(
                    compiler
                        .compile_code(ctx, value)?
                        .into_bytecode()
                        .into_iter()
                        .chain([Bytecode::Define { symbol: name }, Bytecode::PushVoid])
                        .collect(),
                ))
            }
            Formals::List(syms) => {
                // (define (<name> <params>) <body>...) form

                // rejig the formals
                let name = *syms.first().unwrap();

                // Prevent defining macros
                if compiler.get_macro(name).is_some() {
                    return Err(anyhow::anyhow!("cannot define macro"));
                }

                let formals = if syms.len() == 1 {
                    Formals::Empty
                } else {
                    Formals::List(Box::from(&syms[1..]))
                };
                let chunk = lambda_helper(
                    compiler,
                    ctx,
                    import_env,
                    &formals,
                    args.iter().skip(1).copied(),
                );
                let index =
                    ctx.add_lambda(Gc::new(ctx, CompiledLambda::new(formals.arity(), chunk?)));
                Ok(SyntaxReturn::Code(Box::from([
                    Bytecode::PushLambda { index },
                    Bytecode::Define { symbol: name },
                    Bytecode::PushVoid,
                ])))
            }
            Formals::Dotted { pre_dot, dot } => {
                // (define (<name> <params>... . <rest>) <body>...) form

                // rejig the formals
                let name = *pre_dot.first().unwrap();

                // Prevent defining macros
                if compiler.get_macro(name).is_some() {
                    return Err(anyhow::anyhow!("cannot define macro"));
                }

                let formals = Formals::Dotted {
                    pre_dot: Box::from(&pre_dot[1..]),
                    dot,
                };
                let chunk = lambda_helper(
                    compiler,
                    ctx,
                    import_env,
                    &formals,
                    args.iter().skip(1).copied(),
                );
                let index =
                    ctx.add_lambda(Gc::new(ctx, CompiledLambda::new(formals.arity(), chunk?)));
                Ok(SyntaxReturn::Code(Box::from([
                    Bytecode::PushLambda { index },
                    Bytecode::Define { symbol: name },
                    Bytecode::PushVoid,
                ])))
            }
        }
    }

    fn is_definition(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct SetBang;

impl Syntax for SetBang {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.len() != 2 {
            return Err(anyhow::anyhow!("set! must be given exactly 2 arguments"));
        }

        let ProgramData::Symbol(name) = args[0].data else {
            return Err(anyhow::anyhow!(
                "set! must be given a symbol as its first argument"
            ));
        };

        // Make sure peeps are aware they can't set! macros
        if compiler.get_macro(name).is_some() {
            return Err(anyhow::anyhow!("cannot set! macro"));
        }

        let Some(value) = args.get(1).cloned() else {
            return Err(anyhow::anyhow!("define must be given 2 arguments"));
        };

        // Inform the compiler that a name is being defined in scope
        compiler.define_variable(name);

        Ok(SyntaxReturn::Code(
            compiler
                .compile_code(ctx, value)?
                .into_bytecode()
                .into_iter()
                .chain([Bytecode::SetBang { symbol: name }, Bytecode::PushVoid])
                .collect(),
        ))
    }
}
