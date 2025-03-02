//! Implementations for (scheme base)

use std::{collections::HashSet, sync::Arc};

use gc_arena::{Gc, RefLock, unsize};
use procedures::{Add, Subtract};

use crate::{
    bytecode::{Bytecode, Chunk},
    compiler::{ArcSyntax, Compiler, Module, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
    runtime::{convert::IntoValue, lambda},
};

pub use conditionals::If;
pub use define::{Define, SetBang};
pub use procedures::CallCc;
pub use quote::Quote;

mod conditionals;
mod define;
mod procedures;
mod quote;

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
            "+",
            "-",
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
            "call-with-current-continuation" | "call/cc" => lambda!(CallCc),
            "+" => lambda!(Add),
            "-" => lambda!(Subtract),
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
