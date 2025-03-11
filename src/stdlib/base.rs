//! Implementations for (scheme base)

use std::{collections::HashSet, sync::Arc};

use gc_arena::{Gc, RefLock, unsize};

use crate::{
    bytecode::{Bytecode, Chunk, ChunkPtr},
    compiler::{ArcSyntax, Compiler, Module, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
    runtime::{convert::IntoValue, lambda},
};

pub use conditionals::If;
pub use define::{Define, SetBang};
pub use procedures::{
    Add, Apply, Ascending, Caar, Cadr, CallCc, Car, Cdar, Cddr, Cdr, Cons, Descending, Equal, IsEq,
    IsEqv, IsNull, IsPair, MonotonicAscending, MonotonicDescending, Mul, Subtract, Values,
};
pub use quote::Quote;

use super::Formals;

mod conditionals;
mod define;
mod procedures;
mod quote;

// helper function for compiling a lambda
pub fn lambda_helper<'gc>(
    compiler: &mut Compiler<'gc>,
    ctx: &mut SyntaxContext<'_, 'gc>,
    import_env: StackEnvironmentPtr<'gc>,
    formals: &Formals,
    body: impl IntoIterator<Item = ProgramPtr<'gc>>,
) -> anyhow::Result<ChunkPtr<'gc>> {
    compiler.hygenic(ctx, import_env, |ctx, compiler, import_env| {
        compiler.define_arguments(formals.non_rest_params(), formals.rest_param());

        let mut labels = fxhash::FxHashMap::default();
        let mut definitions_allowed = true;
        let mut program_code = Vec::new();
        for program in body {
            if !compiler.is_definition(program) && definitions_allowed {
                definitions_allowed = false;
            } else if compiler.is_definition(program) && !definitions_allowed {
                return Err(anyhow::anyhow!(
                    "lambda body requires all definitions before all expressions"
                ));
            }
            if let Some(source) = program.source {
                labels.insert(program_code.len(), source);
            }
            program_code.extend(compiler.compile_code(ctx, program)?.into_bytecode());
        }

        if definitions_allowed {
            return Err(anyhow::anyhow!(
                "lambda body must have at least 1 expression"
            ));
        }

        // join argument defs, then program code (done here, so that upvalues are known)
        let prelude = compiler.lambda_prelude().into_iter().collect::<Vec<_>>();
        // adjust code labels for arguments code
        for k in labels.keys().copied().collect::<Vec<_>>() {
            let v = labels.remove(&k).expect("[ICE] mislabeled data");
            labels.insert(k + prelude.len(), v);
        }
        // get the code all nice and joind together
        let code: Vec<_> = prelude
            .into_iter()
            .chain(program_code)
            .chain(compiler.lambda_postlude())
            .collect();

        Ok(Chunk::new(
            ctx,
            code,
            ctx.constants(),
            ctx.lambdas(),
            ctx.promises(),
            ctx.upvalues(),
            import_env,
            labels,
        ))
    })
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
        let formals = Formals::convert(arg_list, ctx.interner)?;
        // Make a new hygenic env
        let chunk = lambda_helper(
            compiler,
            ctx,
            import_env,
            &formals,
            args.iter().skip(1).copied(),
        );
        // TODO Optmization opportunity: if the source code for a lambda is the same, we
        // don't actually have to recompile the instructions, we would just be in a
        // different import env (and have change labels to match our labels)
        // dbg!((arg_list, &chunk));
        let index = ctx.add_lambda(Gc::new(
            ctx,
            lambda::CompiledLambda::new(formals.arity(), chunk?),
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
            "*",
            "=",
            "<",
            ">",
            "<=",
            ">=",
            "eq?",
            "eqv?",
            "car",
            "cdr",
            "caar",
            "cadr",
            "cdar",
            "cddr",
            "pair?",
            "null?",
            "cons",
            "values",
            "apply",
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
            "*" => lambda!(Mul),
            "=" => lambda!(Equal),
            "<" => lambda!(MonotonicAscending),
            "<=" => lambda!(Ascending),
            ">" => lambda!(MonotonicDescending),
            ">=" => lambda!(Descending),
            "eq?" => lambda!(IsEq),
            "eqv?" => lambda!(IsEqv),
            "car" => lambda!(Car),
            "cdr" => lambda!(Cdr),
            "caar" => lambda!(Caar),
            "cadr" => lambda!(Cadr),
            "cdar" => lambda!(Cdar),
            "cddr" => lambda!(Cddr),
            "pair?" => lambda!(IsPair),
            "null?" => lambda!(IsNull),
            "cons" => lambda!(Cons),
            "values" => lambda!(Values),
            "apply" => lambda!(Apply),
            _ => None,
        }
    }

    fn syntax(&self, interner: &mut lasso::Rodeo, symbol: lasso::Spur) -> Option<ArcSyntax> {
        match interner.resolve(&symbol) {
            "define" => Some(Arc::new(Define)),
            "lambda" => Some(Arc::new(Lambda)),
            "set!" => Some(Arc::new(SetBang)),
            "if" => Some(Arc::new(If)),
            "quote" => Some(Arc::new(Quote)),
            _ => None,
        }
    }
}
