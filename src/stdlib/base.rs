//! Implementations for (scheme base)

use std::{collections::HashSet, sync::Arc};

use gc_arena::{Gc, RefLock, unsize};

use crate::{
    CompilerHandle, Interpreter, LibraryName, World,
    bytecode::{Bytecode, Chunk, ChunkPtr},
    compiler::{
        ArcSyntax, Compiler, ExternalCompilerContext, LibraryDeclaration, LibraryDefinitionContext,
        Module, ParseProgram, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn,
    },
    environment::StackEnvironmentPtr,
    interpreter::NullIncluder,
    library_name,
    runtime::{convert::IntoValue, lambda},
};

pub use boolean::{And, Or};
pub use conditionals::If;
pub use define::{Define, SetBang};
pub use macros::{DefineSyntax, SyntaxRules};
pub use procedures::{
    Add, Apply, Ascending, Caar, Cadr, CallCc, Car, Cdar, Cddr, Cdr, Cons, Descending, Divide,
    Equal, Exact, Features, Gcd, Inexact, IsEq, IsEqv, IsExact, IsInexact, IsNull, IsPair,
    IsString, IsSymbol, MonotonicAscending, MonotonicDescending, Multiply, StringToNumber,
    StringToSymbol, Subtract, SymbolToString, Values,
};
pub use quote::{Quasiquote, Quote};

use super::Formals;

mod boolean;
mod conditionals;
mod define;
mod macros;
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
pub struct Begin;

// Because begin "forces" an order to it's subexpressions (first-to-last)
// definitions are allowed in any order
//
// We treat (begin <expr or def>...) and (begin <expr1> <expr2>...) the same
impl Syntax for Begin {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if args.is_empty() {
            return Ok(SyntaxReturn::Code(Box::from([Bytecode::PushVoid])));
        }

        let mut program_code = Vec::new();
        for program in args {
            program_code.extend(compiler.compile_code(ctx, *program)?.into_bytecode());
        }

        Ok(SyntaxReturn::Code(program_code.into_boxed_slice()))
    }

    fn is_container(&self, _ptr: ProgramPtr<'_>, _compiler: &Compiler<'_>) -> bool {
        true
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
#[derive(Default, Clone)]
pub struct Base {
    /// Any additional features (features) should provide and runtime cond-expand should expand
    // TODO Add a slot for where compile-time cond-expand should place additional features (probably in
    // LibraryDeclarationContext)
    pub additional_features: Arc<[Arc<str>]>,
}

impl Module for Base {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> HashSet<lasso::Spur> {
        [
            "call-with-current-continuation",
            "call/cc",
            "begin",
            "define",
            "lambda",
            "quote",
            "quasiquote",
            "set!",
            "if",
            "+",
            "-",
            "*",
            "/",
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
            "exact",
            "inexact",
            "features",
            "gcd",
            "exact?",
            "inexact?",
            "define-syntax",
            "syntax-rules",
            "and",
            "or",
            "string->number",
            "string->symbol",
            "symbol->string",
            "symbol?",
            "string?",
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
            "*" => lambda!(Multiply),
            "/" => lambda!(Divide),
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
            "exact" => lambda!(Exact),
            "inexact" => lambda!(Inexact),
            "features" => lambda!(Features::from_iter(
                self.additional_features.iter().cloned()
            )),
            "gcd" => lambda!(Gcd),
            "exact?" => lambda!(IsExact),
            "inexact?" => lambda!(IsInexact),
            "string->number" => lambda!(StringToNumber),
            "string->symbol" => lambda!(StringToSymbol),
            "symbol->string" => lambda!(SymbolToString),
            "symbol?" => lambda!(IsSymbol),
            "string?" => lambda!(IsString),
            _ => None,
        }
    }

    fn syntax(&self, interner: &mut lasso::Rodeo, symbol: lasso::Spur) -> Option<ArcSyntax> {
        match interner.resolve(&symbol) {
            "begin" => Some(Arc::new(Begin)),
            "define" => Some(Arc::new(Define)),
            "lambda" => Some(Arc::new(Lambda)),
            "set!" => Some(Arc::new(SetBang)),
            "if" => Some(Arc::new(If)),
            "quote" => Some(Arc::new(Quote)),
            "quasiquote" => Some(Arc::new(Quasiquote)),
            "define-syntax" => Some(Arc::new(DefineSyntax)),
            "syntax-rules" => Some(Arc::new(SyntaxRules)),
            "and" => Some(Arc::new(And)),
            "or" => Some(Arc::new(Or)),
            _ => None,
        }
    }
}

const MODULE_SRC: &str = include_str!("scheme_base.scm");

/// Registers this module (and it's Scheme implementations) under the name `(scheme base)`
pub fn register_module(
    interpreter: &mut Interpreter,
    handle: &CompilerHandle,
    world: &mut World,
    max_fuel: Option<i32>,
    additional_features: impl IntoIterator<Item = impl AsRef<str>>,
) -> anyhow::Result<()> {
    let additional_features = additional_features
        .into_iter()
        .map(|s| Arc::from(s.as_ref()))
        .collect::<Arc<[_]>>();
    let name = LibraryName::from_iter(library_name!(interpreter.interner_mut() => scheme base));
    let base_module = Base {
        additional_features: additional_features.clone(),
    };
    // Insert our module into the given world.
    // TODO Allow for additional features to be passed in
    world.insert(name.clone(), base_module.clone())?;
    // This is the world used to compile the Scheme implementation of things.
    let world = {
        let mut world = World::default();
        world.insert(name.clone(), base_module)?;
        world
    };
    interpreter.try_enter(|mc, arena, interner| {
        let programs = ("scheme_base.scm", MODULE_SRC).parse_program(mc, interner, false)?;
        let library_decls = programs
            .into_iter()
            .map(|p| LibraryDeclaration::convert(p, mc, interner))
            .collect::<Result<Vec<_>, _>>()?;
        let value_pointers = arena.value_pointers();
        let compiler = arena
            .compiler_mut(handle)
            .ok_or(anyhow::anyhow!("invalid compiler handle"))?;
        let mut ecc = ExternalCompilerContext {
            world: &world,
            includer: &NullIncluder,
            interner,
        };
        let library_def = LibraryDefinitionContext {
            max_fuel,
            value_pointers,
            additional_features: Some(&additional_features),
        };
        compiler.define_library(mc, &name, &mut ecc, false, &library_def, library_decls)?;
        Ok::<_, anyhow::Error>(())
    })?;

    Ok(())
}
