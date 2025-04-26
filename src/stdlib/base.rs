//! Implementations for (scheme base)

use std::{collections::HashSet, sync::Arc};

use gc_arena::{Gc, RefLock, unsize};

use crate::{
    LibraryName,
    bytecode::{Bytecode, Chunk, ChunkPtr},
    compiler::{
        ArcSyntax, Compiler, FeatureRequirement, Module, ProgramData, ProgramPtr, Syntax,
        SyntaxContext, SyntaxReturn,
    },
    environment::StackEnvironmentPtr,
    interpreter::Registerable,
    library_name,
    runtime::{convert::IntoValue, lambda},
};

pub use boolean::{And, Or};
pub use conditionals::If;
pub use define::{Define, SetBang};
pub use exception::WithExceptionHandler;
pub use macros::{DefineSyntax, SyntaxRules};
pub use procedures::{
    Add, Apply, Ascending, Caar, Cadr, CallCc, CallWithValues, Car, Cdar, Cddr, Cdr, CharToInteger,
    Cons, Denominator, Descending, Divide, Equal, Exact, ExactIntegerSqrt, Expt, Features, Gcd,
    Inexact, IntegerToChar, IsEq, IsEqual, IsEqv, IsEven, IsExact, IsExactInteger, IsInexact,
    IsInteger, IsList, IsNull, IsOdd, IsPair, IsProcedure, IsString, IsSymbol, IsVector, Lcm,
    ListCopy, ListSetBang, ListToString, Map, MonotonicAscending, MonotonicDescending, Multiply,
    NumberToString, Numerator, Raise, RaiseContinuable, StringToList, StringToNumber,
    StringToSymbol, Subtract, SymbolToString, Values, VectorRef,
};
pub use quote::{Quasiquote, Quote};

use super::Formals;

mod boolean;
mod conditionals;
mod define;
mod exception;
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
        compiler.define_parameters(
            ctx.interner,
            formals.non_rest_params(),
            formals.rest_param(),
        )?;

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

    fn is_container<'gc>(
        &self,
        ptr: ProgramPtr<'gc>,
        _compiler: &Compiler<'_>,
    ) -> Vec<ProgramPtr<'gc>> {
        // Only a list can trigger this proper (if not a list, return the empty list, which means "don't consider this a container")
        let ProgramData::List { body, .. } = &ptr.data else {
            return Vec::new();
        };

        body.clone()
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

#[derive(Debug)]
pub struct CondExpand {
    additional_features: Arc<[Arc<str>]>,
}

impl CondExpand {
    #[expect(clippy::type_complexity)]
    fn parse<'gc>(
        ctx: &mut SyntaxContext<'_, 'gc>,
        args: &[ProgramPtr<'gc>],
        else_sym: lasso::Spur,
    ) -> anyhow::Result<(
        Vec<(FeatureRequirement, Vec<ProgramPtr<'gc>>)>,
        Vec<ProgramPtr<'gc>>,
    )> {
        let mut branches = vec![];
        let mut else_branch = vec![];
        let mut last_non_else_index = None;
        for (idx, p) in args.iter().enumerate() {
            if let ProgramData::List { head, body } = &p.data {
                if let Some(s) = head.into_symbol(ctx.interner) {
                    if s == else_sym {
                        // This is the else decl, add to else stuff, then break (so that this *has* to be the last one)
                        let decls = body.clone();
                        else_branch.extend(decls);
                        break;
                    }
                }
                // The head this the requirement, the body the declarationss
                last_non_else_index = Some(idx);
                let head = FeatureRequirement::convert(
                    head.into_program(ctx.mc, ctx.interner, p.source),
                    ctx.interner,
                )?;
                let decls = body.clone();
                branches.push((head, decls));
            } else {
                // not a valid cond-expand decl
                anyhow::bail!("cond-expand is not well-formed");
            }
        }

        if (last_non_else_index.is_none() && !branches.is_empty())
            || last_non_else_index
                .is_some_and(|lb| ![args.len(), args.len().saturating_sub(1)].contains(&(lb + 1)))
        {
            // else branch is not the last branch
            anyhow::bail!("cond-expand else must be the last branch")
        } else {
            Ok((branches, else_branch))
        }
    }
}

impl Syntax for CondExpand {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // // A cond-expand consists of at least 1 clause of form (FeatureRequirement <programs>...)
        // // followed by up to one (else <programs>...)
        let else_sym = ctx.interner.get_or_intern_static("else");

        let (branches, else_branch) = Self::parse(ctx, args, else_sym)?;

        let features = Compiler::features(self.additional_features.as_ref(), ctx.interner);
        let mut branch_satisfied = false;
        let mut programs_to_execute = vec![];
        for (req, cond_programs) in branches.into_iter() {
            if req.is_satisfied(compiler, ctx.world, &features) {
                branch_satisfied = true;
                programs_to_execute = cond_programs;
                // Ignore the remaining clauses
                break;
            }
        }
        if !branch_satisfied && !else_branch.is_empty() {
            // expand else branch
            programs_to_execute = else_branch;
        }

        let mut code = vec![];
        for program in programs_to_execute {
            code.extend(compiler.compile_code(ctx, program)?.into_bytecode());
        }

        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }

    // trick: consider empty cond-expand as a definition
    fn is_definition<'gc>(&self, ptr: ProgramPtr<'gc>, _compiler: &Compiler<'gc>) -> bool {
        let mut programs_to_check = vec![];
        if let ProgramData::List { body, .. } = &ptr.data {
            for p in body.iter().copied() {
                if let ProgramData::List { body, .. } = &p.data {
                    // might be valid, so add the "body" elements to check list
                    programs_to_check.extend(body.iter().copied());
                }
            }
        }

        programs_to_check.is_empty()
    }

    fn is_container<'gc>(
        &self,
        ptr: ProgramPtr<'gc>,
        _compiler: &Compiler<'gc>,
    ) -> Vec<ProgramPtr<'gc>> {
        let mut programs_to_check = vec![];
        if let ProgramData::List { body, .. } = &ptr.data {
            for p in body.iter().copied() {
                if let ProgramData::List { body, .. } = &p.data {
                    // might be valid, so add the "body" elements to check list
                    programs_to_check.extend(body);
                }
            }
        }

        programs_to_check
    }
}

#[derive(Debug)]
pub struct Include;

#[derive(Debug)]
pub struct IncludeCi;

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
            "expt",
            "=",
            "<",
            ">",
            "<=",
            ">=",
            "eq?",
            "eqv?",
            "equal?",
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
            "lcm",
            "exact?",
            "inexact?",
            "define-syntax",
            "syntax-rules",
            "and",
            "or",
            "string->number",
            "number->string",
            "string->symbol",
            "symbol->string",
            "symbol?",
            "string?",
            "procedure?",
            "call-with-values",
            "numerator",
            "denominator",
            "even?",
            "odd?",
            "with-exception-handler",
            "cond-expand",
            "exact-integer-sqrt",
            "map",
            "list->string",
            "string->list",
            "list?",
            "char->integer",
            "integer->char",
            "raise",
            "raise-continuable",
            "integer?",
            "exact-integer?",
            "list-set!",
            "list-copy",
            "vector-ref",
            "vector?",
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
            "expt" => lambda!(Expt),
            "=" => lambda!(Equal),
            "<" => lambda!(MonotonicAscending),
            "<=" => lambda!(Ascending),
            ">" => lambda!(MonotonicDescending),
            ">=" => lambda!(Descending),
            "eq?" => lambda!(IsEq),
            "eqv?" => lambda!(IsEqv),
            "equal?" => lambda!(IsEqual),
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
            "features" => lambda!(Features::from(Arc::clone(&self.additional_features))),
            "gcd" => lambda!(Gcd),
            "lcm" => lambda!(Lcm),
            "exact?" => lambda!(IsExact),
            "inexact?" => lambda!(IsInexact),
            "string->number" => lambda!(StringToNumber),
            "string->symbol" => lambda!(StringToSymbol),
            "symbol->string" => lambda!(SymbolToString),
            "symbol?" => lambda!(IsSymbol),
            "string?" => lambda!(IsString),
            "procedure?" => lambda!(IsProcedure),
            "call-with-values" => lambda!(CallWithValues),
            "numerator" => lambda!(Numerator),
            "denominator" => lambda!(Denominator),
            "even?" => lambda!(IsEven),
            "odd?" => lambda!(IsOdd),
            "with-exception-handler" => lambda!(WithExceptionHandler),
            "exact-integer-sqrt" => lambda!(ExactIntegerSqrt),
            "map" => lambda!(Map::default()),
            "list->string" => lambda!(ListToString),
            "string->list" => lambda!(StringToList),
            "list?" => lambda!(IsList),
            "char->integer" => lambda!(CharToInteger),
            "integer->char" => lambda!(IntegerToChar),
            "number->string" => lambda!(NumberToString),
            "raise" => lambda!(Raise),
            "raise-continuable" => lambda!(RaiseContinuable),
            "integer?" => lambda!(IsInteger),
            "exact-integer?" => lambda!(IsExactInteger),
            "list-set!" => lambda!(ListSetBang),
            "list-copy" => lambda!(ListCopy),
            "vector?" => lambda!(IsVector),
            "vector-ref" => lambda!(VectorRef),
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
            "cond-expand" => Some(Arc::new(CondExpand {
                additional_features: Arc::clone(&self.additional_features),
            })),
            _ => None,
        }
    }
}

const MODULE_SRC: &str = include_str!("scheme_base.scm");

impl Registerable for Base {
    fn name(interner: &mut lasso::Rodeo) -> LibraryName {
        LibraryName::from_iter(library_name!(interner => scheme base))
    }

    fn native(&self) -> Option<Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
        Some(Arc::new(self.clone()))
    }

    fn scheme(&self) -> Option<(&str, &str)> {
        Some(("scheme_base.scm", MODULE_SRC))
    }

    fn scheme_native(
        &self,
        interner: &mut lasso::Rodeo,
    ) -> Vec<(
        LibraryName,
        Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
    )> {
        vec![
            (
                LibraryName::from_iter(library_name!(interner => scheme base)),
                Arc::new(self.clone()),
            ),
            (
                LibraryName::from_iter(library_name!(interner => magus impl)),
                Arc::new(super::magus_impl::MagusImpl),
            ),
        ]
    }
}
