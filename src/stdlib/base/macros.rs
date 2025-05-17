use std::rc::Rc;

use fxhash::FxHashMap;
use gc_arena::{Collect, Gc, Mutation, unsize};

use crate::{
    Syntax, SyntaxContext, SyntaxReturn,
    bytecode::{Bytecode, SourceData},
    compiler::{
        Checkpoint, Compiler, EnvironmentSpec, ListHead, ProgramData, ProgramPtr, Transformer,
    },
    environment::StackEnvironmentPtr,
    value::Number,
};

#[derive(Debug)]
pub struct LetSyntax;

// TODO we'll need a way to "reserve" a syntax slot, so that our syntaxes can use themselves
// in their own definition
#[derive(Debug)]
pub struct LetRecSyntax;

#[derive(Debug)]
pub struct SyntaxError;

#[derive(Debug)]
pub struct DefineSyntax;

impl Syntax for DefineSyntax {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // In the format (define-syntax <name> <transformer>)
        if args.len() != 2 {
            return Err(anyhow::anyhow!(
                "define-syntax must be given exactly 2 arguments"
            ));
        }

        let ProgramData::Symbol(name) = args[0].data else {
            return Err(anyhow::anyhow!(
                "define-syntax must be given a symbol as its first argument"
            ));
        };

        let Some(transformer) = compiler.compile_code(ctx, args[1])?.into_transformer() else {
            return Err(anyhow::anyhow!(
                "define-syntax must be given a transformer as its second argument"
            ));
        };

        let syntax = compiler.install_transformer(transformer);
        compiler.define_macro(name, syntax);

        Ok(SyntaxReturn::Code(Box::from([Bytecode::PushVoid])))
    }

    fn is_definition(&self, _ptr: ProgramPtr<'_>, _compiler: &Compiler<'_>) -> bool {
        true
    }
}

// A transformer implementing the matching of syntax rules from the declaration
#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct SyntaxRulesImpl {
    /// Checkpoint to interpret code in
    #[collect(require_static)]
    checkpoint: Checkpoint,
    /// Import env of definition environment
    #[collect(require_static)]
    source_env: Option<EnvironmentSpec>,

    #[collect(require_static)]
    ellipsis_symbol: lasso::Spur,
    literals: Vec<Literal>,
    // We use Box<[Matcher]> b/c the first element of a pattern (at the top-level) is ignored
    // (we do have requirements of it i.e. it *must* be a symbol, but otherwise, we don't actually need to know what it is)
    // because all executable Scheme code are proper lists (even (3 . 4) is a list of number 3, identifier ., then number 4)
    branches: Vec<(Box<[Matcher]>, Template)>,
}

/// These are literal datatypes as written in the input
#[derive(Debug, Collect)]
#[collect(require_static)]
enum Literal {
    Number(Number),
    Inexact(f64),
    String(lasso::Spur),
    Symbol(lasso::Spur),
    Bool(bool),
    Char(char),
    Bytevector(Rc<[u8]>),
    // Pretty sure this is a constant
    EmptyList,
}

/// Representation of a pattern literal
#[derive(Debug, Collect)]
#[collect(require_static)]
enum Matcher {
    // `_` is special, as it *can* show up multiple times in a pattern and *cannot* be matched (unlike variables)
    Underscore,
    // we use the spur to identify variables
    Variable(lasso::Spur),
    // we just have a flat list of literals, so these are indicies
    // literals are any constant data (a literal identifier is a constant)
    Literal(usize),
    // (<pattern> ...)
    List(Vec<Matcher>),
    // (<pattern> ... <pattern> <ellipsis> <pattern> ...)
    EllipsisList {
        pre_ellipsis: Vec<Matcher>,
        ellipsis: Box<Matcher>,
        post_ellipsis: Vec<Matcher>,
    },
    // (<pattern> <pattern> ... . <pattern>)
    DottedList {
        pre_dot: Vec<Matcher>,
        dot: Box<Matcher>,
    },
    // (<pattern> ... <pattern> <ellipsis> <pattern> ... . <pattern>)
    EllipsisDottedList {
        pre_ellipsis: Vec<Matcher>,
        ellipsis: Box<Matcher>,
        post_ellipsis: Vec<Matcher>,
        dot: Box<Matcher>,
    },
    // #(<pattern> ...)
    Vector(Vec<Matcher>),
    // #(<pattern> ... <pattern> <ellipsis> <pattern> ...)
    EllipsisVector {
        pre_ellipsis: Vec<Matcher>,
        ellipsis: Box<Matcher>,
        post_ellipsis: Vec<Matcher>,
    },
}

impl Matcher {
    /// `None` means that this matcher does *not* match the given program
    fn bind<'gc>(
        &self,
        ptr: ProgramPtr<'gc>,
        literals: &[Literal],
    ) -> Option<FxHashMap<lasso::Spur, BoundItem<'gc>>> {
        // We are trying to match on a single element
        match self {
            Matcher::Literal(idx) => match &literals[*idx] {
                Literal::Number(n) => match &ptr.data {
                    ProgramData::Number(pn) if n == pn => Some(FxHashMap::default()),
                    _ => None,
                },
                Literal::String(s) => match &ptr.data {
                    ProgramData::String(ps) if s == ps => Some(FxHashMap::default()),
                    _ => None,
                },
                _ => todo!(),
            },
            // Throw away the program it *would* match
            Matcher::Underscore => Some(FxHashMap::default()),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Collect)]
#[collect(require_static)]
struct Element {
    template: Box<Template>,
    is_ellipsized: bool,
}

#[derive(Debug, Collect)]
#[collect(require_static)]
enum Template {
    Variable(lasso::Spur),
    Literal(usize),
    List(Vec<Element>),
    DottedList {
        pre_dot: Vec<Element>,
        dot: Box<Template>,
    },
    // (<ellipsis> <template>) this *disables* ellipsis expansion in the subtemplate
    EllipsisList(Box<Template>),
    Vector(Vec<Element>),
}

impl Template {
    /// Rewrite a given set of bindings using a template to produce a resulting program
    ///
    /// # Parameters
    /// - `mc`: [`gc_arena`] mutation context
    /// - `default_source`: the [`SourceData`] for program items generated by the template (not bindings)
    /// - `bindings`: container for bindings extracted from input code
    fn instantiate<'gc>(
        &self,
        mc: &Mutation<'gc>,
        default_source: Option<SourceData>,
        bindings: Bindings<'gc>,
    ) -> ProgramPtr<'gc> {
        todo!()
    }
}

enum BoundItem<'gc> {
    Item(ProgramPtr<'gc>),
    // inside ellipsis, this has to be drilled-down
    Ellipsis(Vec<BoundItem<'gc>>),
}

/// Structure that is produced when a [`ProgramPtr`] is successfully matched against a [`Matcher`], which
/// contains data that can be used to instantiate a [`Template`] into a [`ProgramPtr`] for further compilation
struct Bindings<'gc> {
    bindings: FxHashMap<lasso::Spur, BoundItem<'gc>>,
}

impl<'gc> Transformer<'gc> for SyntaxRulesImpl {
    fn evaluate(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        let code = compiler.hygenic_with_env(ctx, self.source_env, |ctx, compiler, import_env| {
            compiler.restore_checkpoint(self.checkpoint);
            let mut code = Vec::new();
            // Execute on our matching! (hygenic, load checkpoint/env in that hygenic env, so that the binding doesn't stay around
            // outside of templated code.)
            code.push(Bytecode::PushNull); // TODO
            code
        });
        Ok(SyntaxReturn::Code(code.into_boxed_slice()))
    }

    fn is_container(
        &self,
        _ptr: ProgramPtr<'gc>,
        _compiler: &Compiler<'gc>,
    ) -> Vec<ProgramPtr<'gc>> {
        // Select the body to be expanded by _ptr and return the elements that would be expanded (if they are all definition / containers
        // of definitions, then we are considered a container of definitions/definitions ourselves)
        Vec::new()
    }
}

#[derive(Debug)]
pub struct SyntaxRules;

impl Syntax for SyntaxRules {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // Parse the matcher and have the code ready
        let (ellipsis_symbol, literal_symbols, skip) = match args.first().map(|a| &a.data) {
            Some(ProgramData::Symbol(ellipsis)) if args.len() >= 2 => {
                let ellipsis = *ellipsis;
                let literal_symbols = match &args[1].data {
                    ProgramData::List { head, body }
                        if (matches!(head, ListHead::Import | ListHead::DefineLibrary)
                            || matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(_))))
                            && body
                                .iter()
                                .all(|bp| matches!(bp.data, ProgramData::Symbol(_))) =>
                    {
                        let head_symbol = match head {
                            ListHead::Import => ctx.ecc.interner.get_or_intern_static("import"),
                            ListHead::DefineLibrary => {
                                ctx.ecc.interner.get_or_intern_static("define-library")
                            }
                            ListHead::Program(p) => match &p.data {
                                ProgramData::Symbol(s) => *s,
                                _ => unreachable!(),
                            },
                        };
                        let body_symbols = body.iter().map(|bp| match &bp.data {
                            ProgramData::Symbol(s) => *s,
                            _ => unreachable!(),
                        });
                        std::iter::once(head_symbol)
                            .chain(body_symbols)
                            .collect::<Vec<_>>()
                    }
                    _ => anyhow::bail!(
                        "syntax-rules first list (after ellipsis) must be a symbol list"
                    ),
                };
                (ellipsis, literal_symbols, 2)
            }
            Some(ProgramData::List { head, body })
                if (matches!(head, ListHead::Import | ListHead::DefineLibrary)
                    || matches!(head, ListHead::Program(p) if matches!(p.data, ProgramData::Symbol(_))))
                    && body
                        .iter()
                        .all(|bp| matches!(bp.data, ProgramData::Symbol(_))) =>
            {
                let head_symbol = match head {
                    ListHead::Import => ctx.ecc.interner.get_or_intern_static("import"),
                    ListHead::DefineLibrary => {
                        ctx.ecc.interner.get_or_intern_static("define-library")
                    }
                    ListHead::Program(p) => match &p.data {
                        ProgramData::Symbol(s) => *s,
                        _ => unreachable!(),
                    },
                };
                let body_symbols = body.iter().map(|bp| match &bp.data {
                    ProgramData::Symbol(s) => *s,
                    _ => unreachable!(),
                });
                let literal_symbols = std::iter::once(head_symbol)
                    .chain(body_symbols)
                    .collect::<Vec<_>>();
                (
                    ctx.ecc.interner.get_or_intern_static("..."),
                    literal_symbols,
                    1,
                )
            }
            Some(ProgramData::EmptyList) => {
                (ctx.ecc.interner.get_or_intern_static("..."), Vec::new(), 1)
            }
            _ => anyhow::bail!(
                "syntax-rules must start with a symbol and a symbol list or a symbol list"
            ),
        };
        #[expect(unused_mut)]
        let mut literals = literal_symbols.into_iter().map(Literal::Symbol).collect();
        let branches = Vec::new();

        dbg!(&literals);

        let branch_code = args.iter().skip(skip);

        // Each branch is a proper list `List` of 2 items, the first being an proper list of matchers (preceded by an ignored symbol term)
        // and the second being a template (first item is a proper list b/c all valid Scheme code at the top level is a proper list)

        // TODO make branches, allocating literals as necessary for branches

        let checkpoint = compiler.checkpoint();
        let source_env = compiler.current_environment();

        let syntax_rules = SyntaxRulesImpl {
            checkpoint,
            source_env,
            ellipsis_symbol,
            literals,
            branches,
        };
        Ok(SyntaxReturn::Transformer(
            unsize!(Gc::new(ctx, syntax_rules) => dyn Transformer<'gc>),
        ))
    }
}
