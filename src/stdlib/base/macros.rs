use std::rc::Rc;

use gc_arena::{Collect, Gc, Mutation, RefLock, unsize};

use crate::{
    Syntax, SyntaxContext, SyntaxReturn,
    bytecode::Bytecode,
    compiler::{Checkpoint, Compiler, EnvironmentSpec, ProgramData, ProgramPtr, Transformer},
    environment::StackEnvironmentPtr,
};

#[derive(Debug)]
pub struct DefineSyntax;

impl Syntax for DefineSyntax {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
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
#[collect(no_drop)]
pub struct SyntaxRulesImpl<'gc> {
    /// Checkpoint to interpret code in
    #[collect(require_static)]
    checkpoint: Checkpoint,
    /// Import env of definition environment
    #[collect(require_static)]
    source_env: Option<EnvironmentSpec>,
    /// Recursion counter
    rec_counter: Gc<'gc, RefLock<usize>>,

    #[collect(require_static)]
    ellipsis_symbol: lasso::Spur,
    literals: Vec<Literal>,
    // We use Box<[Matcher]> b/c the first element of a pattern (at the top-level) is ignored
    // (we do have requirements of it i.e. it *must* be a symbol, but otherwise, we don't actually need to know what it is)
    // because all executable Scheme code are proper lists (even (3 . 4) is a list of number 3, identifier ., then number 4)
    branches: Vec<(Box<[Matcher]>, Template)>,
}

/// Maximum number of recursive calls before macro expansion fails
const MAX_RECURSION: usize = 200;

/// These are literal datatypes as written in the input
#[derive(Debug, Collect)]
#[collect(require_static)]
enum Literal {
    Integer(i64),
    // TODO Rational
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
    fn bind<'gc>(&self, ptr: ProgramPtr<'gc>) -> Option<Bindings<'gc>> {
        None
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
    fn instantiate<'gc>(&self, mc: &Mutation<'gc>, bindings: Bindings<'gc>) -> ProgramPtr<'gc> {
        todo!()
    }
}

/// Structure that is produced when a [`ProgramPtr`] is successfully matched against a [`Matcher`], which
/// contains data that can be used to instantiate a [`Template`] into a [`ProgramPtr`] for further compilation
struct Bindings<'gc> {
    binding: Gc<'gc, ()>,
}

impl<'gc> Transformer<'gc> for SyntaxRulesImpl<'gc> {
    fn evaluate(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        if *self.rec_counter.borrow() >= MAX_RECURSION {
            return Err(anyhow::anyhow!("max recursion reached in macro expansion"));
        }

        let mut code = Vec::new();
        *self.rec_counter.borrow_mut(ctx) += 1;
        // Execute on our matching! (hygenic, load checkpoint/env in that hygenic env, so that the binding doesn't stay around
        // outside of templated code.)
        code.push(Bytecode::PushNull); // TODO
        *self.rec_counter.borrow_mut(ctx) -= 1;
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
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // Parse the matcher and have the code ready
        let ellipsis_symbol = match args.first().map(|a| &a.data) {
            Some(ProgramData::Symbol(ellipsis)) if args.len() >= 2 => *ellipsis,
            Some(ProgramData::List { .. } | ProgramData::EmptyList) => {
                ctx.interner.get_or_intern_static("...")
            }
            _ => anyhow::bail!("syntax-rules must start with a symbol and a list or a list"),
        };

        let literals = Vec::new();
        let branches = Vec::new();

        let checkpoint = compiler.checkpoint();
        let source_env = compiler.current_environment();

        let syntax_rules = SyntaxRulesImpl {
            checkpoint,
            source_env,
            rec_counter: Gc::new(ctx, RefLock::new(0)),
            ellipsis_symbol,
            literals,
            branches,
        };
        Ok(SyntaxReturn::Transformer(
            unsize!(Gc::new(ctx, syntax_rules ) => dyn Transformer<'gc>),
        ))
    }
}
