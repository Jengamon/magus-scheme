use gc_arena::{Collect, Gc, unsize};

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
pub struct SyntaxRulesImpl {
    /// Checkpoint to interpret code in
    #[collect(require_static)]
    checkpoint: Checkpoint,
    /// Import env of definition environment
    #[collect(require_static)]
    source_env: Option<EnvironmentSpec>,
}

impl<'gc> Transformer<'gc> for SyntaxRulesImpl {
    fn evaluate(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // Execute on our matching!
        todo!()
    }

    fn is_definition(&self, _ptr: ProgramPtr<'gc>, _compiler: &Compiler<'gc>) -> bool {
        // Check the head of the matched code and if it is a definition, then we
        // are a container
        // No match is considered false
        false
    }

    fn is_container(&self, _ptr: ProgramPtr<'gc>, _compiler: &Compiler<'gc>) -> bool {
        // Check the head of the matched code and if it is a container, then we
        // are a container
        // No match is considered false
        false
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
        let _ = args;

        let checkpoint = compiler.checkpoint();
        let source_env = compiler.current_environment();

        let syntax_rules = SyntaxRulesImpl {
            checkpoint,
            source_env,
        };
        Ok(SyntaxReturn::Transformer(
            unsize!(Gc::new(ctx, syntax_rules ) => dyn Transformer<'gc>),
        ))
    }
}
