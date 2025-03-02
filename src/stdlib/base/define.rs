use crate::{
    bytecode::Bytecode,
    compiler::{Compiler, ProgramData, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
};

#[derive(Debug)]
pub struct Define {
    /// What symbol is this being defined under?
    pub(crate) self_sym: lasso::Spur,
}

impl Syntax for Define {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // FIXME Make a "base" error, and remove the ad-hocness of these impls (maybe? I like them ad-hoc rn)
        if args.len() != 2 {
            return Err(anyhow::anyhow!("define must be given exactly 2 arguments"));
        }

        // For now, only support (define <sym> <val>) form
        let Some(ProgramData::Symbol(name)) = args.first().map(|p| &p.data) else {
            return Err(anyhow::anyhow!(
                "define must be given a symbol as the first argument"
            ));
        };

        // Save peeps from themselves (im looking at you, me) and don't
        // allow overriding 1 symbol (generally the symbol that defines this macro)
        if *name == self.self_sym {
            return Err(anyhow::anyhow!("cannot define definition macro"));
        }

        let Some(value) = args.get(1).cloned() else {
            return Err(anyhow::anyhow!("define must be given 2 arguments"));
        };

        // Inform the compiler that a name is being defined in scope
        compiler.define_variable(*name);

        Ok(SyntaxReturn::Code(
            compiler
                .compile_code(ctx, value)?
                .into_bytecode()
                .into_iter()
                .chain([Bytecode::Define { symbol: *name }, Bytecode::PushVoid])
                .collect(),
        ))
    }
}

#[derive(Debug)]
pub struct SetBang;

impl Syntax for SetBang {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        let _ = (ctx, compiler, import_env, args);
        todo!()
    }
}
