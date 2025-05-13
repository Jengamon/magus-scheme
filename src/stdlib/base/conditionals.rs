use crate::{
    bytecode::Bytecode,
    compiler::{Compiler, ProgramPtr, Syntax, SyntaxContext, SyntaxReturn},
    environment::StackEnvironmentPtr,
};

#[derive(Debug)]
pub struct If;

impl Syntax for If {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        let (mut code, mut consequent_code, alternate_code) = if args.len() == 2 {
            // (test) (consequenent) form: on false, jump and push void
            let test = args[0];
            let consequent = args[1];
            // get code for the "test" expression
            let code = compiler
                .compile_code(ctx, test)?
                .into_bytecode()
                .into_iter()
                .collect::<Vec<_>>();
            let consequent_code = compiler
                .compile_code(ctx, consequent)?
                .into_bytecode()
                .into_iter()
                .collect::<Vec<_>>();
            let alternate_code = vec![Bytecode::PushVoid];

            (code, consequent_code, alternate_code)
        } else if args.len() == 3 {
            // (test) (consequenent) (alternate) form: on false, jump and run (alternate)
            let test = args[0];
            let consequent = args[1];
            let alternate = args[2];
            // get code for the "test" expression
            let code = compiler
                .compile_code(ctx, test)?
                .into_bytecode()
                .into_iter()
                .collect::<Vec<_>>();
            let consequent_code = compiler
                .compile_code(ctx, consequent)?
                .into_bytecode()
                .into_iter()
                .collect::<Vec<_>>();
            let alternate_code = compiler
                .compile_code(ctx, alternate)?
                .into_bytecode()
                .into_iter()
                .collect::<Vec<_>>();

            (code, consequent_code, alternate_code)
        } else {
            return Err(anyhow::anyhow!("if needs 2 or 3 arguments"));
        };

        // patch in jumps depending on these code chunk sizes
        consequent_code.push(Bytecode::Jump {
            jump: alternate_code.len(),
        });
        code.push(Bytecode::If {
            jump: consequent_code.len(),
        });

        Ok(SyntaxReturn::Code(
            code.into_iter()
                .chain(consequent_code)
                .chain(alternate_code)
                .collect(),
        ))
    }
}
