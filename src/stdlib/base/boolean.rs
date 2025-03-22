use crate::{
    Syntax, SyntaxContext, SyntaxReturn,
    bytecode::Bytecode,
    compiler::{Compiler, ProgramPtr},
    environment::StackEnvironmentPtr,
};

#[derive(Debug)]
pub struct And;

impl Syntax for And {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // compile each argument into blocks of code
        let args_compiled = args
            .iter()
            .map(|p| {
                compiler
                    .compile_code(ctx, *p)
                    .map(|ret| ret.into_bytecode().into_iter().collect::<Vec<_>>())
            })
            .collect::<Result<Vec<_>, _>>()?;

        if args_compiled.is_empty() {
            Ok(SyntaxReturn::Code(Box::from([Bytecode::PushBool {
                bool: true,
            }])))
        } else {
            // structure
            // test1
            // jump if -> 'fail
            // test2
            // jump if -> 'fail
            // ...
            // test_final
            // duplicate
            // jump if 2
            // jump 1
            // 'fail: push false
            let mut code = vec![];
            // at each branch, the jump if -> 'fail target value = size of following branches + number of following branches + 2
            // (as each branch is followed by a single jump if, except for the final branch, which is followed by a dup, jump if *and* a jump)
            let jump_targets = (0..args_compiled.len())
                .map(|idx| {
                    let following = &args_compiled[idx + 1..];
                    following.iter().map(|blk| blk.len()).sum::<usize>() + following.len() + 2
                })
                .collect::<Vec<_>>();
            let num_branches = args_compiled.len();

            for (idx, (block, target)) in args_compiled.into_iter().zip(jump_targets).enumerate() {
                code.extend(block);
                if idx != num_branches - 1 {
                    // If not the final branch, add jump if -> 'fail
                    code.push(Bytecode::If { jump: target });
                }
            }

            // The final target gets the return handling coda
            code.extend([
                Bytecode::Duplicate,
                Bytecode::If { jump: 1 },
                Bytecode::Jump { jump: 1 },
                Bytecode::PushBool { bool: false },
            ]);

            Ok(SyntaxReturn::Code(code.into_boxed_slice()))
        }
    }
}

#[derive(Debug)]
pub struct Or;

impl Syntax for Or {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, 'gc>,
        compiler: &mut Compiler<'gc>,
        _import_env: StackEnvironmentPtr<'gc>,
        args: &[ProgramPtr<'gc>],
    ) -> anyhow::Result<SyntaxReturn<'gc>> {
        // compile each argument into blocks of code
        let args_compiled = args
            .iter()
            .map(|p| {
                compiler
                    .compile_code(ctx, *p)
                    .map(|ret| ret.into_bytecode().into_iter().collect::<Vec<_>>())
            })
            .collect::<Result<Vec<_>, _>>()?;

        if args_compiled.is_empty() {
            Ok(SyntaxReturn::Code(Box::from([Bytecode::PushBool {
                bool: false,
            }])))
        } else {
            // structure
            // test1
            // dup
            // jump if 1
            // jump 'success
            // test2
            // dup
            // jump if 1
            // jump 'success
            // ...
            // test_final
            // dup
            // jump if 1
            // jump 1
            // push #f
            // 'success
            let mut code = vec![];
            // at each branch, the jump 'success target value = size of following branches + number of following branches * 3 + 1
            // (as each branch is followed by a 3 instructions, except for the final branch, which is followed by 4)
            let jump_targets = (0..args_compiled.len())
                .map(|idx| {
                    let following = &args_compiled[idx + 1..];
                    following.iter().map(|blk| blk.len()).sum::<usize>() + following.len() * 3 + 1
                })
                .collect::<Vec<_>>();
            let num_branches = args_compiled.len();

            for (idx, (block, target)) in args_compiled.into_iter().zip(jump_targets).enumerate() {
                code.extend(block);
                if idx != num_branches - 1 {
                    // If not the final branch, add jump 'success
                    code.extend([
                        Bytecode::Duplicate,
                        Bytecode::If { jump: 1 },
                        Bytecode::Jump { jump: target },
                    ]);
                }
            }

            // The final target gets the return handling coda
            code.extend([
                Bytecode::Duplicate,
                Bytecode::If { jump: 1 },
                Bytecode::Jump { jump: 1 },
                Bytecode::PushBool { bool: false },
            ]);

            Ok(SyntaxReturn::Code(code.into_boxed_slice()))
        }
    }
}
