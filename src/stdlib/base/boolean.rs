use crate::{
    Syntax, SyntaxContext, SyntaxReturn,
    bytecode::Bytecode,
    compiler::{Compiler, ProgramPtr},
    environment::StackEnvironmentPtr,
};

/*
TODO match code to report impls so that tail calls work properly
(define-syntax and
    (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
            (if test1 (and test2 ...) #f))))
(define-syntax or
    (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
            (let ((x test1))
                (if x x (or test2 ...))))))

(instead of a complicated coda, just have v simple coda)
a simple and good test for transformers is to compare the manually written code here to
the code it generates
*/

#[derive(Debug)]
pub struct And;

impl Syntax for And {
    fn evaluate<'gc>(
        &self,
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
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
            // jump 1
            // 'fail: push false
            let mut code = vec![];
            // at each branch, the jump if -> 'fail target value = size of following branches + number of following branches
            // (as each branch is followed by a single jump if, except for the final branch, which is followed by a jump instead)
            let jump_targets = (0..args_compiled.len())
                .map(|idx| {
                    let following = &args_compiled[idx + 1..];
                    following.iter().map(|blk| blk.len()).sum::<usize>() + following.len()
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
        ctx: &mut SyntaxContext<'_, '_, 'gc>,
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
            // pop
            // test2
            // dup
            // jump if 1
            // jump 'success
            // pop
            // ...
            // test_final
            // jump 1
            // push #f
            // 'success
            let mut code = vec![];
            // at each branch, the jump 'success target value = size of following branches + number of following branches * 4 - 3
            // (as each branch is followed by a 4 instructions, except for the final branch, which is followed by 2, the first which success targets)
            let jump_targets = (0..args_compiled.len())
                .map(|idx| {
                    let following = &args_compiled[idx + 1..];
                    (following.iter().map(|blk| blk.len()).sum::<usize>() + following.len() * 4)
                        .saturating_sub(3)
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
                        Bytecode::Pop,
                    ]);
                }
            }

            // The final target gets the return handling coda
            code.extend([
                Bytecode::Jump { jump: 1 },
                Bytecode::PushBool { bool: false },
            ]);

            Ok(SyntaxReturn::Code(code.into_boxed_slice()))
        }
    }
}
