use crate::declare_lambdas;
use crate::{
    runtime::lambda::{
        Lambda, LambdaCall, ProcedureError, ProcedureResult, ProcedureReturn, Typecheck,
    },
    treewalk::TreewalkExecutor,
    value::ValueType,
    Fuel,
};

pub mod macros {
    use gc_arena::Collect;

    use crate::{
        runtime::FuelCosts,
        transformer::{Macro, MacroInstruction, MacroReturn},
        treewalk::{Context, TreewalkExecutor},
        value::Value,
        Fuel,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Define;

    impl<'gc> Macro<'gc> for Define {
        fn rewrite(
            &mut self,
            ctx: &Context<'gc>,
            executor: &mut TreewalkExecutor<'gc>,
            fuel: &mut Fuel,
        ) -> anyhow::Result<MacroReturn<'gc>> {
            // RN only support <name> <val> input
            let Some(value) = executor.stack.pop() else {
                return Err(anyhow::anyhow!("no define value"));
            };
            let Some(name) = executor
                .stack
                .pop()
                .and_then(|v| v.borrow().as_symbol())
                .map(|sym| sym.0)
            else {
                return Err(anyhow::anyhow!("no define name"));
            };

            // this macro "returns" void
            fuel.consume(FuelCosts::ENV_SET_COST);

            Ok(MacroReturn::Return {
                ret: Value::Void.into_ptr(ctx.mutation),
                inst: vec![
                    MacroInstruction::Evaluate(value),
                    MacroInstruction::Define { name },
                ],
            })
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct SetBang;

    impl<'gc> Macro<'gc> for SetBang {
        fn rewrite(
            &mut self,
            ctx: &Context<'gc>,
            executor: &mut TreewalkExecutor<'gc>,
            fuel: &mut Fuel,
        ) -> anyhow::Result<MacroReturn<'gc>> {
            // only support <name> <val> input
            let Some(value) = executor.stack.pop() else {
                return Err(anyhow::anyhow!("no set! value"));
            };
            let Some(name) = executor
                .stack
                .pop()
                .and_then(|v| v.borrow().as_symbol())
                .map(|sym| sym.0)
            else {
                return Err(anyhow::anyhow!("no set! name"));
            };

            // this macro "returns" void
            fuel.consume(FuelCosts::ENV_SET_COST);

            Ok(MacroReturn::Return {
                ret: Value::Void.into_ptr(ctx.mutation),
                inst: vec![
                    MacroInstruction::Evaluate(value),
                    MacroInstruction::SetBang { name },
                ],
            })
        }
    }
}

fn all_numbers_typecheck(op: &'static str) -> Typecheck {
    Typecheck::new(move |sig| {
        if sig.iter().all(|ty| ty == &ValueType::Number) && !sig.is_empty() {
            Ok(())
        } else {
            Err(if sig.is_empty() {
                anyhow::anyhow!("cannot {op} nothing")
            } else {
                anyhow::anyhow!("cant {op} on {sig:?}")
            })
        }
    })
}

fn add_impl<'gc>(
    _root: &mut (),
    mc: &Mutation<'gc>,
    call: &mut LambdaCall<'gc>,
    _interpreter: &mut TreewalkExecutor<'gc>,
    _fuel: &mut Fuel,
) -> Result<ProcedureReturn<'gc>, ProcedureError<'gc>> {
    // TODO support inexacts too!
    let mut total: i64 = 0;
    while !call.stack.is_empty() {
        let Some(v) = call.pop::<i64>().ok() else {
            unreachable!("typechecking");
        };
        total = total
            .checked_add(v)
            .ok_or(anyhow::anyhow!("cannot add {v} to {total}"))?;
    }
    call.push(mc, total);
    Ok(ProcedureReturn::Suspend)
}

fn div_impl<'gc>(
    _root: &mut (),
    mc: &Mutation<'gc>,
    call: &mut LambdaCall<'gc>,
    _interpreter: &mut TreewalkExecutor<'gc>,
    _fuel: &mut Fuel,
) -> ProcedureResult<'gc> {
    // TODO support inexacts too!
    let mut data = vec![];
    while !call.stack.is_empty() {
        let Some(v) = call.pop::<i64>().ok() else {
            unreachable!("typechecking");
        };
        data.push(v);
    }
    let init = data.pop().unwrap();
    data.reverse();
    if !data.is_empty() {
        let res = data.into_iter().try_fold(init, |acc, it| {
            acc.checked_div(it).ok_or(if it == 0 {
                anyhow::anyhow!("divide by zero")
            } else {
                anyhow::anyhow!("can't divide {acc} by {it}")
            })
        });
        match res {
            Ok(val) => {
                call.push(mc, val);
                Ok(ProcedureReturn::Return)
            }
            Err(e) => Err(ProcedureError::General(e)),
        }
    } else {
        call.push(mc, (init as f64).recip());
        Ok(ProcedureReturn::Return)
    }
}

declare_lambdas!(
    SchemeBase => {
        div_op as op_div => |mc| {
            Lambda::with_typecheck(
                mc,
                all_numbers_typecheck("divide"),
                div_impl,
            )
        },
        add_op as op_add => |mc| {
            Lambda::with_typecheck(
                mc,
                all_numbers_typecheck("add"),
                add_impl,
            )
        }
    }
);
