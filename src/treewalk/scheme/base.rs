use gc_arena::{Collect, Gc, Mutation, RefLock};

use crate::{
    runtime::lambda::{
        Lambda, LambdaCall, LambdaPtr, ProcedureError, ProcedureResult, ProcedureReturn, Typecheck,
    },
    treewalk::TreewalkExecutor,
    value::ValueType,
    Fuel,
};

pub mod macros {

    use gc_arena::Collect;

    use crate::{
        transformer::{Macro, MacroInstruction, MacroReturn},
        treewalk::{Context, StackValue, TreewalkExecutor},
        value::Value,
        Fuel,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Define;

    impl<'gc> Macro<'gc> for Define {
        fn rewrite(
            &mut self,
            _ctx: &Context<'gc>,
            executor: &mut TreewalkExecutor<'gc>,
            _fuel: &mut Fuel,
        ) -> anyhow::Result<MacroReturn<'gc>> {
            // RN only support <name> <val> input
            let Some(value) = executor.stack.pop() else {
                return Err(anyhow::anyhow!("no define value"));
            };
            let Some(name) = executor.stack.pop().and_then(|v| v.borrow().as_symbol()) else {
                return Err(anyhow::anyhow!("no define name"));
            };

            Ok(MacroReturn::Return(vec![
                MacroInstruction::Evaluate(value, None),
                MacroInstruction::Define { name: name.0 },
            ]))
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
            _fuel: &mut Fuel,
        ) -> anyhow::Result<MacroReturn<'gc>> {
            // only support <name> <val> input
            let Ok([name, value]): Result<[StackValue<'gc>; 2], _> = executor.stack().try_into()
            else {
                return Err(anyhow::anyhow!(
                    "set! doesn't support input [{}]",
                    executor
                        .stack()
                        .iter()
                        .map(|sv| format!("{}", sv.borrow().resolve_into(ctx.interner.clone())))
                        .collect::<Vec<_>>()
                        .join(" ")
                ));
            };

            // Get name as symbol
            let name = match *name.borrow() {
                Value::Symbol(sym) => sym.0,
                _ => return Err(anyhow::anyhow!("name must be a symbol")),
            };

            Ok(MacroReturn::Return(vec![
                MacroInstruction::Evaluate(value, None),
                MacroInstruction::SetBang { name },
            ]))
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

fn div_impl<'gc>(
    _root: &mut (),
    mc: &Mutation<'gc>,
    call: &mut LambdaCall<'gc>,
    _interpreter: &mut TreewalkExecutor<'gc>,
    _fuel: &mut Fuel,
) -> ProcedureResult<'gc> {
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

// TODO Make a macro out of these
#[derive(Collect, Clone, Copy)]
#[collect(no_drop)]
pub struct SchemeBase<'gc> {
    pub(crate) div_op: Gc<'gc, RefLock<Option<LambdaPtr<'gc>>>>,
}

impl<'gc> SchemeBase<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            div_op: Gc::new(mc, RefLock::new(None)),
        }
    }

    pub fn op_div(&self, mc: &Mutation<'gc>) -> LambdaPtr<'gc> {
        let mut div = self.div_op.borrow_mut(mc);
        if let Some(div) = *div {
            div
        } else {
            let ndiv = Gc::new(
                mc,
                RefLock::new(Lambda::with_typecheck(
                    mc,
                    all_numbers_typecheck("divide"),
                    div_impl,
                )),
            );
            *div = Some(ndiv);
            ndiv
        }
    }
}
