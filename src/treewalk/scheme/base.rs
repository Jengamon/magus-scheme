use crate::declare_lambdas;
use crate::environment::EnvironmentPtr;
use crate::treewalk::Context;
use crate::{
    runtime::lambda::{
        Lambda, LambdaCall, ProcedureError, ProcedureResult, ProcedureReturn, Typecheck,
    },
    treewalk::TreewalkExecutor,
    value::ValueType,
    Fuel,
};

pub mod macros {
    use std::collections::{HashSet, VecDeque};

    use gc_arena::{Collect, Gc, RefLock, Rootable};

    use crate::{
        environment::Environment,
        runtime::{
            lambda::{
                Lambda as LambdaProc, LambdaCall, Procedure, ProcedureResult, ProcedureReturn,
            },
            userstruct::UserStruct,
            FuelCosts,
        },
        transformer::{Macro, MacroInstruction, MacroReturn},
        treewalk::{
            virtual_inst::{VirtualInstruction, VirtualInstructionDatum},
            Context, StackValue, TreewalkExecutor,
        },
        value::{ConsCell, Symbol, Value},
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
            _ctx: &Context<'gc>,
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
                inst: vec![
                    MacroInstruction::Evaluate(value),
                    MacroInstruction::SetBang { name },
                ],
            })
        }
    }

    /// Runtime value for lambdas, used by `lambda` and friends
    /// See [`Lambda`] for the simplest wrapper around this.
    #[derive(Debug, Collect)]
    #[collect(no_drop)]
    struct RuntimeLambda<'gc> {
        args: StackValue<'gc>,
        ops: Vec<StackValue<'gc>>,
    }

    impl<'gc> RuntimeLambda<'gc> {
        fn is_valid(&self) -> bool {
            // these are the invariants a runtime lambda expects of args
            matches!(*self.args.borrow(), Value::Symbol(_))
                || matches!(
                    *self.args.borrow(),
                    Value::Cons(c) if c.is_param_list(*self.args)
                )
        }

        fn get_arities(&self) -> (Option<usize>, Option<usize>) {
            assert!(self.is_valid());

            // check the stack for a set number of params
            // if args is a cons list
            // we can manually count the cons, b/c is_param_list
            // makes sure the list is not circular
            if let Value::Cons(mut cell) = *self.args.borrow() {
                let mut count = 0;
                let mut only_min = false;
                while cell.car.is_some() {
                    count += 1;
                    match cell.cdr.map(|v| *v.borrow()) {
                        Some(Value::Cons(c)) => {
                            cell = c;
                        }
                        Some(s) => {
                            assert!(matches!(s, Value::Symbol(_)));
                            only_min = true;
                        }
                        None => {}
                    }
                }
                (Some(count), if only_min { None } else { Some(count) })
            } else {
                // we should only be a symbol here (asserts go brrrr)
                (None, None)
            }
        }

        /// Extract named symbols, plus an optional rest parameter
        fn get_symbols(&self) -> (Vec<Symbol>, Option<Symbol>) {
            assert!(self.is_valid());

            match *self.args.borrow() {
                Value::Cons(mut cell) => {
                    let mut syms = Vec::new();
                    let mut rest = None;
                    while let Some(car) = cell.car {
                        if let Value::Symbol(s) = *car.borrow() {
                            syms.push(s);
                        } else {
                            unreachable!()
                        }

                        match cell.cdr.map(|v| *v.borrow()) {
                            Some(Value::Cons(c)) => {
                                cell = c;
                            }
                            Some(Value::Symbol(s)) => {
                                rest = Some(s);
                            }
                            Some(_) => unreachable!(),
                            None => {}
                        }
                    }
                    (syms, rest)
                }
                Value::Symbol(s) => (Vec::new(), Some(s)),
                _ => unreachable!(),
            }
        }
    }

    impl<'gc> Procedure<'gc> for RuntimeLambda<'gc> {
        fn typecheck(&self, call: &LambdaCall<'gc>) -> anyhow::Result<()> {
            let (min_count, arg_count) = self.get_arities();

            if arg_count.is_some_and(|min| call.args() != min) {
                Err(anyhow::anyhow!(
                    "mismatched arity (arity {}, args passed {})",
                    arg_count.unwrap(),
                    call.args()
                ))
            } else if min_count.is_some_and(|min| call.args() < min) {
                Err(anyhow::anyhow!(
                    "too few arguments (arity {}, args passed {})",
                    arg_count.unwrap(),
                    call.args()
                ))
            } else {
                Ok(())
            }
        }

        fn execute(
            &mut self,
            ctx: &Context<'gc>,
            call: &mut LambdaCall<'gc>,
            interpreter: &mut TreewalkExecutor<'gc>,
            _fuel: &mut Fuel,
        ) -> ProcedureResult<'gc> {
            eprintln!(
                ">> ARGSU {}",
                self.args
                    .borrow()
                    .resolve_into(ctx.interner.clone(), ctx.null_ptr)
            );
            // we store the body members we've yet to execute
            #[derive(Collect)]
            #[collect(no_drop)]
            struct LambdaState<'gc> {
                unexecuted: VecDeque<StackValue<'gc>>,
            }

            if let Some(Value::UserStruct(lambda_state)) = call.data().map(|vp| *vp.borrow()) {
                let lambda_state = lambda_state
                    .downcast_write::<Rootable![RefLock<LambdaState<'_>>]>(ctx.mutation)
                    .unwrap();
                let (next, is_tail) = {
                    let mut lambda_state = lambda_state.unlock().borrow_mut();
                    let next = lambda_state.unexecuted.pop_front();
                    (next, lambda_state.unexecuted.is_empty())
                };

                if let Some(code) = next {
                    return Ok(ProcedureReturn::Call { code, is_tail });
                } else {
                    unreachable!("tail call optimized out")
                }
            }

            // we know that args is either a symbol or a param list,
            // so use those assumptions to bind our variables!
            let mut new_env = Environment::new(ctx.mutation, Some(interpreter.current_env()));
            let (named, rest_name) = self.get_symbols();
            let (named_args, rest) = call.stack.split_at(named.len());
            for (name, arg) in named.into_iter().zip(named_args) {
                new_env.define(ctx.mutation, name, *arg, false).unwrap();
            }

            if let Some(rest_sym) = rest_name {
                let rest_cons =
                    ConsCell::from_iter(ctx.mutation, ctx.null_ptr, rest.iter().map(|sv| **sv));
                let rest_sv = interpreter.current_scope_value_ptr(ctx.mutation, rest_cons);
                new_env
                    .define(ctx.mutation, rest_sym, rest_sv, false)
                    .unwrap();
            }

            // Set the execution environment to this new environment
            interpreter.scope_mut().environment = Gc::new(ctx.mutation, RefLock::new(new_env));

            let body_call = self.ops.first().unwrap();
            let rest: VecDeque<_> = self.ops.iter().skip(1).copied().collect();
            let is_tail = rest.is_empty();
            let ls = LambdaState { unexecuted: rest };
            *call.data_mut() = Some(
                Value::UserStruct(UserStruct::new::<Rootable![RefLock<LambdaState<'_>>]>(
                    ctx.mutation,
                    RefLock::new(ls),
                ))
                .into_ptr(ctx.mutation),
            );

            Ok(ProcedureReturn::Call {
                code: *body_call,
                is_tail,
            })
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Lambda;

    impl<'gc> Macro<'gc> for Lambda {
        fn is_properly_formed(
            &self,
            args: &[VirtualInstructionDatum<'gc>],
        ) -> Result<(), anyhow::Error> {
            if args.len() >= 2 {
                (match &args[0] {
                    VirtualInstructionDatum::Symbol(_) => true,
                    VirtualInstructionDatum::EmptyList => true,
                    VirtualInstructionDatum::List { head, body, dot } => {
                        // if a list, *every* member must be a *symbol*
                        fn is_valid<'a, 'gc>(
                            vii: impl Iterator<Item = &'a VirtualInstruction<'gc>>,
                        ) -> bool
                        where
                            'gc: 'a,
                        {
                            let mut seen = HashSet::new();
                            for vi in vii {
                                match vi.payload.datum() {
                                    VirtualInstructionDatum::Symbol(s) => {
                                        if seen.contains(s) {
                                            return false;
                                        }
                                        seen.insert(*s);
                                    }
                                    _ => return false,
                                }
                            }
                            true
                        }

                        is_valid(
                            std::iter::once(head.as_ref())
                                .chain(body.iter())
                                .chain(dot.as_ref().map(|d| d.as_ref())),
                        )
                    }
                    _ => false,
                })
                .then_some(())
                // a "param list" is a cons list (dotted or normal) whose car
                // is only symbols and cdr is either a cons or symbol
                .ok_or(anyhow::anyhow!(
                    "first argument to lambda must be a symbol or param list"
                ))
            } else {
                Err(anyhow::anyhow!("lambda must have 2 or more arguments"))
            }
        }

        fn rewrite(
            &mut self,
            ctx: &Context<'gc>,
            executor: &mut TreewalkExecutor<'gc>,
            fuel: &mut Fuel,
        ) -> anyhow::Result<MacroReturn<'gc>> {
            // FIXME figure out testing situation before moving this to core
            // because doing this every time is nasty
            //
            // (basically gets the difference in size of the full stack compared to
            // the stuff that is in this scope's stack to get the number of arguments
            // that were pushed to the stack for this macro's execution)
            let split_point = executor.stack.len().saturating_sub(executor.stack().len());
            let args = executor.stack.split_off(split_point);
            fuel.consume(FuelCosts::NEW_LAMBDA);
            let (args, ops) = args.split_at(1);
            executor.stack.push(
                executor.current_scope_value_ptr(
                    ctx.mutation,
                    Value::Lambda(Gc::new(
                        ctx.mutation,
                        RefLock::new(LambdaProc::with_procedure(
                            ctx.mutation,
                            RuntimeLambda {
                                args: args[0],
                                ops: ops.to_vec(),
                            },
                        )),
                    ))
                    .into_ptr(ctx.mutation),
                ),
            );
            Ok(MacroReturn::Return { inst: Vec::new() })
            // todo!(
            //     "{}",
            //     args.into_iter()
            //         .map(|v| v
            //             .borrow()
            //             .resolve_into(ctx.interner.clone(), ctx.null_ptr)
            //             .to_string())
            //         .collect::<Vec<_>>()
            //         .join(" ")
            // );
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
    ctx: &Context<'gc>,
    call: &mut LambdaCall<'gc>,
    interpreter: &mut TreewalkExecutor<'gc>,
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
    Ok(ProcedureReturn::Return(
        interpreter.current_scope_value(ctx.mutation, total),
    ))
}

fn div_impl<'gc>(
    _root: &mut (),
    ctx: &Context<'gc>,
    call: &mut LambdaCall<'gc>,
    interpreter: &mut TreewalkExecutor<'gc>,
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
            Ok(val) => Ok(ProcedureReturn::Return(
                interpreter.current_scope_value(ctx.mutation, val),
            )),
            Err(e) => Err(ProcedureError::General(e)),
        }
    } else {
        Ok(ProcedureReturn::Return(
            interpreter.current_scope_value(ctx.mutation, (init as f64).recip()),
        ))
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

impl<'gc> SchemeBase<'gc> {
    /// Import all names defined in this module into the given environment
    pub fn import_all(&self, mc: &Mutation<'gc>, env: EnvironmentPtr<'gc>) {
        let _ = mc;
        let _ = env;
        todo!()
    }
}
