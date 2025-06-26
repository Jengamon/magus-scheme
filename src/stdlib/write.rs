use crate::{
    LibraryName,
    compiler::Module,
    interpreter::Registerable,
    library_name,
    runtime::{convert::IntoValue, lambda},
};
use gc_arena::{Gc, unsize};

mod procedures {
    use gc_arena::Collect;

    use crate::{
        Value, ValuePtr,
        runtime::lambda::{
            Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext, NativeLambdaState,
        },
        value::{ModeDisplay, ModeWrite},
    };

    macro_rules! write_lam {
        ($lam:ty => $name:literal, $mode:ty) => {
            impl<'gc> NativeLambda<'gc> for $lam {
                fn name(&self) -> &str {
                    $name
                }

                fn arity(&self) -> Arity {
                    Arity::Bounded { min: 1, max: 2 }
                }

                fn run(
                    &self,
                    _state: &mut NativeLambdaState<'gc>,
                    ctx: NativeLambdaContext<'_, 'gc>,
                    args: &[ValuePtr<'gc>],
                ) -> Result<LambdaReturn<'gc>, LambdaError> {
                    use std::io::Write;

                    let port = if args.len() == 1 {
                        // we are *not* given a port to use, so first get the current port value
                        if ctx.stack.is_empty() {
                            return Ok(LambdaReturn::Parameter {
                                parameter: ctx.thread_ref.output_port(),
                            });
                        } else {
                            // port to use is at top of stack (index 0)
                            let Value::OutputPort(prt) = *ctx.stack.last().copied().unwrap().borrow()
                            else {
                                return Err(anyhow::anyhow!(
                                    "{} expects current output port parameter to be an output port",
                                    $name
                                ))?;
                            };

                            if prt.borrow().is_closed() {
                                return Err(anyhow::anyhow!(
                                    "{} expects current output port parameter to be an open output port",
                                    $name
                                ))?;
                            }

                            prt
                        }
                    } else {
                        let Value::OutputPort(prt) = *args[1].borrow() else {
                            return Err(anyhow::anyhow!(
                                "{} expects an output port as its second argument",
                                $name
                            ))?;
                        };

                        if prt.borrow().is_closed() {
                            return Err(anyhow::anyhow!(
                                "{} expects an open output port as its second argument",
                                $name
                            ))?;
                        }

                        prt
                    };

                    write!(
                        port.borrow_mut(&ctx),
                        "{}",
                        Value::resolve_into::<_, $mode>(
                            args[0],
                            ctx.interner.clone(),
                            ctx.thread_ctx.null_value
                        )
                    )
                    .map_err(|e| anyhow::anyhow!("{}: failed to write to port: {e}", $name))?;

                    Ok(LambdaReturn::Return(vec![]))
                }
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct DisplayLam;

    write_lam!(DisplayLam => "display", ModeDisplay);

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct WriteLam;

    write_lam!(WriteLam => "write", ModeWrite);

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Newline;

    impl<'gc> NativeLambda<'gc> for Newline {
        fn name(&self) -> &str {
            "newline"
        }

        fn arity(&self) -> Arity {
            Arity::Bounded { min: 0, max: 1 }
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            use std::io::Write;

            let port = if args.is_empty() {
                // we are *not* given a port to use, so first get the current port value
                if ctx.stack.is_empty() {
                    return Ok(LambdaReturn::Parameter {
                        parameter: ctx.thread_ref.output_port(),
                    });
                } else {
                    // port to use is at top of stack (index 0)
                    let Value::OutputPort(prt) = *ctx.stack.last().copied().unwrap().borrow()
                    else {
                        return Err(anyhow::anyhow!(
                            "newline expects current output port parameter to be an output port",
                        ))?;
                    };

                    if prt.borrow().is_closed() {
                        return Err(anyhow::anyhow!(
                            "newline expects current output port parameter to be an open output port",
                        ))?;
                    }

                    prt
                }
            } else {
                let Value::OutputPort(prt) = *args[0].borrow() else {
                    return Err(anyhow::anyhow!(
                        "newline expects an output port as its argument",
                    ))?;
                };

                if prt.borrow().is_closed() {
                    return Err(anyhow::anyhow!(
                        "newline expects an open output port as its argument",
                    ))?;
                }

                prt
            };

            writeln!(port.borrow_mut(&ctx))
                .map_err(|e| anyhow::anyhow!("newline: failed to write to port: {e}"))?;

            Ok(LambdaReturn::Return(vec![]))
        }
    }
}

pub use procedures::{DisplayLam, Newline, WriteLam};

// #[derive(Default)]
/// `(scheme write)` implementation
pub struct Write;
// So that code can change the "default" input/output port, we have to emulate parameter objects natively.
// And this module would be created with 2 ports that it considered the "default" (and so would set to the parameter objects
// initially)

impl Module for Write {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        ["write", "display", "newline"]
            .into_iter()
            .map(|s| interner.get_or_intern_static(s))
            .collect()
    }

    fn value<'gc>(
        &self,
        mc: &gc_arena::Mutation<'gc>,
        symbol: &str,
    ) -> Option<crate::ValuePtr<'gc>> {
        macro_rules! lambda {
            ($lmb:expr) => {
                 Some(
                    lambda::Lambda::Native(
                        unsize![Gc::new(mc, $lmb) => dyn lambda::NativeLambda],
                    )
                    .into_value(mc)
                    .into_ptr(mc),
                )
            };
        }

        match symbol {
            "display" => lambda!(DisplayLam),
            "write" => lambda!(WriteLam),
            "newline" => lambda!(Newline),
            _ => None,
        }
    }
}

impl Registerable for Write {
    fn name(interner: &mut lasso::Rodeo) -> crate::LibraryName {
        LibraryName::from_iter(library_name!(interner => scheme write))
    }

    fn native(
        &self,
    ) -> Option<std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
        // We are a ZST, so we can do this~
        Some(std::sync::Arc::new(Self))
    }

    fn scheme(&self) -> Option<(&str, &str)> {
        None
    }

    fn scheme_native(
        &self,
        _interner: &mut lasso::Rodeo,
    ) -> Vec<(
        LibraryName,
        std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
    )> {
        Vec::new()
    }
}
