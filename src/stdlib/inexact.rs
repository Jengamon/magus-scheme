use std::sync::Arc;

use crate::{LibraryName, compiler::Module, interpreter::Registerable, library_name};

pub use procedures::{
    Acos, Asin, Atan, Cos, Exp, IsFinite, IsInfinite, IsNan, Log, Sin, Sqrt, Tan,
};
mod procedures {
    // TODO Support inexact complex and complex numbers!
    use gc_arena::Collect;
    use num::Zero;

    use crate::{
        Value, ValuePtr,
        runtime::lambda::{
            Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext, NativeLambdaState,
        },
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Sin;
    impl<'gc> NativeLambda<'gc> for Sin {
        fn name(&self) -> &str {
            "sin"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.sin(),
                Value::Number(n) => n.to_inexact().sin(),
                _ => {
                    return Err(anyhow::anyhow!("sin expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Cos;
    impl<'gc> NativeLambda<'gc> for Cos {
        fn name(&self) -> &str {
            "cos"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.cos(),
                Value::Number(n) => n.to_inexact().cos(),
                _ => {
                    return Err(anyhow::anyhow!("cos expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Tan;
    impl<'gc> NativeLambda<'gc> for Tan {
        fn name(&self) -> &str {
            "tan"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.tan(),
                Value::Number(n) => n.to_inexact().tan(),
                _ => {
                    return Err(anyhow::anyhow!("tan expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Asin;
    impl<'gc> NativeLambda<'gc> for Asin {
        fn name(&self) -> &str {
            "asin"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.asin(),
                Value::Number(n) => n.to_inexact().asin(),
                _ => {
                    return Err(anyhow::anyhow!("asin expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Acos;
    impl<'gc> NativeLambda<'gc> for Acos {
        fn name(&self) -> &str {
            "acos"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.acos(),
                Value::Number(n) => n.to_inexact().acos(),
                _ => {
                    return Err(anyhow::anyhow!("acos expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Atan;
    impl<'gc> NativeLambda<'gc> for Atan {
        fn name(&self) -> &str {
            "atan"
        }

        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args.len() == 1 {
                // Single argument atan
                let value = match *args[0].borrow() {
                    Value::Number(n) => n.to_inexact().atan(),
                    Value::Inexact(i) => i.atan(),
                    _ => Err(anyhow::anyhow!("atan expects a number as its argument"))?,
                };

                Ok(LambdaReturn::Return(vec![
                    Value::Inexact(value).into_ptr(&ctx),
                ]))
            } else if args.len() == 2 {
                // 2-argument atan
                let y = match *args[0].borrow() {
                    Value::Number(n) => n.to_inexact(),
                    Value::Inexact(i) => i,
                    _ => Err(anyhow::anyhow!(
                        "atan expects a number as its first argument"
                    ))?,
                };
                let x = match *args[1].borrow() {
                    Value::Number(n) => n.to_inexact(),
                    Value::Inexact(i) => i,
                    _ => Err(anyhow::anyhow!(
                        "atan expects a number as its second argument"
                    ))?,
                };

                Ok(LambdaReturn::Return(vec![
                    Value::Inexact(y.atan2(x)).into_ptr(&ctx),
                ]))
            } else {
                Err(anyhow::anyhow!("atan expects either 1 or 2 arguments"))?
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Exp;
    impl<'gc> NativeLambda<'gc> for Exp {
        fn name(&self) -> &str {
            "exp"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            let res = match *args[0].borrow() {
                Value::Inexact(i) => i.exp(),
                Value::Number(n) => n.to_inexact().exp(),
                _ => {
                    return Err(anyhow::anyhow!("exp expects a number as its argument"))?;
                }
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(res).into_ptr(&ctx),
            ]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Log;
    impl<'gc> NativeLambda<'gc> for Log {
        fn name(&self) -> &str {
            "log"
        }

        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args.len() == 1 {
                // natural logarithm
                let value = match *args[0].borrow() {
                    Value::Number(n) => n.to_inexact().ln(),
                    Value::Inexact(i) => i.ln(),
                    _ => Err(anyhow::anyhow!("log expects a number as its argument"))?,
                };

                Ok(LambdaReturn::Return(vec![
                    Value::Inexact(value).into_ptr(&ctx),
                ]))
            } else if args.len() == 2 {
                // base logarithm
                let arg = match *args[0].borrow() {
                    Value::Number(n) => n.to_inexact(),
                    Value::Inexact(i) => i,
                    _ => Err(anyhow::anyhow!(
                        "log expects a number as its first argument"
                    ))?,
                };
                let base = match *args[1].borrow() {
                    Value::Number(n) => n.to_inexact(),
                    Value::Inexact(i) => i,
                    _ => Err(anyhow::anyhow!(
                        "log expects a number as its second argument"
                    ))?,
                };

                Ok(LambdaReturn::Return(vec![
                    Value::Inexact(arg.log(base)).into_ptr(&ctx),
                ]))
            } else {
                Err(anyhow::anyhow!("log expects either 1 or 2 arguments"))?
            }
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsFinite;

    impl<'gc> NativeLambda<'gc> for IsFinite {
        fn name(&self) -> &str {
            "finite?"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO Support complex numbers
            let is_finite = match *args[0].borrow() {
                Value::Number(_n) => true,
                Value::Inexact(i) => i.is_finite(),
                _ => Err(anyhow::anyhow!("finite? expects a number as its argument"))?,
            };

            Ok(LambdaReturn::Return(vec![if is_finite {
                ctx.thread_ctx.true_value
            } else {
                ctx.thread_ctx.false_value
            }]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsInfinite;

    impl<'gc> NativeLambda<'gc> for IsInfinite {
        fn name(&self) -> &str {
            "infinite?"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO Support complex numbers
            let is_infinite = match *args[0].borrow() {
                Value::Number(_n) => false,
                Value::Inexact(i) => i.is_infinite(),
                _ => Err(anyhow::anyhow!(
                    "infinite? expects a number as its argument"
                ))?,
            };

            Ok(LambdaReturn::Return(vec![if is_infinite {
                ctx.thread_ctx.true_value
            } else {
                ctx.thread_ctx.false_value
            }]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct IsNan;

    impl<'gc> NativeLambda<'gc> for IsNan {
        fn name(&self) -> &str {
            "nan?"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO Support complex numbers
            let is_nan = match *args[0].borrow() {
                Value::Number(_n) => false,
                Value::Inexact(i) => i.is_nan(),
                _ => Err(anyhow::anyhow!("nan? expects a number as its argument"))?,
            };

            Ok(LambdaReturn::Return(vec![if is_nan {
                ctx.thread_ctx.true_value
            } else {
                ctx.thread_ctx.false_value
            }]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Sqrt;

    impl<'gc> NativeLambda<'gc> for Sqrt {
        fn name(&self) -> &str {
            "sqrt"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            // TODO COMPLEX. NUMBERS!
            let value = match *args[0].borrow() {
                Value::Number(n) if n.is_positive() => n.to_inexact().sqrt(),
                Value::Inexact(i) if i.is_sign_positive() => i.sqrt(),
                Value::Number(n) if n.is_zero() => 0.,
                Value::Inexact(0.) => 0.,
                Value::Inexact(_) | Value::Number(_) => {
                    Err(anyhow::anyhow!("complex numbers are not supported yet"))?
                }
                _ => Err(anyhow::anyhow!("sqrt expects a number as its argument"))?,
            };

            Ok(LambdaReturn::Return(vec![
                Value::Inexact(value).into_ptr(&ctx),
            ]))
        }
    }
}

pub struct Inexact;

impl Module for Inexact {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        [
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "exp",
            "log",
            "sqrt",
            "finite?",
            "infinite?",
            "nan?",
        ]
        .into_iter()
        .map(|n| interner.get_or_intern_static(n))
        .collect()
    }

    fn value<'gc>(
        &self,
        mc: &gc_arena::Mutation<'gc>,
        symbol: &str,
    ) -> Option<crate::ValuePtr<'gc>> {
        use crate::runtime::convert::IntoValue;
        macro_rules! lambda {
            ($lmb:expr) => {
                 Some(
                    $crate::runtime::lambda::Lambda::Native(
                        gc_arena::unsize![gc_arena::Gc::new(mc, $lmb) => dyn $crate::runtime::lambda::NativeLambda],
                    )
                    .into_value(mc)
                    .into_ptr(mc),
                )
            };
        }

        match symbol {
            "sin" => lambda!(Sin),
            "cos" => lambda!(Cos),
            "tan" => lambda!(Tan),
            "atan" => lambda!(Atan),
            "asin" => lambda!(Asin),
            "acos" => lambda!(Acos),
            "exp" => lambda!(Exp),
            "log" => lambda!(Log),
            "sqrt" => lambda!(Sqrt),
            "finite?" => lambda!(IsFinite),
            "infinite?" => lambda!(IsInfinite),
            "nan?" => lambda!(IsNan),
            _ => None,
        }
    }
}

impl Registerable for Inexact {
    fn name(interner: &mut lasso::Rodeo) -> LibraryName {
        LibraryName::from_iter(library_name!(interner => scheme inexact))
    }

    fn native(
        &self,
    ) -> Option<std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
        Some(Arc::new(Self))
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
        vec![]
    }
}
