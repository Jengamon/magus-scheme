//! SRFI implementations

/// SRFI 1
pub mod list {
    use std::{collections::HashSet, sync::Arc};

    use crate::{
        LibraryName, compiler::Module, interpreter::Registerable, library_name, stdlib::magus_impl,
    };

    pub struct Srfi1;

    impl Module for Srfi1 {
        fn all_symbols(
            &self,
            _interner: &mut lasso::Rodeo,
        ) -> std::collections::HashSet<lasso::Spur> {
            HashSet::new()
        }
    }

    const MODULE_SRC: &str = include_str!("srfi_list.scm");

    impl Registerable for Srfi1 {
        fn name(interner: &mut lasso::Rodeo) -> LibraryName {
            LibraryName::from_iter(library_name!(interner => srfi 1))
        }

        fn native(&self) -> Option<Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
            Some(Arc::new(Self))
        }

        fn scheme(&self) -> Option<(&str, &str)> {
            Some(("srfi_list.scm", MODULE_SRC))
        }

        fn scheme_native(
            &self,
            interner: &mut lasso::Rodeo,
        ) -> Vec<(
            LibraryName,
            std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>,
        )> {
            vec![(
                LibraryName::from_iter(library_name!(interner => magus impl)),
                Arc::new(magus_impl::MagusImpl),
            )]
        }

        fn scheme_dependency(&self, interner: &mut lasso::Rodeo) -> Vec<LibraryName> {
            [
                LibraryName::from_iter(library_name!(interner => scheme base)),
                LibraryName::from_iter(library_name!(interner => scheme cxr)),
            ]
            .into_iter()
            .collect()
        }
    }
}

/// SRFI 151: bitwise operations
pub mod bitwise {
    use gc_arena::{Gc, unsize};

    use crate::{
        LibraryName, Registerable,
        compiler::Module,
        library_name,
        runtime::{convert::IntoValue, lambda},
    };

    mod procedures {
        use gc_arena::{Collect, Gc};

        use crate::{
            Value,
            runtime::lambda::{
                Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext,
                NativeLambdaState,
            },
            value::{ConsCell, Number},
        };

        #[derive(Debug, Collect)]
        #[collect(require_static)]
        pub struct BitwiseNot;

        impl<'gc> NativeLambda<'gc> for BitwiseNot {
            fn name(&self) -> &str {
                "bitwise-not"
            }

            fn arity(&self) -> Arity {
                Arity::Exact(1)
            }

            fn run(
                &self,
                _state: &mut NativeLambdaState<'gc>,
                ctx: NativeLambdaContext<'_, 'gc>,
                args: &[crate::ValuePtr<'gc>],
            ) -> Result<LambdaReturn<'gc>, LambdaError> {
                let Value::Number(n) = *args[0].borrow() else {
                    return Err(anyhow::anyhow!(
                        "bitwise-not expects an integer as its argument"
                    ))?;
                };

                let Number::Integer(i) = &*n else {
                    return Err(anyhow::anyhow!(
                        "bitwise-not expects an integer as its argument"
                    ))?;
                };

                Ok(LambdaReturn::Return(vec![
                    Value::Number(Gc::new(&ctx, Number::Integer(!i))).into_ptr(&ctx),
                ]))
            }
        }

        #[derive(Debug, Collect)]
        #[collect(require_static)]
        pub struct BitsToList;

        impl<'gc> NativeLambda<'gc> for BitsToList {
            fn name(&self) -> &str {
                "bits->list"
            }

            fn arity(&self) -> Arity {
                Arity::Bounded { min: 1, max: 2 }
            }

            fn run(
                &self,
                _state: &mut NativeLambdaState<'gc>,
                ctx: NativeLambdaContext<'_, 'gc>,
                args: &[crate::ValuePtr<'gc>],
            ) -> Result<LambdaReturn<'gc>, LambdaError> {
                let Value::Number(bitn) = *args[0].borrow() else {
                    return Err(anyhow::anyhow!(
                        "bits->list expects a non-negative integer as its first argument"
                    ))?;
                };

                if bitn.is_negative() || !matches!(&*bitn, Number::Integer(_)) {
                    return Err(anyhow::anyhow!(
                        "bits->list expects a non-negative integer as its first argument"
                    ))?;
                }

                let Number::Integer(bitn) = &*bitn else {
                    unreachable!()
                };

                let limit = match args.get(1).map(|v| *v.borrow()) {
                    Some(Value::Number(n))
                        if matches!(&*n, Number::Integer(_)) && !n.is_negative() =>
                    {
                        use num::ToPrimitive;
                        let Number::Integer(n) = &*n else {
                            unreachable!();
                        };
                        Some(
                            n.to_u64()
                                .ok_or(anyhow::anyhow!("bits->list: index too large"))?,
                        )
                    }
                    Some(_) => {
                        return Err(anyhow::anyhow!(
                            "bits->list expect a non-negative integer as it's second argument"
                        ))?;
                    }
                    None => None,
                };

                let bits = limit.unwrap_or(bitn.bits()).max(1);
                let list = (0..bits).rev().map(|i| ctx.bool(bitn.bit(i)));
                let cons = ConsCell::from_iter(&ctx, ctx.thread_ctx.null_value, list);

                Ok(LambdaReturn::Return(vec![cons]))
            }
        }
    }

    pub use procedures::{BitsToList, BitwiseNot};

    #[derive(Debug)]
    pub struct Srfi151;

    impl Module for Srfi151 {
        fn all_symbols(
            &self,
            interner: &mut lasso::Rodeo,
        ) -> std::collections::HashSet<lasso::Spur> {
            ["bitwise-not", "bits->list"]
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
                "bitwise-not" => lambda!(BitwiseNot),
                "bits->list" => lambda!(BitsToList),
                _ => None,
            }
        }
    }

    impl Registerable for Srfi151 {
        fn name(interner: &mut lasso::Rodeo) -> crate::LibraryName {
            LibraryName::from_iter(library_name!(interner => srfi 151))
        }

        fn native(
            &self,
        ) -> Option<std::sync::Arc<dyn crate::compiler::Module + Send + Sync + 'static>> {
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
            vec![]
        }
    }
}
