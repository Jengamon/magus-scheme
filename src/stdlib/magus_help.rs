//! Magus help system

use crate::{
    LibraryName, Registerable,
    compiler::Module,
    library_name,
    runtime::{convert::IntoValue, lambda},
};

use gc_arena::{Gc, unsize};
use procedures::{Help, SetHelp};

mod procedures {
    use gc_arena::{Collect, Gc, RefLock};

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaReturn, NativeLambda, NativeLambdaState},
        value,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Help;

    impl<'gc> NativeLambda<'gc> for Help {
        fn name(&self) -> &str {
            "help"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            let Value::Lambda(l) = *args[0].borrow() else {
                return Err(anyhow::anyhow!("help expects a lambda as its argument"))?;
            };

            let help_string = l.doc_string(Some(self));

            Ok(LambdaReturn::Return(vec![if let Some(s) = help_string {
                Value::String(value::String::new_frozen(Gc::new(&ctx, RefLock::new(s))))
                    .into_ptr(&ctx)
            } else {
                ctx.thread_ctx.false_value
            }]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct SetHelp;

    impl<'gc> NativeLambda<'gc> for SetHelp {
        fn name(&self) -> &str {
            "set-help"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(2)
        }

        fn run(
            &self,
            _state: &mut NativeLambdaState<'gc>,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, crate::runtime::lambda::LambdaError> {
            let Value::Lambda(l) = *args[0].borrow() else {
                return Err(anyhow::anyhow!(
                    "set-help expects a lambda as its first argument"
                ))?;
            };

            let Value::String(s) = *args[1].borrow() else {
                return Err(anyhow::anyhow!(
                    "set-help expects a string as its second argument"
                ))?;
            };

            if l.doc_string(Some(self)).is_some() {
                return Err(anyhow::anyhow!("set-help: lambda already has doc string"))?;
            }

            if !l.set_doc_string(&ctx, Some(s)) {
                return Err(anyhow::anyhow!("set-help: could not set doc string"))?;
            }

            Ok(LambdaReturn::Return(vec![args[0]]))
        }
    }
}

pub struct MagusHelp;

impl Module for MagusHelp {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        ["help", "set-help"]
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
            "help" => lambda!(Help),
            "set-help" => lambda!(SetHelp),
            _ => None,
        }
    }
}

impl Registerable for MagusHelp {
    fn name(interner: &mut lasso::Rodeo) -> crate::LibraryName {
        LibraryName::from_iter(library_name!(interner => magus help))
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
