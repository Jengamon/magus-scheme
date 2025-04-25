use crate::{
    LibraryName,
    compiler::Module,
    interpreter::Registerable,
    library_name,
    runtime::{convert::IntoValue, lambda},
};
use gc_arena::{Gc, RefLock, unsize};

mod procedures {
    use gc_arena::Collect;

    use crate::{
        Value, ValuePtr,
        runtime::lambda::{Arity, LambdaError, LambdaReturn, NativeLambda, NativeLambdaContext},
        value::ModeWrite,
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct DisplayLam;

    impl<'gc> NativeLambda<'gc> for DisplayLam {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &mut self,
            _ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args.len() > 2 {
                return Err(anyhow::anyhow!("display expects either 1 or 2 arguments"))?;
            }
            todo!()
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct WriteLam;

    impl<'gc> NativeLambda<'gc> for WriteLam {
        fn arity(&self) -> Arity {
            Arity::AtLeast(1)
        }

        fn run(
            &mut self,
            ctx: NativeLambdaContext<'_, 'gc>,
            args: &[ValuePtr<'gc>],
        ) -> Result<LambdaReturn<'gc>, LambdaError> {
            if args.len() > 2 {
                return Err(anyhow::anyhow!("write expects either 1 or 2 arguments"))?;
            }

            eprintln!(
                "{}",
                Value::resolve_into::<_, ModeWrite>(
                    args[0],
                    ctx.interner.clone(),
                    ctx.thread_ctx.null_value
                )
            );

            // rn just ignore ports, and just dump to stdout
            Ok(LambdaReturn::Return(vec![]))
        }
    }
}

pub use procedures::{DisplayLam, WriteLam};

// #[derive(Default)]
pub struct Write;
// So that code can change the "default" input/output port, we have to emulate parameter objects natively.
// And this module would be created with 2 ports that it considered the "default" (and so would set to the parameter objects
// initially)

impl Module for Write {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        ["write", "display"]
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
                        unsize![Gc::new(mc, RefLock::new($lmb)) => RefLock<dyn lambda::NativeLambda>],
                    )
                    .into_value(mc)
                    .into_ptr(mc),
                )
            };
        }

        match symbol {
            "display" => lambda!(DisplayLam),
            "write" => lambda!(WriteLam),
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
