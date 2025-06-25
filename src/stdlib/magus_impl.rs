//! Not really a "module" in the way the others are, this is used by
//! library code to do things outside the R7RS standard (like create undefined values)

use crate::{
    compiler::Module,
    runtime::{convert::IntoValue, lambda},
};

use gc_arena::{Gc, RefLock, unsize};

mod procedures {
    use gc_arena::Collect;

    use crate::{
        Value,
        runtime::lambda::{Arity, LambdaReturn, NativeLambda},
    };

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Undefined;

    impl<'gc> NativeLambda<'gc> for Undefined {
        fn name(&self) -> &str {
            "undefined"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(0)
        }

        fn run(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            _args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            Ok(LambdaReturn::Return(vec![Value::Undefined.into_ptr(&ctx)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct Void;

    impl<'gc> NativeLambda<'gc> for Void {
        fn name(&self) -> &str {
            "void"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(0)
        }

        fn run(
            &mut self,
            ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            _args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            Ok(LambdaReturn::Return(vec![Value::Void.into_ptr(&ctx)]))
        }
    }

    #[derive(Debug, Collect)]
    #[collect(require_static)]
    pub struct DebugPrint;

    impl<'gc> NativeLambda<'gc> for DebugPrint {
        fn name(&self) -> &str {
            "debug"
        }

        fn arity(&self) -> Arity {
            Arity::Exact(1)
        }

        fn run(
            &mut self,
            _ctx: crate::runtime::lambda::NativeLambdaContext<'_, 'gc>,
            args: &[crate::ValuePtr<'gc>],
        ) -> Result<crate::runtime::lambda::LambdaReturn<'gc>, crate::runtime::lambda::LambdaError>
        {
            dbg!(args[0]);
            Ok(LambdaReturn::Return(vec![args[0]]))
        }
    }
}

pub struct MagusImpl;

impl Module for MagusImpl {
    fn all_symbols(&self, interner: &mut lasso::Rodeo) -> std::collections::HashSet<lasso::Spur> {
        ["undefined", "void", "debug"]
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
            "undefined" => lambda!(procedures::Undefined),
            "void" => lambda!(procedures::Void),
            "debug" => lambda!(procedures::DebugPrint),
            _ => None,
        }
    }
}
