//! Redo treewalk, but with clearer concepts of continuation vs environment (scope)
//! while reusing most of the things we can, like the external interface

use core::fmt;
use std::{
    cell::RefMut,
    collections::{HashMap, VecDeque},
    sync::mpsc::{channel, Receiver, Sender},
};

use gc_arena::{Arena, Collect, Gc, Mutation, RefLock, Rootable};
use lasso::Rodeo;
use rowan::TextRange;
use slotmap::{new_key_type, HopSlotMap};

use crate::{
    create_environment_pair,
    environment::{Environment, StackEnvironmentPtr},
    runtime::{
        convert::IntoValue,
        error::{SchemeError, SchemeErrorPtr, SchemeErrorType, StackFrame},
        lambda::{LambdaCall, LambdaPtr},
    },
    treewalk::{
        context::Context,
        scheme::SchemeStd,
        stack_value::StackValue,
        virtual_inst::{convert_to_virtual, VirtualInstruction},
    },
    value::{self, ConsCell, Value, ValuePtr},
    ContainsDatum as _, Fuel, GAstNode as _, Module,
};

mod transformer;

pub use transformer::{Macro, MacroInstruction, MacroPtr, MacroReturn};

create_environment_pair!(
    pub Macro => MacroPtr<'gc>
);

new_key_type! {struct InnerExecutorKey;}
pub struct ExecutorKey(InnerExecutorKey, Sender<TreewalkMsg>);
impl fmt::Debug for ExecutorKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ExecutorKey").field(&self.0).finish()
    }
}
impl Clone for ExecutorKey {
    fn clone(&self) -> Self {
        let _ = self.1.send(TreewalkMsg::IncrementCount(self.0));
        Self(self.0, self.1.clone())
    }
}
impl Drop for ExecutorKey {
    fn drop(&mut self) {
        let _ = self.1.send(TreewalkMsg::DecrementCount(self.0));
    }
}

enum TreewalkMsg {
    IncrementCount(InnerExecutorKey),
    DecrementCount(InnerExecutorKey),
}

// I don't like this, but
struct Executors<'gc>(HopSlotMap<InnerExecutorKey, TreewalkExecutorPtr<'gc>>);

// What's the deal for external types...
unsafe impl<'gc> Collect for Executors<'gc> {
    #[inline]
    fn trace(&self, cc: &gc_arena::Collection) {
        for v in self.0.values() {
            v.trace(cc)
        }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct TreewalkArena<'gc> {
    null_val: ValuePtr<'gc>,
    executors: Executors<'gc>,
    pub state: TreewalkState<'gc>,

    pub scheme: SchemeStd<'gc>,
    // TODO look into DynamicRootSet, so that we can stash values into here
    // The use case is for certain API lambda, like (< (power card) 3)??
    // This might not be necessary (at these locations `<` would be overwritten with a lambda that pushes
    // a value representing "check if in ascending order" to stack)
    // NOTE we probably don't need this
}
type TreewalkRoot = Rootable![TreewalkArena<'_>];

pub struct Treewalk {
    arena: Arena<TreewalkRoot>,

    sender: Sender<TreewalkMsg>,
    receiver: Receiver<TreewalkMsg>,
    keys: HashMap<InnerExecutorKey, usize>,
}

impl Default for Treewalk {
    fn default() -> Self {
        let (sender, receiver) = channel();
        Self {
            arena: Arena::new(|mc| TreewalkArena {
                null_val: Gc::new(mc, RefLock::new(Value::Cons(ConsCell::empty()))),
                executors: Executors(HopSlotMap::with_key()),
                state: TreewalkState::new(mc),
                scheme: SchemeStd::new(mc),
            }),
            sender,
            receiver,
            keys: HashMap::new(),
        }
    }
}

impl Treewalk {
    fn collect_executors(&mut self) {
        for msg in self.receiver.try_iter() {
            match msg {
                // unmarked slots mean that exactly 1 key pointing there exists
                TreewalkMsg::IncrementCount(key) => {
                    *self.keys.entry(key).or_insert(1) += 1;
                }
                TreewalkMsg::DecrementCount(key) => {
                    // which we take advantage of here, by not even
                    // allocating a count if it is immediately decremented
                    if let Some(count) = self.keys.get_mut(&key) {
                        *count -= 1;
                    }

                    // because the only way something is None is if the count has *never*
                    // been incremented, and the first thing that is done is the count is decremented.
                    match self.keys.get(&key) {
                        Some(0) | None => {
                            // deallocate the executor
                            let _ = self.keys.remove(&key);
                            self.arena.mutate_root(|_mc, arena| {
                                arena.executors.0.remove(key);
                            });
                        }
                        _ => {}
                    }
                }
            }
        }

        // run gc
        self.arena.collect_debt();
    }

    pub fn arena_mut<R, F>(&mut self, func: F) -> R
    where
        F: FnOnce(&mut Arena<TreewalkRoot>) -> R,
    {
        func(&mut self.arena)
    }

    /// The source id identifies instructions and is copied to the stack trace
    pub fn new_executor<F>(&mut self, code: Module, source_id: usize, env_init: F) -> ExecutorKey
    where
        F: for<'gc> FnOnce(
            &'gc Mutation<'gc>,
            &TreewalkArena<'gc>,
            StackEnvironmentPtr<'gc>,
            MacroEnvironmentPtr<'gc>,
        ),
    {
        self.collect_executors();

        let inner = self.arena.mutate_root(|mc, arena| {
            let new_env = Gc::new(mc, RefLock::new(Environment::new(mc, None)));
            let new_macro_env = Gc::new(mc, RefLock::new(Environment::new(mc, None)));
            env_init(mc, arena, new_env, new_macro_env);
            let range = code.syntax().text_range();
            let exec = TreewalkExecutor::new(
                mc,
                &mut arena.state.interner.borrow_mut(mc),
                new_env,
                new_macro_env,
                code,
                source_id,
                range,
            );
            arena.executors.0.insert(exec)
        });

        ExecutorKey(inner, self.sender.clone())
    }

    pub fn run<F, O>(&mut self, key: ExecutorKey, func: F) -> Option<O>
    where
        F: for<'gc> FnOnce(Context<'gc>, RefMut<'gc, TreewalkExecutor<'gc>>) -> O,
    {
        self.collect_executors();

        self.arena.mutate(move |mc, arena| {
            if let Some(executor) = arena.executors.0.get(key.0).copied() {
                let context = arena.state.ctx(mc, arena.null_val);
                let exec = executor.borrow_mut(mc);
                Some(func(context, exec))
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct TreewalkState<'gc> {
    pub interner: Gc<'gc, RefLock<Rodeo>>,
}

impl<'gc> TreewalkState<'gc> {
    fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            interner: Gc::new_static(mc, RefLock::new(Rodeo::new())),
        }
    }

    fn ctx(&'gc self, mc: &'gc Mutation<'gc>, null_ptr: ValuePtr<'gc>) -> Context<'gc> {
        let mut interner = self.interner.borrow_mut(mc);
        let quote_sym = value::Symbol(interner.get_or_intern_static("quote"));
        Context {
            mutation: mc,
            null_ptr,
            interner,
            quote_sym,
        }
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
struct LambdaPackage<'gc> {
    call: LambdaCall<'gc>,
    ptr: LambdaPtr<'gc>,
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
struct MacroPackage<'gc> {
    /// Interpreted stack value causing the execution of a macro
    macro_call: StackValue<'gc>,
    ptr: MacroPtr<'gc>,
}

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub enum ContinuationItem<'gc> {
    /// Push a given value to stack
    Push(ValuePtr<'gc>),
    /// Execute a given value
    Execute(VirtualInstruction<'gc>),
    /// Whenever code would "return", it pops the stack.
    // NOTE This should be an ICE if it would pop the root scope
    PopScope,
}

#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct Scope<'gc> {
    pub environment: StackEnvironmentPtr<'gc>,
    pub macro_environment: MacroEnvironmentPtr<'gc>,

    #[collect(require_static)]
    range: Option<TextRange>,
    source_id: Option<usize>,
    label: Option<Box<str>>,

    error: Option<SchemeErrorPtr<'gc>>,
}

impl<'gc> Scope<'gc> {
    #[inline]
    pub fn label(&self) -> &str {
        self.maybe_label().unwrap_or("<<root>>")
    }

    #[inline]
    pub fn maybe_label(&self) -> Option<&str> {
        self.label.as_ref().map(|s| s.as_ref())
    }

    /// Error of the scope (if any)
    #[inline]
    pub fn error(&self) -> Option<SchemeErrorPtr<'gc>> {
        self.error
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TreewalkExecutor<'gc> {
    current_macro: Option<MacroPackage<'gc>>,
    current_lambda: Option<LambdaPackage<'gc>>,
    continuation: VecDeque<ContinuationItem<'gc>>,
    execution: Option<ContinuationItem<'gc>>,

    // !!!! FIXME IMPORTANT split the macro stack from the stack in general
    // so that macros don't confuse arguments
    // also, do macro checks almost like lambdas, so that macros can
    // also evaluate code!!!
    pub stack: Vec<StackValue<'gc>>,
    scope_stack: Vec<Scope<'gc>>,
}
pub type TreewalkExecutorPtr<'gc> = Gc<'gc, RefLock<TreewalkExecutor<'gc>>>;

#[derive(thiserror::Error, Debug)]
#[error("cannot step while root scope has errored")]
pub struct StepError;

impl<'gc> TreewalkExecutor<'gc> {
    fn new(
        mc: &Mutation<'gc>,
        interner: &mut Rodeo,
        environment: StackEnvironmentPtr<'gc>,
        macro_environment: MacroEnvironmentPtr<'gc>,
        code: Module,
        source_id: usize,
        range: TextRange,
    ) -> TreewalkExecutorPtr<'gc> {
        Gc::new(
            mc,
            RefLock::new(Self {
                stack: Vec::new(),
                scope_stack: vec![Scope {
                    error: None,
                    label: None,
                    range: Some(range),
                    environment,
                    macro_environment,

                    source_id: Some(source_id),
                }],
                current_lambda: None,
                current_macro: None,
                execution: None,
                continuation: code
                    .datum()
                    .map(|d| convert_to_virtual(d, interner, mc, Some(source_id)))
                    .map(ContinuationItem::Execute)
                    .collect(),
            }),
        )
    }

    /// Add a new frame on top of the current one
    /// evaluating a given source
    pub fn add_source(
        &mut self,
        _ctx: &Context<'gc>,
        _code: Module,
        _source_id: usize,
        _range: TextRange,
    ) {
        todo!()
    }

    /// Create a new stack value in the current scope
    pub fn current_scope_value_ptr(
        &self,
        mc: &Mutation<'gc>,
        value: ValuePtr<'gc>,
    ) -> StackValue<'gc> {
        let Some(scope) = self.scope() else {
            unreachable!("[ICE] create value in null scope");
        };

        StackValue {
            value,
            range: scope.range,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
            source_id: None,
        }
    }

    /// Create a new stack value in the current scope
    pub fn current_scope_value(
        &self,
        mc: &Mutation<'gc>,
        value: impl IntoValue<'gc>,
    ) -> StackValue<'gc> {
        self.current_scope_value_ptr(mc, Gc::new(mc, RefLock::new(value.into_value(mc))))
    }

    /// Access to the current lambda call (if any)
    #[inline]
    pub fn lambda_call(&self) -> Option<&LambdaCall<'gc>> {
        self.current_lambda.as_ref().map(|lp| &lp.call)
    }

    /// Access to the current macro (if any)
    #[inline]
    pub fn macro_call(&self) -> Option<(MacroPtr<'gc>, StackValue<'gc>)> {
        self.current_macro
            .as_ref()
            .map(|mp| (mp.ptr, mp.macro_call))
    }

    /// root scope is always the first scope
    pub fn all_scopes(&self) -> impl Iterator<Item = &Scope<'gc>> {
        self.scope_stack.iter()
    }

    pub fn scope(&self) -> Option<&Scope<'gc>> {
        self.scope_stack.last()
    }

    pub fn continuation(&self) -> impl Iterator<Item = &ContinuationItem<'gc>> {
        self.continuation.iter()
    }

    fn scope_mut(&mut self) -> Option<&mut Scope<'gc>> {
        self.scope_stack.last_mut()
    }

    pub fn stack(&self) -> &[StackValue<'gc>] {
        &self.stack
    }

    pub fn can_continue(&self) -> bool {
        (!self.continuation.is_empty()
            || self.current_macro.is_some()
            || self.current_lambda.is_some())
            && self
                .scope_stack
                .first()
                .is_some_and(|rs| rs.error.is_none())
    }

    pub fn current_env(&self) -> Option<StackEnvironmentPtr<'gc>> {
        self.scope().map(|sc| sc.environment)
    }

    pub fn current_macro_env(&self) -> Option<MacroEnvironmentPtr<'gc>> {
        self.scope().map(|sc| sc.macro_environment)
    }

    // Make a backtrace of frames until this one
    fn build_error(
        &self,
        mc: &Mutation<'gc>,
        error_type: SchemeErrorType<'gc>,
    ) -> SchemeErrorPtr<'gc> {
        let backtrace = self
            .scope_stack
            .iter()
            .map(|scope| StackFrame {
                range: scope.range,
                scope_label: scope.label.clone(),
                source_id: scope.source_id,
            })
            .collect();

        Gc::new(
            mc,
            SchemeError {
                backtrace,
                error_type,
            },
        )
    }

    /// Compounds existing errors with a new error (if any), and sets the current
    /// scopes error to that result.
    fn raise_error(&mut self, err_ptr: SchemeErrorPtr<'gc>, ctx: &Context<'gc>) {
        // combine the generated error pointer with any existing errors
        let err_ptr = if let Some(err) = &self.scope().and_then(|sc| sc.error) {
            let mut errors: Vec<_> = match &err.error_type {
                SchemeErrorType::Compound(errors) => errors.to_vec(),
                _ => Vec::new(),
            };

            if !errors.iter().any(|ep| Gc::ptr_eq(*ep, err_ptr)) {
                errors.push(err_ptr);
            }

            Gc::new(
                ctx.mutation,
                SchemeError {
                    backtrace: err.backtrace.clone(),
                    error_type: SchemeErrorType::Compound(errors),
                },
            )
        } else {
            err_ptr
        };

        let Some(sc) = self.scope_mut() else {
            unreachable!("[ICE] raised error in null scope");
        };

        sc.error = Some(err_ptr);
    }

    fn is_reserved_form(ctx: &Context<'gc>, head: value::Symbol) -> bool {
        head == ctx.quote_sym
    }

    /// Handle "reserved forms": quote, quasiquote, unquote, unquote-splicing
    fn handle_reserved_form(
        &mut self,
        ctx: &Context<'gc>,
        form_sym: value::Symbol,
        reserved_body: VirtualInstruction<'gc>,
    ) {
        todo!()
    }

    fn translate_macro(inst: MacroInstruction<'gc>) -> impl Iterator<Item = ContinuationItem<'gc>> {
        std::iter::empty()
    }

    /// Returns true if the macro want to resuspend execution
    fn call_macro(
        &mut self,
        mcr: MacroPtr<'gc>,
        this: StackValue<'gc>,
        ctx: &Context<'gc>,
        fuel: &mut Fuel,
    ) -> bool {
        let mut macro_access = mcr.borrow_mut(ctx.mutation);
        match macro_access.rewrite(this, ctx, fuel) {
            Ok(res) => match res {
                MacroReturn::Waiting => true,
                MacroReturn::Return { inst } => {
                    // self.push(self.current_scope_value(ctx.mutation, ret));
                    self.continuation = inst
                        .into_iter()
                        .flat_map(Self::translate_macro)
                        .chain(self.continuation.drain(..))
                        .collect();
                    false
                }
            },
            Err(e) => {
                self.raise_error(
                    self.build_error(ctx.mutation, SchemeErrorType::Macro(e)),
                    ctx,
                );
                false
            }
        }
    }

    /// Returns Some(true) if macro found
    /// Some(false) no macro found
    /// None cannot execute this list
    fn check_list_for_macro(
        &mut self,
        ctx: &Context<'gc>,
        fuel: &mut Fuel,
        exec: &VirtualInstruction<'gc>,
        head: &VirtualInstruction<'gc>,
        body: &[VirtualInstruction<'gc>],
        dot: Option<&VirtualInstruction<'gc>>,
    ) -> Option<bool> {
        // for lists, we only allow the head to be a symbol
        // or list. if it isn't either, this is not executable
        if dot.is_some() || !exec.payload.list_executable() {
            self.raise_error(
                self.build_error(ctx.mutation, SchemeErrorType::BadList),
                ctx,
            );
            return None;
        }

        if head
            .payload
            .as_symbol()
            .is_some_and(|sym| Self::is_reserved_form(ctx, sym))
        {
            let form_sym = head.payload.as_symbol().unwrap();
            // handle the reserved forms (quoting)
            if body.len() == 1 {
                self.handle_reserved_form(ctx, form_sym, body[0].clone());
            } else {
                // all reserved forms have exactly 1 body, so this is a macro err
                self.raise_error(
                    self.build_error(
                        ctx.mutation,
                        SchemeErrorType::MacroForm(anyhow::anyhow!(
                            "`{}` needs exactly 1 argument (found {})",
                            ctx.interner.resolve(&form_sym.0),
                            body.len()
                        )),
                    ),
                    ctx,
                );
            }
            return Some(true);
        } else if let Some(mcr) = head
            .payload
            .as_symbol()
            .and_then(|sym| {
                self.current_macro_env()
                    .and_then(|env| env.borrow().get(sym))
            })
            .map(|mcrb| *mcrb.get().borrow())
        {
            let args: Vec<_> = body
                .iter()
                .cloned()
                .map(|vi| vi.payload.datum().clone())
                .collect();

            if let Err(err) = mcr.borrow().is_properly_formed(&args) {
                self.raise_error(
                    self.build_error(ctx.mutation, SchemeErrorType::MacroForm(err)),
                    ctx,
                );
                return Some(true);
            }

            let macro_bottom = Some(self.stack.len());
            // push values to stack *as-is*
            self.stack
                .extend(body.iter().cloned().map(|vi| vi.into_value(ctx)));

            let this = exec.clone().into_value(ctx);
            if self.call_macro(mcr, this, ctx, fuel) {
                self.current_macro = Some(MacroPackage {
                    macro_call: this,
                    ptr: mcr,
                });
            }
            return Some(true);
        }

        Some(false)
    }

    pub fn step(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) -> Result<(), StepError> {
        todo!()
    }
}
