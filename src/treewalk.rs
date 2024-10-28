// Compilation is for *losers*. Get from code to execution as fast as possible.
//
// More seriously, this is executing the GAst as is, w/o a separate compilation
// step. Build an initial script environment, send it to us, and we are off!

// We don't need Source, but we do want the environment

use core::fmt;
use std::{
    cell::RefMut,
    collections::{HashMap, VecDeque},
    sync::mpsc::{channel, Receiver, Sender},
};

use gc_arena::{Arena, Collect, Gc, Mutation, RefLock, Rootable};
use lasso::Rodeo;
use rowan::TextRange;
use scheme::SchemeStd;
use slotmap::{new_key_type, HopSlotMap};
use virtual_inst::{
    convert_to_virtual, CodeChunkPtr, VirtualInstruction, VirtualInstructionDatum,
    VirtualInstructionPayload,
};

use crate::{
    environment::{Environment, EnvironmentPtr},
    runtime::{
        convert::IntoValue,
        error::{SchemeError, SchemeErrorPtr, SchemeErrorType, StackFrame},
        lambda::{LambdaCall, LambdaExecError, LambdaPtr, ProcedureError, ProcedureReturn},
        FuelCosts,
    },
    transformer::{MacroInstruction, MacroPtr, MacroReturn},
    value::{self, ConsCell, Value, ValuePtr, ValueType},
    ContainsDatum, Fuel, GAstNode, Module,
};

pub mod scheme;
pub mod virtual_inst;

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
                null_val: Gc::new(
                    mc,
                    RefLock::new(Value::Cons(ConsCell {
                        car: None,
                        cdr: None,
                    })),
                ),
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
        F: for<'gc> FnOnce(&'gc Mutation<'gc>, &TreewalkArena<'gc>, EnvironmentPtr<'gc>),
    {
        self.collect_executors();

        let inner = self.arena.mutate_root(|mc, arena| {
            let new_env = Gc::new(mc, RefLock::new(Environment::new(mc, None)));
            env_init(mc, arena, new_env);
            let range = code.syntax().text_range();
            let exec = TreewalkExecutor::new(
                mc,
                &mut arena.state.interner.borrow_mut(mc),
                new_env,
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

/// A value pointer enhanced with source tracking information
#[derive(Debug, Clone, Collect, Copy)]
#[collect(no_drop)]
pub struct StackValue<'gc> {
    pub(crate) value: ValuePtr<'gc>,
    #[collect(require_static)]
    pub(crate) range: Option<TextRange>,
    /// preservation of touch_count when used as data
    pub(crate) touch_count: Gc<'gc, RefLock<usize>>,
    /// If this value has been JIT'ed, what is the chunk
    /// If it hasn't, store if it is possible to JIT (if we haven't tried already)
    pub(crate) chunk: Result<CodeChunkPtr<'gc>, Gc<'gc, RefLock<bool>>>,
    /// Source id from VirtualInstruction
    pub(crate) source_id: Option<usize>,
}

impl<'gc> StackValue<'gc> {
    pub fn touch_count(&self) -> usize {
        *self.touch_count.borrow()
    }

    /// This is for stack values generated externally, not from anywhere in the source
    pub fn external<V>(mc: &Mutation<'gc>, v: V) -> Self
    where
        V: IntoValue<'gc>,
    {
        Self {
            value: Gc::new(mc, RefLock::new(v.into_value(mc))),
            range: None,
            source_id: None,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
        }
    }
}

/// Transparently access the value of this pointer
impl<'gc> std::ops::Deref for StackValue<'gc> {
    type Target = ValuePtr<'gc>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub struct Context<'gc> {
    pub mutation: &'gc Mutation<'gc>,
    pub interner: RefMut<'gc, Rodeo>,
    pub null_ptr: ValuePtr<'gc>,
    // symbol for "quote" from the interner
    quote_sym: value::Symbol,
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TreewalkExecutor<'gc> {
    rewrite_queue: VecDeque<MacroInstruction<'gc>>,
    pub stack: Vec<StackValue<'gc>>,
    root_scope: Scope<'gc>,
    scope_stack: Vec<Scope<'gc>>,
}
pub type TreewalkExecutorPtr<'gc> = Gc<'gc, RefLock<TreewalkExecutor<'gc>>>;

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
enum ContinuationItem<'gc> {
    /// execute an instruction
    Instruction(VirtualInstruction<'gc>),
    /// execute a *macro* instruction
    Rewrite(MacroInstruction<'gc>),
    /// call a function. this should be a tail call if there
    /// are no instructions after this
    Call(usize),
}

// TODO use ContinuationItem instead of this
#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
enum ScopeExecution<'gc> {
    Instruction(VirtualInstruction<'gc>),
    Rewrite(MacroInstruction<'gc>),
    Empty,
}

impl<'gc> ScopeExecution<'gc> {
    fn is_some(&self) -> bool {
        !self.is_none()
    }

    fn is_none(&self) -> bool {
        matches!(self, ScopeExecution::Empty)
    }

    fn take(&mut self) -> ScopeExecution<'gc> {
        std::mem::replace(self, ScopeExecution::Empty)
    }
}

// A scope consists of an environment, next instructions (continuation)
// and a "bottom", which is a marker for a sections of the stack the scope
// doesn't have access to.
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct Scope<'gc> {
    pub environment: EnvironmentPtr<'gc>,

    #[collect(require_static)]
    range: Option<TextRange>,
    source_id: Option<usize>,
    label: Option<Box<str>>,

    error: Option<SchemeErrorPtr<'gc>>,

    // FIXME merge the stuff here together with rewrite_queue
    // to form "continuation", which is want determined what we do next
    // the overall process should make scopes just a stack of
    // environments again
    #[collect(require_static)]
    executable: Executable,
    next_datum: VecDeque<VirtualInstruction<'gc>>,

    /// TODO move these to the execututor too.
    current_macro: Option<(MacroPtr<'gc>, Option<usize>)>,
    current_lambda: Option<(LambdaCall<'gc>, LambdaPtr<'gc>)>,

    // TODO remove these
    bottom: Option<usize>,
    processed: usize,
    execution: ScopeExecution<'gc>,
}

impl<'gc> Scope<'gc> {
    #[inline]
    pub fn bottom(&self) -> Option<usize> {
        self.bottom
    }

    #[inline]
    pub fn label(&self) -> &str {
        self.label
            .as_ref()
            .map(|s| s.as_ref())
            .unwrap_or("<<root>>")
    }

    /// Number of processed instructions
    #[inline]
    pub fn processed(&self) -> usize {
        self.processed
    }

    /// Error of the scope (if any)
    #[inline]
    pub fn error(&self) -> Option<SchemeErrorPtr<'gc>> {
        self.error
    }

    /// Access to the current lambda call (if any)
    #[inline]
    pub fn lambda_call(&self) -> Option<&LambdaCall<'gc>> {
        self.current_lambda.as_ref().map(|(call, _)| call)
    }

    /// Access to the current macro (if any)
    #[inline]
    pub fn macro_call(&self) -> Option<(MacroPtr<'gc>, Option<usize>)> {
        self.current_macro
    }
}

#[derive(Debug)]
enum LambdaCallReturn<'gc> {
    /// Return the top of the stack
    Result(StackValue<'gc>),
    /// Suspend the call to this lambda
    Suspend,
    /// Suspend the call to this lambda, executing the given code in a new frame
    Call(StackValue<'gc>),
    /// Execute the given code on this frame
    TailCall(StackValue<'gc>),
    /// Lambda error
    Error(LambdaExecError<'gc>),
}

#[derive(thiserror::Error, Debug)]
#[error("cannot step while root scope has errored")]
pub struct StepError;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum Executable {
    /// The current scope has a pending lambda evaluation
    Callable(usize),
    /// The current scope is inside a macro
    Macro { sub: Box<Executable> },
    /// The current scope has no pending evaluations
    #[default]
    Not,
}

impl Executable {
    fn callable(&self) -> Option<usize> {
        match self {
            Executable::Callable(c) => Some(*c),
            _ => None,
        }
    }
}

impl<'gc> TreewalkExecutor<'gc> {
    fn new(
        mc: &Mutation<'gc>,
        interner: &mut Rodeo,
        environment: EnvironmentPtr<'gc>,
        code: Module,
        source_id: usize,
        range: TextRange,
    ) -> TreewalkExecutorPtr<'gc> {
        Gc::new(
            mc,
            RefLock::new(Self {
                stack: Vec::new(),
                root_scope: Scope {
                    executable: Executable::default(),
                    error: None,
                    label: None,
                    range: Some(range),
                    environment,
                    processed: 0,
                    next_datum: code
                        .datum()
                        .map(|d| convert_to_virtual(d, interner, mc, Some(source_id)))
                        .collect(),
                    source_id: Some(source_id),
                    bottom: None,
                    current_lambda: None,
                    current_macro: None,
                    execution: ScopeExecution::Empty,
                },
                rewrite_queue: VecDeque::new(),
                scope_stack: Vec::new(),
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

    /// root scope is always the first scope
    pub fn all_scopes(&self) -> impl Iterator<Item = &Scope<'gc>> {
        std::iter::once(&self.root_scope).chain(self.scope_stack.iter())
    }

    pub fn scope(&self) -> &Scope<'gc> {
        self.scope_stack.last().unwrap_or(&self.root_scope)
    }

    pub fn rewrite_queue(&self) -> impl Iterator<Item = &MacroInstruction<'gc>> {
        self.rewrite_queue.iter()
    }

    fn scope_mut(&mut self) -> &mut Scope<'gc> {
        self.scope_stack.last_mut().unwrap_or(&mut self.root_scope)
    }

    pub fn full_stack(&self) -> &[StackValue<'gc>] {
        &self.stack
    }

    pub fn stack(&self) -> &[StackValue<'gc>] {
        if let Some(bottom) = self.scope().bottom {
            &self.stack[bottom..]
        } else {
            &self.stack
        }
    }

    pub fn stack_mut(&mut self) -> &mut [StackValue<'gc>] {
        if let Some(bottom) = self.scope().bottom {
            &mut self.stack[bottom..]
        } else {
            &mut self.stack
        }
    }

    /// Get the value a scope want to call, if any
    fn get_scope_lambda(&self, scope: &Scope) -> Option<StackValue<'gc>> {
        let loc = scope
            .executable
            .callable()
            .map(|ex| self.stack.len().saturating_sub(ex).saturating_sub(1))
            .and_then(|l| {
                if let Some(bot) = scope.bottom {
                    (l >= bot).then_some(l)
                } else {
                    Some(l)
                }
            });
        loc.and_then(|loc| self.stack.get(loc).copied())
    }

    /// A scope is executable if the first element on its stack is a lambda
    fn scope_executable(&self, scope: &Scope) -> bool {
        self.get_scope_lambda(scope)
            .is_some_and(|v| v.borrow().value_type() == ValueType::Lambda)
    }

    pub fn can_continue(&self) -> bool {
        (
            // does any scope have a continuation or suspended lambda, or an instruction to execute
            self
                .scope_stack
                .iter()
                .any(|scope| !scope.next_datum.is_empty() || scope.current_lambda.is_some() || scope.execution.is_some()|| self.scope_executable(scope))
            // does the root scope have a continuation or suspended lambda, or an instruction to execute
            || !self.root_scope.next_datum.is_empty()
            || self.root_scope.current_lambda.is_some()
            || self.scope_executable(&self.root_scope)
        || self.root_scope.execution.is_some() || !self.rewrite_queue.is_empty())
            // is the *root* scope not erroring (if it is, we "return" an error)
            && self.root_scope.error.is_none()
    }

    pub fn current_env(&self) -> EnvironmentPtr<'gc> {
        self.scope().environment
    }

    fn next_inst(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) -> bool {
        // only attempt to pop the rewrite queue if we are *not* in a macro frame
        if !matches!(self.scope().executable, Executable::Macro { .. }) {
            if let Some(rewrite) = self.rewrite_queue.pop_front() {
                self.scope_mut().execution = ScopeExecution::Rewrite(rewrite);
            }
        }

        if !self.scope().next_datum.is_empty() {
            let mut next_inst = self.scope_mut().next_datum.pop_front();
            while next_inst.is_none() && !self.scope_stack.is_empty() {
                let ret_val = self.stack().last().copied();
                if let Some(bot) = self.scope().bottom {
                    self.stack.truncate(bot);
                } else {
                    unreachable!()
                }

                if let Some(v) = ret_val {
                    self.stack.push(v);
                }
                // pop scope
                self.scope_stack.pop();
                next_inst = self.scope_mut().next_datum.pop_front();
            }
            self.scope_mut().execution = if let Some(inst) = next_inst {
                ScopeExecution::Instruction(inst)
            } else {
                ScopeExecution::Empty
            };
        } else {
            return self.end_scope(ctx, fuel);
        }
        /*
        else if let Some(args) = self.scope().executable.callable() {
            // scope needs to execute lambda
            if !self.scope_executable(self.scope()) {
                self.raise_error(
                    self.build_error(
                        ctx.mutation,
                        SchemeErrorType::NonLambda(
                            self.get_scope_lambda(self.scope())
                                .map(|sv| *sv)
                                .unwrap_or(Value::Void.into_ptr(ctx.mutation))
                                .borrow()
                                .resolve_into(ctx.interner.clone(), ctx.null_ptr),
                        ),
                    ),
                    ctx,
                );
                return None;
            }
            return Some(self.call_frame(ctx, fuel, args));
        }*/
        false
    }

    /// Call the current frame
    #[must_use]
    fn call_frame(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel, args: usize) -> bool {
        let stack_len = self.stack.len().saturating_sub(args);
        eprintln!("{} {:?}", self.stack.len(), &self.stack);
        let arg_stack = self.stack.split_off(stack_len);
        eprintln!("{} {:?}", self.stack.len(), &self.stack);

        dbg!(&arg_stack);
        let maybe_lambda = self.stack.pop();
        let Some(lambda) = maybe_lambda.and_then(|sv| sv.borrow().as_lambda()) else {
            panic!("{:?} {:?}", maybe_lambda, arg_stack)
        };
        let lambda_access = lambda.borrow_mut(ctx.mutation);
        let mut call = lambda_access.call(arg_stack, self.scope().source_id, self.scope().range);
        fuel.consume(FuelCosts::CALL_COST);
        drop(lambda_access);

        let ret = self.call_lambda(lambda, ctx, &mut call, fuel, self.scope().error);
        let should_suspend = self.handle_lambda_return(ret, ctx, &mut call);
        if should_suspend {
            self.scope_mut().current_lambda = Some((call, lambda));
        } else {
            self.scope_mut().executable = Executable::Not;
        }
        should_suspend
    }

    /// Push a new scope using a given parent environment
    #[expect(clippy::too_many_arguments)]
    fn push_scope_with_env(
        &mut self,
        mc: &Mutation<'gc>,
        label: impl AsRef<str>,
        bottom: usize,
        inst: impl IntoIterator<Item = VirtualInstruction<'gc>>,
        parent: EnvironmentPtr<'gc>,
        (source_id, range): (Option<usize>, Option<TextRange>),
        executable: Executable,
    ) {
        self.scope_stack.push(Scope {
            error: None,
            environment: Gc::new(mc, RefLock::new(Environment::new(mc, Some(parent)))),
            next_datum: inst.into_iter().collect(),
            label: Some(Box::from(label.as_ref())),
            bottom: Some(bottom),
            current_lambda: None,
            current_macro: None,
            execution: ScopeExecution::Empty,
            processed: 0,
            executable,
            source_id,
            range,
        })
    }

    /// Push a new scope, using the current environment as the parent
    fn push_scope(
        &mut self,
        mc: &Mutation<'gc>,
        label: impl AsRef<str>,
        bottom: usize,
        inst: impl IntoIterator<Item = VirtualInstruction<'gc>>,
        code_loc: (Option<usize>, Option<TextRange>),
        executable: Executable,
    ) {
        let parent = self.scope().environment;
        self.push_scope_with_env(mc, label, bottom, inst, parent, code_loc, executable)
    }

    /// Pop the current scope.
    ///
    /// Returns the scope it popped (if any)
    fn pop_scope(&mut self) -> Option<Scope<'gc>> {
        self.scope_stack.pop()
    }

    /// Create a new stack value in the current scope
    pub fn current_scope_value(
        &self,
        mc: &Mutation<'gc>,
        value: impl IntoValue<'gc>,
    ) -> StackValue<'gc> {
        self.current_scope_value_ptr(mc, Gc::new(mc, RefLock::new(value.into_value(mc))))
    }

    /// Create a new stack value in the current scope
    pub fn current_scope_value_ptr(
        &self,
        mc: &Mutation<'gc>,
        value: ValuePtr<'gc>,
    ) -> StackValue<'gc> {
        StackValue {
            value,
            range: self.scope().range,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
            source_id: None,
        }
    }

    /// Push a value pointer to current scope
    fn push(&mut self, value: StackValue<'gc>) {
        // get the current scope and push to there
        self.stack.push(value);
    }

    /// Push a value pointer to current scope (copying information from a virtual instruction)
    fn push_from_inst(
        &mut self,
        mc: &Mutation<'gc>,
        value: impl IntoValue<'gc>,
        vi: &VirtualInstruction<'gc>,
    ) {
        let sv = StackValue {
            value: Gc::new(mc, RefLock::new(value.into_value(mc))),
            range: vi.range,
            touch_count: Gc::new(mc, RefLock::new(0)),
            chunk: Err(Gc::new(mc, RefLock::new(true))),
            source_id: vi.source_id,
        };
        self.stack.push(sv);
    }

    // Make a backtrace of frames until this one
    fn build_error(
        &self,
        mc: &Mutation<'gc>,
        error_type: SchemeErrorType<'gc>,
    ) -> SchemeErrorPtr<'gc> {
        let backtrace = std::iter::once(&self.root_scope)
            .chain(self.scope_stack.iter())
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

    /// Returns true if the macro want to resuspend execution
    fn call_macro(&mut self, mcr: MacroPtr<'gc>, ctx: &Context<'gc>, fuel: &mut Fuel) -> bool {
        let mut macro_access = mcr.borrow_mut(ctx.mutation);
        match macro_access.rewrite(ctx, self, fuel) {
            Ok(res) => match res {
                MacroReturn::Waiting => true,
                MacroReturn::Return { inst } => {
                    // self.push(self.current_scope_value(ctx.mutation, ret));
                    self.rewrite_queue.extend(inst);
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

    /// Returns if the lambda should be suspended
    fn call_lambda(
        &mut self,
        lambda: LambdaPtr<'gc>,
        ctx: &Context<'gc>,
        call: &mut LambdaCall<'gc>,
        fuel: &mut Fuel,
        error: Option<SchemeErrorPtr<'gc>>,
    ) -> LambdaCallReturn<'gc> {
        let mut lambda_access = lambda.borrow_mut(ctx.mutation);
        let ret = if let Some(error) = error {
            // Call the lambda in an erroring context
            lambda_access.execute_erroring(error, ctx, call, self, fuel)
        } else {
            lambda_access.execute(ctx, call, self, fuel)
        };

        match ret {
            Ok(ret) => match ret {
                ProcedureReturn::Return(ret) => LambdaCallReturn::Result(ret),
                ProcedureReturn::Suspend => LambdaCallReturn::Suspend,
                ProcedureReturn::Call { code, is_tail } => {
                    if is_tail {
                        LambdaCallReturn::TailCall(code)
                    } else {
                        LambdaCallReturn::Call(code)
                    }
                }
            },
            Err(e) => LambdaCallReturn::Error(e),
        }
    }

    fn raise_error(&mut self, err_ptr: SchemeErrorPtr<'gc>, ctx: &Context<'gc>) {
        // combine the generated error pointer with any existing errors
        let err_ptr = if let Some(err) = &self.scope().error {
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

        self.scope_mut().error = Some(err_ptr);
    }

    #[must_use]
    fn handle_lambda_return(
        &mut self,
        ret: LambdaCallReturn<'gc>,
        ctx: &Context<'gc>,
        call: &mut LambdaCall<'gc>,
    ) -> bool {
        match dbg!(ret) {
            LambdaCallReturn::Result(ret) => {
                self.stack.push(ret);
                assert!(
                    matches!(self.scope().executable, Executable::Callable(_)),
                    "{} {:?}",
                    self.stack.len(),
                    self.stack,
                );
                // Pop the scope, and if root, just set exec to not
                if self.pop_scope().is_none() {
                    self.scope_mut().executable = Executable::Not;
                }
                false
            }
            LambdaCallReturn::Suspend => true,
            LambdaCallReturn::Call(value) => {
                // Suspend the lambda and create new scope
                let code = match (ctx.mutation, value).try_into() {
                    Ok(code) => code,
                    Err(_err) => {
                        let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::BadList);
                        self.raise_error(err_ptr, ctx);
                        return false;
                    }
                };

                self.scope_mut().execution = ScopeExecution::Instruction(code);
                self.scope_mut().executable = Executable::default();
                // suspend the lambda too
                true
            }
            LambdaCallReturn::TailCall(value) => {
                // Bash the current scope, replacing it with the given code
                let code = match (ctx.mutation, value).try_into() {
                    Ok(code) => code,
                    Err(_err) => {
                        let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::BadList);
                        self.raise_error(err_ptr, ctx);
                        return false;
                    }
                };

                self.scope_mut().executable = Executable::default();
                // If there are rewrites, make this the next datum, otherwise,
                // do it now
                if self.rewrite_queue.is_empty() {
                    self.scope_mut().execution = ScopeExecution::Instruction(code);
                } else {
                    self.scope_mut().next_datum.push_front(code);
                }
                false
            }
            LambdaCallReturn::Error(le) => {
                match le {
                    LambdaExecError::ProcedureError(err) => {
                        let err_ptr = match err {
                            ProcedureError::Scheme(serr) => {
                                // this error is to be propagated
                                serr
                            }
                            ProcedureError::Value(val) => {
                                // Any *non-propagated* errors become the error of the current scope

                                // Resolve the value to get more useful (and nicer) text in the error
                                let val = val
                                    .borrow()
                                    .resolve_into(ctx.interner.clone(), ctx.null_ptr);
                                let err_type = SchemeErrorType::Raise(val);
                                // make a Scheme error using the generating the current backtrace
                                // and the currrent execution's range
                                self.build_error(ctx.mutation, err_type)
                            }
                            ProcedureError::General(gen) => {
                                // Any *non-propagated* errors become the error of the current scope
                                let err_type = SchemeErrorType::Rust(gen);
                                // make a Scheme error using the generating the current backtrace
                                // and the currrent execution's range
                                self.build_error(ctx.mutation, err_type)
                            }
                        };

                        self.raise_error(err_ptr, ctx);
                    }
                    LambdaExecError::TypecheckFailure(err) => {
                        // Create a new error on this frame using this error
                        self.raise_error(
                            self.build_error(ctx.mutation, SchemeErrorType::Typecheck(err)),
                            ctx,
                        )
                    }
                    LambdaExecError::LambdaMismatch { id, call_id } => {
                        panic!("ICE: mismatched lambda call {call_id:p} for lambda {id:p}")
                    }
                };
                false
            }
        }
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

    /// Handle macro rewrite rules
    fn handle_rewrite(&mut self, ctx: &Context<'gc>, rewrite: MacroInstruction<'gc>) {
        match rewrite {
            MacroInstruction::Quote(sv) => {
                // just push the value to stack
                self.stack.push(sv);
            }
            MacroInstruction::Evaluate(sv) => match (ctx.mutation, sv).try_into() {
                Ok(vi) => {
                    assert!(matches!(self.scope().execution, ScopeExecution::Empty));
                    self.scope_mut().execution = ScopeExecution::Instruction(vi);
                    // mark this as "inside a macro"
                    let exec = self.scope_mut().executable.clone();
                    self.scope_mut().executable = Executable::Macro {
                        sub: Box::new(exec),
                    };
                }
                Err(()) => {
                    let err_ptr = self.build_error(
                        ctx.mutation,
                        SchemeErrorType::BadEval(
                            sv.borrow().resolve_into(ctx.interner.clone(), ctx.null_ptr),
                        ),
                    );
                    self.raise_error(err_ptr, ctx);
                }
            },
            // a define is only ready if there are no pending evaluations
            MacroInstruction::Define { name } if self.scope().next_datum.is_empty() => {
                // define a name in the environment by popping the top of the stack
                let Some(value) = dbg!(self.stack.pop()) else {
                    let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::NullDefine);
                    self.raise_error(err_ptr, ctx);
                    return;
                };

                if self
                    .current_env()
                    .borrow_mut(ctx.mutation)
                    .define(ctx.mutation, name, value, false)
                    .is_err()
                {
                    let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::FrozenDefine);
                    self.raise_error(err_ptr, ctx);
                };

                // put value back
                self.stack.push(
                    self.current_scope_value_ptr(ctx.mutation, Value::Void.into_ptr(ctx.mutation)),
                );
            }
            mi @ MacroInstruction::Define { .. } => {
                // retry the frame as a macro frame
                self.rewrite_queue.push_front(mi);
            }
            MacroInstruction::SetEnvironment(env) => {
                self.scope_mut().environment = env;
            }
            // a set! is only ready if there are no pending evaluations
            MacroInstruction::SetBang { name } if self.scope().executable == Executable::Not => {
                // define a name in the environment by popping the top of the stack
                let Some(value) = self.stack.pop() else {
                    let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::NullDefine);
                    self.raise_error(err_ptr, ctx);
                    return;
                };
                if let Err(err) = self.current_env().borrow_mut(ctx.mutation).rebind(
                    ctx.mutation,
                    name,
                    value,
                    &ctx.interner,
                ) {
                    let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::SetBang(err));
                    self.raise_error(err_ptr, ctx);
                };
            }
            mi @ MacroInstruction::SetBang { .. } => {
                // retry the frame later
                self.rewrite_queue.push_front(mi);
            }
            MacroInstruction::CallLambda { args: _ } => {
                // TODO This might be buggy...
                // but I'll fix it when I need it
                todo!("this make a new scope def. bring back the old code and fix it")
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
            .and_then(|sym| self.current_env().borrow().get_macro(sym))
            .map(|mcrb| mcrb.get())
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

            // move the frame's bottom during macro evaluation
            // so it can tell how many arguments it was given
            // FIXME add tests so that we can flip this logic
            // (instead of holding onto *our* bottom, hold onto
            // the bottom we replaced. it should be equiv, but
            // I want tests or it's not)
            let macro_bottom = Some(self.stack.len());
            let old_bottom = std::mem::replace(&mut self.scope_mut().bottom, macro_bottom);
            self.scope_mut().bottom = macro_bottom;
            // push values to stack *as-is*
            self.stack
                .extend(body.iter().cloned().map(|vi| vi.into_value(ctx)));

            if self.call_macro(mcr, ctx, fuel) {
                self.scope_mut().current_macro = Some((mcr, macro_bottom));
            }
            self.scope_mut().bottom = old_bottom;
            return Some(true);
        }

        Some(false)
    }

    fn end_scope(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) -> bool {
        // pop the scope, propagating any errors
        let Some(done) = dbg!(self.pop_scope()) else {
            // no propagation, just ending a call
            match self.scope().executable {
                Executable::Macro { .. } => unreachable!(),
                Executable::Not => {}
                Executable::Callable(args) => {
                    dbg!(&self.stack);
                    dbg!(args);
                    return self.call_frame(ctx, fuel, args);
                }
            }
            return false;
        };

        match self.scope().executable {
            Executable::Macro { .. } => {}
            Executable::Not => {
                panic!("{} {:?}", self.stack.len(), done.bottom);
            }
            Executable::Callable(args) => {
                // execute the frame
                self.scope_stack.push(done);

                return self.call_frame(ctx, fuel, args);
            }
        }

        // raise any errors encountered in the done scope
        if let Some(err_ptr) = done.error {
            self.raise_error(err_ptr, ctx);
        }

        false
    }

    pub fn step(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) -> Result<(), StepError> {
        // Don't step if we are at root
        // TODO Make explicit with an error type?
        if self.root_scope.error.is_some() {
            return Err(StepError);
        }

        fuel.clear_interrupt();

        while fuel.should_continue() {
            if let Some((mcr, bottom)) = self.scope_mut().current_macro.take() {
                let old_bottom = std::mem::replace(&mut self.scope_mut().bottom, bottom);
                if self.call_macro(mcr, ctx, fuel) {
                    // resuspend execution, and disrupt fuel,
                    // because we can't continue execution until
                    // this finishes (returns false)
                    self.scope_mut().current_macro = Some((mcr, old_bottom));
                    self.scope_mut().bottom = old_bottom;
                    fuel.interrupt();
                    continue;
                }
                self.scope_mut().bottom = old_bottom;
            }

            // If the scope is processing a lambda, continue to do so
            if let Some((mut call, lambda)) = self.scope_mut().current_lambda.take() {
                let ret = self.call_lambda(lambda, ctx, &mut call, fuel, self.scope().error);
                if self.handle_lambda_return(ret, ctx, &mut call) {
                    self.scope_mut().current_lambda = Some((call, lambda));
                    continue;
                }
            }

            if self.next_inst(ctx, fuel) {
                continue;
            }

            // if we aren't doing anything, figure out what to do
            if self.scope().execution.is_none() {
                // If this scope is in an error state, pop it, and if it is the root scope
                // stop execution
                if let Some(err) = self.scope_mut().error.take() {
                    if self.pop_scope().is_none() {
                        // put it back
                        self.scope_mut().error = Some(err);
                    } else {
                        let done = self.pop_scope();
                        if let Some(err_ptr) = done.and_then(|done| done.error) {
                            self.raise_error(err_ptr, ctx)
                        }
                    }
                    continue;
                }
            }

            match self.scope_mut().execution.take() {
                ScopeExecution::Empty => {
                    if self.scope_stack.is_empty() {
                        break;
                    }

                    if self.end_scope(ctx, fuel) {
                        continue;
                    }
                }
                ScopeExecution::Rewrite(rew) => self.handle_rewrite(ctx, rew),
                ScopeExecution::Instruction(exec) => {
                    *exec.touch_count.borrow_mut(ctx.mutation) += 1;
                    match &exec.payload {
                        VirtualInstructionPayload::Jit { .. } => {
                            unreachable!("jit isn't built yet")
                        }
                        VirtualInstructionPayload::Datum { datum, can_jit: _ } => match datum {
                            VirtualInstructionDatum::EmptyList => {
                                let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::Null);
                                self.raise_error(err_ptr, ctx);
                            }
                            VirtualInstructionDatum::Number(num) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                self.push_from_inst(ctx.mutation, *num, &exec);
                            }
                            VirtualInstructionDatum::Bool(b) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                self.push_from_inst(ctx.mutation, *b, &exec);
                            }
                            VirtualInstructionDatum::String(s) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                self.push_from_inst(ctx.mutation, s.clone(), &exec);
                            }
                            VirtualInstructionDatum::Character(c) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                self.push_from_inst(ctx.mutation, *c, &exec);
                            }

                            VirtualInstructionDatum::Bytevector(bv) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                self.push_from_inst(
                                    ctx.mutation,
                                    Value::Bytevector(
                                        Gc::new(ctx.mutation, RefLock::new(bv.clone())).into(),
                                    ),
                                    &exec,
                                );
                            }
                            VirtualInstructionDatum::Symbol(sym) => {
                                fuel.consume(FuelCosts::ENV_COST);
                                let current_env = self.current_env();
                                let Some(binding) = current_env.borrow().get(*sym) else {
                                    let err_ptr = self.build_error(
                                        ctx.mutation,
                                        SchemeErrorType::EnvLoad(Box::from(
                                            ctx.interner.resolve(&sym.0),
                                        )),
                                    );
                                    self.raise_error(err_ptr, ctx);
                                    continue;
                                };

                                let sv = *binding.get().borrow();
                                match *sv.borrow() {
                                    Value::Undefined => {
                                        // act as if this value is *not* defined
                                        let err_ptr = self.build_error(
                                            ctx.mutation,
                                            SchemeErrorType::EnvLoad(Box::from(
                                                ctx.interner.resolve(&sym.0),
                                            )),
                                        );
                                        self.raise_error(err_ptr, ctx);
                                    }
                                    _ => self.push(sv),
                                }
                            }
                            // Showtime
                            //
                            // Lists handle their first subdatum specially when executed
                            // If the first item is:
                            //   - a symbol
                            //   - in the current env, refering to a macro
                            // then it is a macro, otherwise it's just a normal symbol guys
                            // we also change how we execute depending on the Executable

                            // it is safe to tail-call on this frame, because we aren't doing anything
                            // suspendable in this scope, and we have nothing else to use this frame for.
                            VirtualInstructionDatum::List { head, body, dot }
                                if !matches!(
                                    self.scope().executable,
                                    Executable::Callable(_) | Executable::Macro { .. }
                                ) =>
                            {
                                if let Some(true) | None = self.check_list_for_macro(
                                    ctx,
                                    fuel,
                                    &exec,
                                    head,
                                    body,
                                    dot.as_ref().map(Box::as_ref),
                                ) {
                                    continue;
                                }

                                let label = format!("{}", head.payload.display(&ctx.interner));
                                //     cscope.label = Some(Box::from(label.as_str()));
                                //     cscope.range = exec.range;
                                //     cscope.source_id = exec.source_id;
                                //     cscope.executable.handle_sub(body.len());
                                //     cscope.next_datum = std::iter::once(head.as_ref().clone())
                                //         .chain(body.iter().cloned())
                                //         .collect();
                                let cscope = self.scope_mut();

                                cscope.label = Some(Box::from(label.as_str()));
                                cscope.range = exec.range;
                                cscope.source_id = exec.source_id;
                                if let Some(sym) = head.payload.as_symbol() {
                                    let Some(binding) = self.current_env().borrow().get(sym) else {
                                        let err_ptr = self.build_error(
                                            ctx.mutation,
                                            SchemeErrorType::EnvLoad(Box::from(
                                                ctx.interner.resolve(&sym.0),
                                            )),
                                        );
                                        self.raise_error(err_ptr, ctx);
                                        continue;
                                    };

                                    self.stack.push(*binding.get().borrow());

                                    self.scope_mut().executable = Executable::Callable(body.len());
                                    self.scope_mut().next_datum = body.clone().into();
                                } else {
                                    self.scope_mut().executable =
                                        Executable::Callable(body.len() + 1);
                                    self.scope_mut().next_datum =
                                        std::iter::once(head.as_ref().clone())
                                            .chain(body.iter().cloned())
                                            .collect();
                                }
                                // if self.scope().executable.is_callable() {
                                // we aren't in tail position, so we have to keep the original scope around
                                // TODO Switch up, if head is a symbol this code is correct,
                                // but *not* if head is a list...

                                // } else {
                                //     let cscope = self.scope_mut();
                                //     cscope.label = Some(Box::from(label.as_str()));
                                //     cscope.range = exec.range;
                                //     cscope.source_id = exec.source_id;
                                //     cscope.executable.handle_sub(body.len());
                                //     cscope.next_datum = std::iter::once(head.as_ref().clone())
                                //         .chain(body.iter().cloned())
                                //         .collect();
                                // }
                            }
                            // These frames cannot be tail-called from, as a suspended lambda is present
                            VirtualInstructionDatum::List { head, body, dot } => {
                                if let Some(true) | None = self.check_list_for_macro(
                                    ctx,
                                    fuel,
                                    &exec,
                                    head,
                                    body,
                                    dot.as_ref().map(Box::as_ref),
                                ) {
                                    continue;
                                }

                                let label = format!("{}", head.payload.display(&ctx.interner));

                                let Executable::Callable(args) = self.scope().executable else {
                                    unreachable!()
                                };

                                panic!(
                                    "recalc scopes: {:#?} {} {} {} {:#?}",
                                    self.scope(),
                                    args,
                                    body.len(),
                                    self.stack.len(),
                                    self.stack
                                );

                                if let Some(sym) = head.payload.as_symbol() {
                                    let Some(binding) = self.current_env().borrow().get(sym) else {
                                        let err_ptr = self.build_error(
                                            ctx.mutation,
                                            SchemeErrorType::EnvLoad(Box::from(
                                                ctx.interner.resolve(&sym.0),
                                            )),
                                        );
                                        self.raise_error(err_ptr, ctx);
                                        continue;
                                    };

                                    let bottom = self.stack.len() + 1;
                                    self.stack.push(*binding.get().borrow());

                                    // panic!("{bottom} {}", body.len());

                                    self.push_scope(
                                        ctx.mutation,
                                        label,
                                        bottom,
                                        // put all the arguments as datum instructions
                                        body.clone(),
                                        (
                                            exec.source_id.or(self.scope().source_id),
                                            exec.range.or(self.scope().range),
                                        ),
                                        Executable::Callable(body.len()),
                                    );
                                } else {
                                    // head is a list, we need *2* scopes one to evaluate head, the
                                    // other to evaluate list
                                    self.push_scope(
                                        ctx.mutation,
                                        label,
                                        self.stack.len() + 1,
                                        // put all the arguments as datum instructions
                                        std::iter::once(head.as_ref().clone())
                                            .chain(body.iter().cloned()),
                                        (
                                            exec.source_id.or(self.scope().source_id),
                                            exec.range.or(self.scope().range),
                                        ),
                                        Executable::Callable(body.len() + 1),
                                    );
                                }
                            }
                            VirtualInstructionDatum::Labeled {
                                is_circular: true, ..
                            } => {
                                todo!("label error")
                            }
                            VirtualInstructionDatum::Labeled { .. } => todo!(),
                        },
                    }
                }
            }
        }

        Ok(())
    }
}
