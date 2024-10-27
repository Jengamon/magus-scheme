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
    value::{ConsCell, Value, ValuePtr, ValueType},
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
        Context {
            mutation: mc,
            null_ptr,
            interner: self.interner.borrow_mut(mc),
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
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TreewalkExecutor<'gc> {
    rewrite_queue: VecDeque<MacroInstruction<'gc>>,
    stack: Vec<StackValue<'gc>>,
    root_scope: Scope<'gc>,
    scope_stack: Vec<Scope<'gc>>,
}
pub type TreewalkExecutorPtr<'gc> = Gc<'gc, RefLock<TreewalkExecutor<'gc>>>;

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

    execution: ScopeExecution<'gc>,
    #[collect(require_static)]
    range: Option<TextRange>,
    source_id: Option<usize>,

    current_macro: Option<MacroPtr<'gc>>,
    current_lambda: Option<(LambdaCall<'gc>, LambdaPtr<'gc>)>,
    error: Option<SchemeErrorPtr<'gc>>,
    processed: usize,
    next_datum: VecDeque<VirtualInstruction<'gc>>,
    bottom: Option<usize>,
    label: Option<Box<str>>,
    #[collect(require_static)]
    executable: Executable,
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
    pub fn macro_call(&self) -> Option<MacroPtr<'gc>> {
        self.current_macro
    }

    #[inline]
    fn code_loc(&self) -> (Option<usize>, Option<TextRange>) {
        (self.source_id, self.range)
    }
}

enum LambdaCallReturn<'gc> {
    /// Return the top of the stack
    Result,
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
    fn take(&mut self) -> Option<Executable> {
        match self {
            Executable::Macro { sub } => Some(std::mem::replace(sub.as_mut(), Executable::Not)),
            _ => None,
        }
    }
    /// Is this frame considered executable?
    fn is_callable(&self) -> bool {
        match self {
            Executable::Callable(_) => true,
            Executable::Macro { sub } => sub.is_callable(),
            _ => false,
        }
    }

    /// If this is a macro frame, mark the sub frame type,
    /// otherwise, bash the callable
    fn handle_sub(&mut self, args: usize) {
        match self {
            Executable::Not => {
                *self = Executable::Callable(args);
            }
            Executable::Callable(oargs) => *oargs = args,
            Executable::Macro { sub } => sub.handle_sub(args),
        }
    }

    fn callable(&self) -> Option<usize> {
        match self {
            Executable::Callable(c) => Some(*c),
            Executable::Macro { sub } => sub.callable(),
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

    /// A scope is executable if the first element on its stack is a lambda
    fn scope_executable(&self, scope: &Scope) -> bool {
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
        loc.is_some_and(|loc| {
            self.stack
                .get(loc)
                .is_some_and(|v| v.borrow().value_type() == ValueType::Lambda)
        })
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

    fn next_inst(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) {
        // only attempt to pop the rewrite queue if we are *not* in a macro frame
        if !matches!(self.scope().executable, Executable::Macro { .. }) {
            if let Some(rewrite) = self.rewrite_queue.pop_front() {
                self.scope_mut().execution = ScopeExecution::Rewrite(rewrite);
                return;
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
        } else if let Some(args) = self.scope().executable.callable() {
            assert!(self.scope_executable(self.scope()));
            self.call_frame(ctx, fuel, args);
        }
    }

    /// Call the current frame
    fn call_frame(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel, args: usize) {
        let stack_len = self.stack.len().saturating_sub(args);
        let arg_stack = self.stack.split_off(stack_len);

        let lambda = self
            .stack
            .pop()
            .and_then(|sv| sv.borrow().as_lambda())
            .expect("bottom of stack should be a lambda");
        let lambda_access = lambda.borrow_mut(ctx.mutation);
        let mut call = lambda_access.call(arg_stack, self.scope().source_id, self.scope().range);
        fuel.consume(FuelCosts::CALL_COST);
        drop(lambda_access);

        let ret = self.call_lambda(lambda, ctx.mutation, &mut call, fuel, self.scope().error);
        self.handle_lambda_return(ret, ctx, lambda, call);
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
    pub fn current_scope(&self, mc: &Mutation<'gc>, value: ValuePtr<'gc>) -> StackValue<'gc> {
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
                MacroReturn::Suspend => true,
                MacroReturn::Return { ret, inst } => {
                    self.push(self.current_scope(ctx.mutation, ret));
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
        mc: &Mutation<'gc>,
        call: &mut LambdaCall<'gc>,
        fuel: &mut Fuel,
        error: Option<SchemeErrorPtr<'gc>>,
    ) -> LambdaCallReturn<'gc> {
        let mut lambda_access = lambda.borrow_mut(mc);
        let ret = if let Some(error) = error {
            // Call the lambda in an erroring context
            lambda_access.execute_erroring(error, mc, call, self, fuel)
        } else {
            lambda_access.execute(mc, call, self, fuel)
        };

        match ret {
            Ok(ret) => match ret {
                ProcedureReturn::Return => LambdaCallReturn::Result,
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
        let range = self.scope().range;

        // combine the generated error pointer with any existing errors
        let err_ptr = if let Some(err) = &self.scope().error {
            let mut errors: Vec<_> = match &err.error_type {
                SchemeErrorType::Compound(errors) => errors.to_vec(),
                _ => Vec::new(),
            };

            if !errors.is_empty() {
                errors.push(*err);
            }

            errors.push(err_ptr);

            // TODO Create a new compound error, with this scope included in the backtrace,
            // then pop this scope
            Gc::new(
                ctx.mutation,
                SchemeError {
                    backtrace: std::iter::once(StackFrame {
                        scope_label: self.scope().label.clone(),
                        // This would be the range from the current VirtualInstruction
                        range,
                        source_id: self.scope().source_id,
                    })
                    .chain(err.backtrace.iter().cloned())
                    .collect(),
                    error_type: SchemeErrorType::Compound(errors),
                },
            )
        } else {
            err_ptr
        };

        self.scope_mut().error = Some(err_ptr);
    }

    fn handle_lambda_return(
        &mut self,
        ret: LambdaCallReturn<'gc>,
        ctx: &Context<'gc>,
        lambda: LambdaPtr<'gc>,
        mut call: LambdaCall<'gc>,
    ) {
        match ret {
            LambdaCallReturn::Result => {
                if let Some(val) = call.stack.pop() {
                    self.stack.push(val);
                } else {
                    // error, b/c expressions *have* to return a value
                    self.raise_error(
                        self.build_error(ctx.mutation, SchemeErrorType::LambdaNoReturn),
                        ctx,
                    );
                    return;
                }

                if self.scope_stack.is_empty() {
                    self.scope_mut().executable = Executable::default();
                } else {
                    self.pop_scope();
                }
            }
            LambdaCallReturn::Suspend => {
                self.scope_mut().current_lambda = Some((call, lambda));
            }
            LambdaCallReturn::Call(value) => {
                // Create a new scope to execute the given code
                let code = match (ctx.mutation, value).try_into() {
                    Ok(code) => code,
                    Err(_err) => {
                        let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::Dot);
                        self.raise_error(err_ptr, ctx);
                        return;
                    }
                };

                self.push_scope(
                    ctx.mutation,
                    format!(
                        "{} eval {}",
                        self.scope().label(),
                        value
                            .borrow()
                            .resolve_into(ctx.interner.clone(), ctx.null_ptr)
                    ),
                    self.stack.len(),
                    std::iter::once(code),
                    // if an instruction/value doesn't have a range, it is from outside
                    // this source file, so we use the range of the scope to represent that
                    (
                        value.source_id.or(self.scope().source_id),
                        value.range.or(self.scope().range),
                    ),
                    Executable::default(),
                );
            }
            LambdaCallReturn::TailCall(value) => {
                // Bash the current scope, replacing it with the given code
                let code = match (ctx.mutation, value).try_into() {
                    Ok(code) => code,
                    Err(_err) => {
                        let err_ptr = self.build_error(ctx.mutation, SchemeErrorType::Dot);
                        self.raise_error(err_ptr, ctx);
                        return;
                    }
                };

                // we "lazy", so use the code we have
                self.push_scope(
                    ctx.mutation,
                    format!(
                        "{} eval {}",
                        self.scope().label(),
                        value
                            .borrow()
                            .resolve_into(ctx.interner.clone(), ctx.null_ptr)
                    ),
                    self.stack.len(),
                    std::iter::once(code),
                    (
                        value.source_id.or(self.scope().source_id),
                        value.range.or(self.scope().range),
                    ),
                    Executable::default(),
                );
                let new_scope = self.pop_scope().unwrap();
                if self.pop_scope().is_none() {
                    // bash the root scope
                    self.root_scope = new_scope;
                } else {
                    self.scope_stack.push(new_scope);
                }
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
                }
            }
        }
    }

    /// Handle macro rewrite rules
    fn handle_rewrite(&mut self, ctx: &Context<'gc>, rewrite: MacroInstruction<'gc>) {
        match rewrite {
            MacroInstruction::Evaluate(sv) => match (ctx.mutation, sv).try_into() {
                Ok(vi) => {
                    self.push_scope(
                        ctx.mutation,
                        format!("{} macro eval", self.scope().label()),
                        self.stack.len(),
                        [vi],
                        self.scope().code_loc(),
                        Executable::Macro {
                            sub: Box::new(Executable::default()),
                        },
                    );
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
            MacroInstruction::Define { name } if self.scope().executable == Executable::Not => {
                // define a name in the environment by popping the top of the stack
                let Some(value) = self.stack.pop() else {
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
            }
            mi @ MacroInstruction::Define { .. } => {
                // retry the frame as a macro frame
                self.scope_mut().executable = Executable::Macro {
                    sub: Box::new(self.scope().executable.clone()),
                };
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
                // retry the frame as a macro frame
                self.scope_mut().executable = Executable::Macro {
                    sub: Box::new(self.scope().executable.clone()),
                };
                self.rewrite_queue.push_front(mi);
            }
            MacroInstruction::CallFunction { args } => {
                // call a function with at most `args` arguments
                // TODO do we need an exact mode
                let macro_label = format!("{} macro", self.scope().label());
                let new_bottom = self.stack.len().saturating_sub(args);
                if self.scope().executable == Executable::Not {
                    // if this scope is not executable, just make it executable
                    let scope = self.scope_mut();
                    scope.executable = Executable::Callable(args);
                    scope.label = Some(Box::from(macro_label.as_str()));
                    scope.bottom = if new_bottom > 0 {
                        Some(new_bottom)
                    } else {
                        None
                    };
                    // push the current exec back as an instruction
                    match self.scope().execution {
                        ScopeExecution::Empty => {}
                        _ => unreachable!("codepath assumed {:?} == Empty", self.scope().execution),
                    }
                } else {
                    // push a *new* scope, and make that frame executable
                    self.push_scope(
                        ctx.mutation,
                        macro_label,
                        new_bottom,
                        std::iter::empty(),
                        (self.scope().source_id, self.scope().range),
                        Executable::Callable(args),
                    );
                }
            }
        }
    }

    pub fn step(&mut self, ctx: &Context<'gc>, fuel: &mut Fuel) -> Result<(), StepError> {
        // Don't step if we are at root
        // TODO Make explicit with an error type?
        if self.root_scope.error.is_some() {
            return Err(StepError);
        }

        while fuel.should_continue() {
            if let Some(mcr) = self.scope_mut().current_macro.take() {
                if self.call_macro(mcr, ctx, fuel) {
                    // resuspend execution
                    self.scope_mut().current_macro = Some(mcr);
                    continue;
                }
            }

            // If the scope is processing a lambda, continue to do so
            if let Some((mut call, lambda)) = self.scope_mut().current_lambda.take() {
                let ret =
                    self.call_lambda(lambda, ctx.mutation, &mut call, fuel, self.scope().error);
                self.handle_lambda_return(ret, ctx, lambda, call);
            }

            // If this scope is in an error state, pop it, and if it is the root scope
            // stop execution
            if let Some(err) = self.scope_mut().error.take() {
                if self.pop_scope().is_none() {
                    // put it back
                    self.scope_mut().error = Some(err);
                    break;
                } else {
                    self.scope_mut().error = Some(err);
                }
            }

            // if there's nothing in the instruction slot, but there is a continuation, load it in
            if self.scope().execution.is_none() {
                self.next_inst(ctx, fuel);
            }

            if self.scope().execution.is_none()
                && matches!(self.scope().executable, Executable::Macro { .. })
            {
                let Some(sub) = self.scope_mut().executable.take() else {
                    unreachable!()
                };

                // if the sub is a macro, panic
                match sub {
                    Executable::Not => {
                        // Pop this scope, returning the last value
                        let Some(ret) = self.stack.pop() else {
                            self.raise_error(
                                self.build_error(ctx.mutation, SchemeErrorType::LambdaNoReturn),
                                ctx,
                            );
                            self.scope_mut().executable = Executable::default();
                            continue;
                        };
                        // raze the stack
                        self.stack.truncate(self.scope().bottom.unwrap_or(0));
                        self.stack.push(ret);

                        // pop the stack, or reset the executable
                        if self.scope_stack.is_empty() {
                            self.scope_mut().executable = Executable::default();
                        } else {
                            self.pop_scope();
                        }
                    }
                    Executable::Macro { .. } => panic!("ICE macro in macro"),
                    Executable::Callable(args) => {
                        // call the current frame
                        self.scope_mut().executable = Executable::Callable(args);
                    }
                }
            }

            match self.scope_mut().execution.take() {
                ScopeExecution::Empty => break,
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
                            VirtualInstructionDatum::List { head, body, dot } => {
                                // Showtime
                                //
                                // Lists handle their first subdatum specially when executed
                                // If the first item is:
                                //   - a symbol
                                //   - in the current env, refering to a macro
                                // then it is a macro, otherwise it's just a normal symbol guys
                                if dot.is_some() {
                                    self.raise_error(
                                        self.build_error(ctx.mutation, SchemeErrorType::Dot),
                                        ctx,
                                    );
                                    continue;
                                }

                                let label = format!("{}", head.payload.display(&ctx.interner));

                                if let Some(mcr) = head
                                    .payload
                                    .as_symbol()
                                    .and_then(|sym| self.current_env().borrow().get_macro(sym))
                                    .map(|mcrb| mcrb.get())
                                {
                                    let do_macro =
                                        if let Some(fc) = mcr.borrow().is_form(self, &exec) {
                                            if let Err(err) = fc {
                                                self.build_error(
                                                    ctx.mutation,
                                                    SchemeErrorType::MacroForm(err),
                                                );
                                                continue;
                                            }
                                            true
                                        } else {
                                            false
                                        };

                                    if do_macro {
                                        // push values to stack *as-is*
                                        self.stack.extend(
                                            body.iter().cloned().map(|vi| {
                                                vi.into_value(ctx.mutation, ctx.null_ptr)
                                            }),
                                        );
                                        if self.call_macro(mcr, ctx, fuel) {
                                            self.scope_mut().current_macro = Some(mcr);
                                        }
                                        continue;
                                    }
                                }

                                if !self.scope().next_datum.is_empty()
                                    || self.scope().executable.is_callable()
                                {
                                    // we aren't in tail position, so we have to keep the original scope around
                                    self.push_scope(
                                        ctx.mutation,
                                        label,
                                        self.stack.len(),
                                        // put all the arguments as datum instructions
                                        std::iter::once(head.as_ref().clone())
                                            .chain(body.iter().cloned()),
                                        (
                                            exec.source_id.or(self.scope().source_id),
                                            exec.range.or(self.scope().range),
                                        ),
                                        Executable::Callable(body.len()),
                                    );
                                } else {
                                    let cscope = self.scope_mut();
                                    cscope.label = Some(Box::from(label.as_str()));
                                    cscope.range = exec.range;
                                    cscope.source_id = exec.source_id;
                                    cscope.executable.handle_sub(body.len());
                                    cscope.next_datum = std::iter::once(head.as_ref().clone())
                                        .chain(body.iter().cloned())
                                        .collect();
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
