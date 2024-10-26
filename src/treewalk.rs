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

use crate::{
    environment::{Environment, EnvironmentPtr},
    runtime::{
        convert::IntoValue,
        error::{SchemeError, SchemeErrorPtr, SchemeErrorType, StackFrame},
        lambda::{LambdaCall, LambdaExecError, LambdaPtr, ProcedureError, ProcedureReturn},
        EnsureNullVisitor, FuelCosts,
    },
    value::{self, ConsCell, Value, ValuePtr, ValueVisitor as _},
    Boolean, Character, ContainsDatum, Datum, DatumKind, ExactReal, Fuel, GAstNode, List, Module,
    Number, SchemeNumber, StringToken, Symbol,
};

pub mod scheme;

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
    state: TreewalkState<'gc>,

    pub scheme: SchemeStd<'gc>,
}

impl<'gc> TreewalkArena<'gc> {
    // Convert ConsCell [ None None ] to root null_val
    pub fn ensure_null(&self, mc: &Mutation<'gc>, value_ptr: ValuePtr<'gc>) {
        let mut ensure_null = EnsureNullVisitor {
            mutation: mc,
            null: &self.null_val.borrow(),
        };
        ensure_null.visit_value(value_ptr);
    }
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

    pub fn new_executor<F>(&mut self, code: Module, env_init: F) -> ExecutorKey
    where
        F: for<'gc> FnOnce(&'gc Mutation<'gc>, &TreewalkArena<'gc>, EnvironmentPtr<'gc>),
    {
        self.collect_executors();

        let inner = self.arena.mutate_root(|mc, arena| {
            let new_env = Gc::new(mc, RefLock::new(Environment::new(mc, None)));
            env_init(mc, arena, new_env);
            arena
                .executors
                .0
                .insert(TreewalkExecutor::new(mc, new_env, code))
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
                let context = arena.state.ctx(mc);
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
struct TreewalkState<'gc> {
    interner: Gc<'gc, RefLock<Rodeo>>,
}

impl<'gc> TreewalkState<'gc> {
    fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            interner: Gc::new_static(mc, RefLock::new(Rodeo::new())),
        }
    }

    fn ctx(&'gc self, mc: &'gc Mutation<'gc>) -> Context<'gc> {
        Context {
            mutation: mc,
            interner: self.interner.borrow_mut(mc),
        }
    }
}

#[derive(Debug, Clone)]
#[expect(dead_code)]
enum VirtualInstructionPayload {
    Number(i64),
    Bool(bool),
    String(String),
    Bytevector(Vec<u8>),
    Symbol(value::Symbol),
    List {
        head: Box<VirtualInstruction>,
        body: Vec<VirtualInstruction>,
        // when excuting a list, this an index into the body
        body_index: usize,
        // If the list has a dot, this is the element after the dot
        dot: Option<Box<VirtualInstruction>>,
    },
    Labeled {
        label: usize,
        instruction: Box<VirtualInstruction>,
        // we can cache if an instruction is circular
        //
        // this is so that we don't have to determine whether it is over and over again
        // and can be a quick check to prevent the execution of circular lists
        // (we don't want to stop the ~meh~ but technically correct #0=(... not even gonna use the label ....), but
        // we *also* want to fully preserve circular structures b/c `quote` is a thing)
        is_circular: bool,
    },
}

// TODO this is what to evaluate, it preserves information about
// where the instruction is from w/o using the GAst
#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
#[expect(dead_code)]
struct VirtualInstruction {
    payload: VirtualInstructionPayload,
    // if this is directly from datum, store it.
    // this can also be synthesized (generally copied) by macros
    // TODO should we start storing this kind of information in values too?
    range: TextRange,

    /// when using this, we can track how many times this particular expression
    /// has been executed (this is kept through even as a stack value)
    touch_count: usize,
}

/// A value pointer enhanced with source tracking information
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct StackValue<'gc> {
    value: ValuePtr<'gc>,
    #[collect(require_static)]
    range: TextRange,
    /// preservation of touch_count when used as a virtual instruction
    touch_count: usize,
}

/// Transparently access the value of this pointer
impl<'gc> std::ops::Deref for StackValue<'gc> {
    type Target = ValuePtr<'gc>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

/// This is what the treewalk is to execute next
#[derive(Clone, Debug, Collect)]
#[collect(no_drop)]
enum Instruction {
    // eventually this will move to VirtualInstruction instead of Datum
    // and an alternate instruction for executing a bytecode Chunk will be added
    // (which is why no_drop instead of require_static)
    // (the Chunk instruction will preserve the VirtualInstruction in case it is quoted)
    Evaluate(#[collect(require_static)] Datum),

    // Hopefully moved out once macro system is more generalized
    CallFunction {
        args: usize,
    },
    Define {
        name: Box<str>,
        fail_if_not_present: bool,
    },
}

pub struct Context<'gc> {
    pub mutation: &'gc Mutation<'gc>,
    pub interner: RefMut<'gc, Rodeo>,
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct TreewalkExecutor<'gc> {
    stack: Vec<ValuePtr<'gc>>,
    execution: Option<Instruction>,
    root_scope: Scope<'gc>,
    scope_stack: Vec<Scope<'gc>>,
}
pub type TreewalkExecutorPtr<'gc> = Gc<'gc, RefLock<TreewalkExecutor<'gc>>>;

// A scope consists of an environment, next instructions (continuation)
// and a "bottom", which is a marker for a sections of the stack the scope
// doesn't have access to.
#[derive(Debug, Collect, Clone)]
#[collect(no_drop)]
pub struct Scope<'gc> {
    pub environment: EnvironmentPtr<'gc>,
    current_lambda: Option<(LambdaCall<'gc>, LambdaPtr<'gc>)>,
    error: Option<SchemeErrorPtr<'gc>>,
    processed: usize,
    next_datum: VecDeque<Instruction>,
    bottom: Option<usize>,
    label: Option<Box<str>>,
}

impl<'gc> Scope<'gc> {
    #[inline]
    pub fn bottom(&self) -> Option<usize> {
        self.bottom
    }

    #[inline]
    pub fn label(&self) -> Option<&str> {
        self.label.as_ref().map(|s| s.as_ref())
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
}

enum LambdaCallReturn<'gc> {
    /// Return the top of the stack
    Result,
    /// Suspend the call to this lambda
    Suspend,
    /// Suspend the call to this lambda, executing the given code in a new frame
    Call(ValuePtr<'gc>),
    /// Execute the given code on this frame
    TailCall(ValuePtr<'gc>),
    /// Lambda error
    Error(LambdaExecError<'gc>),
}

#[derive(thiserror::Error, Debug)]
#[error("cannot step while root scope has errored")]
pub struct StepError;

impl<'gc> TreewalkExecutor<'gc> {
    fn new(
        mc: &Mutation<'gc>,
        environment: EnvironmentPtr<'gc>,
        code: Module,
    ) -> TreewalkExecutorPtr<'gc> {
        Gc::new(
            mc,
            RefLock::new(Self {
                stack: Vec::new(),
                root_scope: Scope {
                    error: None,
                    label: None,
                    environment,
                    processed: 0,
                    next_datum: code.datum().map(Instruction::Evaluate).collect(),
                    bottom: None,
                    current_lambda: None,
                },
                execution: None,
                scope_stack: Vec::new(),
            }),
        )
    }

    pub fn all_scopes(&self) -> impl Iterator<Item = &Scope<'gc>> {
        std::iter::once(&self.root_scope).chain(self.scope_stack.iter())
    }

    pub fn scope(&self) -> &Scope<'gc> {
        self.scope_stack.last().unwrap_or(&self.root_scope)
    }

    fn scope_mut(&mut self) -> &mut Scope<'gc> {
        self.scope_stack.last_mut().unwrap_or(&mut self.root_scope)
    }

    pub fn full_stack(&self) -> &[ValuePtr<'gc>] {
        &self.stack
    }

    pub fn stack(&self) -> &[ValuePtr<'gc>] {
        if let Some(bottom) = self.scope().bottom {
            &self.stack[bottom..]
        } else {
            &self.stack
        }
    }

    pub fn stack_mut(&mut self) -> &mut [ValuePtr<'gc>] {
        if let Some(bottom) = self.scope().bottom {
            &mut self.stack[bottom..]
        } else {
            &mut self.stack
        }
    }

    pub fn can_continue(&self) -> bool {
        // do we have an instruction to execute
        (self.execution.is_some()
            // does any scope have a continuation or suspended lambda
            || self
                .scope_stack
                .iter()
                .any(|scope| !scope.next_datum.is_empty() || scope.current_lambda.is_some())
            // does the root scope have a continuation or suspended lambda
            || !self.root_scope.next_datum.is_empty()
            || self.root_scope.current_lambda.is_some())
            // is the *root* scope not erroring (if it is, we "return" an error)
            && self.root_scope.error.is_none()
    }

    fn current_env(&self) -> EnvironmentPtr<'gc> {
        self.scope().environment
    }

    fn next_inst(&mut self) {
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
        self.execution = next_inst;
    }

    /// Push a new scope using a given parent environment
    fn push_scope_with_env(
        &mut self,
        mc: &Mutation<'gc>,
        label: impl AsRef<str>,
        bottom: usize,
        inst: impl IntoIterator<Item = Instruction>,
        parent: EnvironmentPtr<'gc>,
    ) {
        self.scope_stack.push(Scope {
            error: None,
            environment: Gc::new(mc, RefLock::new(Environment::new(mc, Some(parent)))),
            next_datum: inst.into_iter().collect(),
            label: Some(Box::from(label.as_ref())),
            bottom: Some(bottom),
            current_lambda: None,
            processed: 0,
        })
    }

    /// Push a new scope, using the current environment as the parent
    fn push_scope(
        &mut self,
        mc: &Mutation<'gc>,
        label: impl AsRef<str>,
        bottom: usize,
        inst: impl IntoIterator<Item = Instruction>,
    ) {
        let parent = self.scope().environment;
        self.push_scope_with_env(mc, label, bottom, inst, parent)
    }

    /// Pop the current scope.
    ///
    /// Returns the scope it popped (if any)
    fn pop_scope(&mut self) -> Option<Scope<'gc>> {
        self.scope_stack.pop()
    }

    /// Push a value pointer to current scope
    fn push(&mut self, value: ValuePtr<'gc>) {
        // get the current scope and push to there
        self.stack.push(value);
    }

    /// Push a value to current scope
    fn push_val(&mut self, mc: &Mutation<'gc>, value: impl IntoValue<'gc>) {
        // get the current scope and push to there
        self.push(Gc::new(mc, RefLock::new(value.into_value(mc))));
    }

    // Hardcoded-macro implementation
    //
    // returns if a macro was handled
    fn macro_handlng(
        &mut self,
        _ctx: &Context<'gc>,
        _fuel: &mut Fuel,
        symbol: Symbol,
        list: List,
    ) -> bool {
        let Some(ident) = symbol.identifier(false) else {
            return false;
        };

        match ident.as_ref() {
            "define" => {
                // define <name> <val>
                // define (<name> <args>) <body>...
                // set a variable in the environment
                let mut args = list.tail();
                let name = args.next();
                match name.as_ref().and_then(Datum::kind) {
                    Some(DatumKind::Symbol) => {
                        // this is the define <name> <val> form
                        let Some((val, name)) = args.next().zip(
                            name.as_ref()
                                .and_then(Datum::as_symbol)
                                .and_then(|s| s.identifier(false)),
                        ) else {
                            let syntax = list.syntax();
                            eprintln!(
                                "Malformed define: {} [{:?}]",
                                syntax
                                    .text()
                                    .to_string()
                                    .replace('\n', " ")
                                    .replace(['\t', '\r'], ""),
                                syntax.text_range()
                            );
                            return false;
                        };
                        let datum = &mut self.scope_mut().next_datum;
                        datum.push_front(Instruction::Define {
                            name,
                            fail_if_not_present: false,
                        });
                        datum.push_front(Instruction::Evaluate(val));
                    }
                    Some(DatumKind::List) => {
                        // this is the define (<name> <args>...) <body>... form
                        todo!()
                    }
                    _ => {
                        let syntax = list.syntax();
                        eprintln!(
                            "Malformed define: {} [{:?}]",
                            syntax
                                .text()
                                .to_string()
                                .replace('\n', " ")
                                .replace(['\t', '\r'], ""),
                            syntax.text_range()
                        );
                        return false;
                    }
                }

                true
            }
            "set!" => {
                // set! <name> <val>
                // set a variable in the env, error if it doesn't exist
                let mut args = list.tail();
                let Some(name) = args
                    .next()
                    .as_ref()
                    .and_then(Datum::as_symbol)
                    .and_then(|s| s.identifier(false))
                else {
                    eprintln!("malformed set! {}:{}", file!(), line!());
                    return true;
                };
                let Some(val) = args.next() else {
                    eprintln!("malformed set! {}:{}", file!(), line!());
                    return true;
                };
                if args.next().is_some() {
                    eprintln!("set! has too many args");
                    return true;
                }

                let datum = &mut self.scope_mut().next_datum;
                datum.push_front(Instruction::Define {
                    name,
                    fail_if_not_present: true,
                });
                datum.push_front(Instruction::Evaluate(val));
                true
            }
            "let" => {
                // let ((<name> <val>)...) <body>...
                // bind variables temporarily using the current environment
                true
            }
            "let*" => {
                // let* ((<name> <val>)...) <body>...
                // bind variables temporarily using sequential environments
                true
            }
            "lambda" => {
                // lambda <var> <body>...
                // lambda (<arg>...) <body>...
                // lambda (<arg>... . <rest>) <body>...
                // create an unnamed function
                true
            }
            _ => false,
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
                }
            }
            LambdaCallReturn::Suspend => {
                self.scope_mut().current_lambda = Some((call, lambda));
            }
            LambdaCallReturn::Call(_) => {
                // Create a new scope to execute the given code
                todo!()
            }
            LambdaCallReturn::TailCall(_) => {
                // Bash the current scope, replacing it with the given code
                todo!()
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
                                let val = val.borrow().resolve_into(ctx.interner.clone());
                                let _err_type = SchemeErrorType::Raise(val);
                                // make a Scheme error using the generating the current backtrace
                                // and the currrent execution's range
                                todo!("raised value")
                            }
                            ProcedureError::General(gen) => {
                                // Any *non-propagated* errors become the error of the current scope
                                let _err_type = SchemeErrorType::Rust(gen);
                                // make a Scheme error using the generating the current backtrace
                                // and the currrent execution's range
                                todo!("general error")
                            }
                        };

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
                                        range: todo!(),
                                    })
                                    .chain(err.backtrace.iter().cloned())
                                    .collect(),
                                    error_type: SchemeErrorType::Compound(errors),
                                },
                            )
                        } else {
                            err_ptr
                        };

                        // Pop the scope and assign the error to the upper scope
                        if self.pop_scope().is_none() {
                            unreachable!("root scope should not be popped")
                        }

                        self.scope_mut().error = Some(err_ptr);
                    }
                    LambdaExecError::TypecheckFailure(_err) => {
                        // Create a new error on this frame using this error
                    }
                    LambdaExecError::LambdaMismatch { id, call_id } => {
                        panic!("ICE: mismatched lambda call {call_id:p} for lambda {id:p}")
                    }
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
            // If the scope is processing a lambda, continue to do so
            if let Some((mut call, lambda)) = self.scope_mut().current_lambda.take() {
                let ret =
                    self.call_lambda(lambda, ctx.mutation, &mut call, fuel, self.scope().error);
                self.handle_lambda_return(ret, ctx, lambda, call);
            }

            // if there's nothing, but something remains, load it in
            if self.execution.is_none() {
                self.next_inst();
            }

            if let Some(exec) = self.execution.take() {
                match exec {
                    Instruction::Evaluate(exec) => {
                        match exec.kind() {
                            Some(DatumKind::Number) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                let Some(number) =
                                    exec.as_number().as_ref().and_then(Number::number)
                                else {
                                    eprintln!(
                                        "badly formatted number [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                let number: Option<i64> =
                                    if let SchemeNumber::Exact(ExactReal::Integer {
                                        value,
                                        is_neg,
                                    }) = number
                                    {
                                        if is_neg {
                                            value.try_into().ok().map(|i: i64| -i)
                                        } else {
                                            value.try_into().ok()
                                        }
                                    } else {
                                        None
                                    };

                                let Some(number) = number else {
                                    eprintln!(
                                        "unsupported number [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                self.push_val(ctx.mutation, number);
                            }
                            Some(DatumKind::String) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                let Some(string) =
                                    exec.as_string().as_ref().and_then(StringToken::string)
                                else {
                                    eprintln!(
                                        "badly formatted string [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                let string_value =
                                    Gc::new(ctx.mutation, RefLock::new(string.to_string()));
                                self.push(Gc::new(
                                    ctx.mutation,
                                    RefLock::new(Value::String(string_value)),
                                ));
                            }
                            Some(DatumKind::Boolean) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                let Some(bool) = exec.as_bool().as_ref().and_then(Boolean::bool)
                                else {
                                    eprintln!(
                                        "badly formatted boolean [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                self.push_val(ctx.mutation, bool);
                            }
                            Some(DatumKind::Character) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                let Some(char) = exec.as_char().as_ref().and_then(Character::char)
                                else {
                                    eprintln!(
                                        "badly formatted character [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                self.push_val(ctx.mutation, char);
                            }
                            Some(DatumKind::Symbol) => {
                                fuel.consume(FuelCosts::ENV_COST);
                                // this means to *evaluate* the symbol, not storing it
                                // we always assume case-sensitivity (as the directives are handled at the GAst layer
                                let Some(symbol) =
                                    exec.as_symbol().as_ref().and_then(|s| s.identifier(false))
                                else {
                                    eprintln!(
                                        "badly formatted symbol [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                let current_env = self.current_env();
                                let Some(binding) = current_env.borrow().get(&symbol) else {
                                    eprintln!(
                                        "no {} in env [{:?}]",
                                        symbol,
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };

                                self.push(binding.get());
                            }
                            Some(DatumKind::Bytevector) => {
                                fuel.consume(FuelCosts::LOAD_COST);
                                let Some(bv): Option<Vec<_>> = exec
                                    .as_bytevector()
                                    .as_ref()
                                    .and_then(|b| b.bytes().collect())
                                else {
                                    eprintln!(
                                        "badly formatted bytevector [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };

                                let bv = Gc::new(ctx.mutation, RefLock::new(bv));
                                self.push(Gc::new(
                                    ctx.mutation,
                                    RefLock::new(Value::Bytevector(bv.into())),
                                ));
                            }
                            Some(DatumKind::List) => {
                                // Lists handle their first subdatum specially when executed
                                // If the first item is:
                                //   - a symbol
                                //   - in the current env, refering to a macro
                                // then it is a macro, otherwise it's just a normal symbol guys

                                // for tail call optimization, if we have nothing else to execute, we
                                // are in tail position
                                let Some(list) = exec.as_list() else {
                                    eprintln!(
                                        "badly formatted list [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };
                                if list.has_dot() {
                                    eprintln!(
                                        "cannot execute dotted list [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                }
                                let Some(head) = list.head() else {
                                    eprintln!(
                                        "can't execute empty list [{:?}]",
                                        exec.syntax().text_range()
                                    );
                                    continue;
                                };

                                let label = format!(
                                    "{} <{:?}>",
                                    head.syntax().text(),
                                    list.syntax().text_range()
                                );

                                // If head is a macro, process it and push_front the resulting
                                // lists to next_datum
                                //
                                // Otherwise, call it as a function

                                // For now, we just hardcode all macros right here
                                if let Some(symbol) = head.as_symbol() {
                                    if self.macro_handlng(ctx, fuel, symbol, list.clone()) {
                                        continue;
                                    }
                                }

                                // This code assumes that `head` is *not* a macro
                                if !self.scope().next_datum.is_empty() {
                                    // we aren't in tail position, so we have to keep the original scope around
                                    self.push_scope(
                                        ctx.mutation,
                                        label,
                                        self.stack.len(),
                                        // put all the arguments as datum instructions
                                        list.datum().map(Instruction::Evaluate).chain(
                                            std::iter::once(Instruction::CallFunction {
                                                args: list.tail().count(),
                                            }),
                                        ),
                                    );
                                } else {
                                    // bash the current scope into executing our list
                                    self.scope_mut().label = Some(Box::from(label.as_str()));
                                    // we *know* next_datum is empty!
                                    self.scope_mut().next_datum = list
                                        .datum()
                                        .map(Instruction::Evaluate)
                                        .chain(std::iter::once(Instruction::CallFunction {
                                            args: list.tail().count(),
                                        }))
                                        .collect();
                                }
                            }
                            Some(_) => todo!(),
                            None => {
                                let syntax = exec.syntax();
                                eprintln!(
                                    "erroneous datum {} [{:?}]",
                                    syntax.text(),
                                    syntax.text_range()
                                );
                            }
                        }
                    }
                    Instruction::CallFunction { args } => {
                        let stack_len = self.stack.len();
                        let stack_keep = stack_len.saturating_sub(args);
                        if self.scope().bottom.is_some_and(|bot| stack_keep < bot) {
                            panic!("function bashes stack! {self:#?}");
                        }
                        let arg_stack = self.stack.split_off(stack_keep);

                        let Some(maybe_func) = self.stack.pop() else {
                            let scope = self.scope();
                            eprintln!(
                                "cannot execute nothing! {} processed {}",
                                scope.label().unwrap_or("<<root>>"),
                                scope.processed
                            );
                            continue;
                        };
                        if let Value::Lambda(lambda) = *maybe_func.borrow() {
                            let lambda_access = lambda.borrow_mut(ctx.mutation);
                            let mut call = lambda_access.call(arg_stack);
                            // drop the access so call_lambda can make a new one
                            drop(lambda_access);
                            fuel.consume(FuelCosts::CALL_COST);
                            let ret = self.call_lambda(
                                lambda,
                                ctx.mutation,
                                &mut call,
                                fuel,
                                self.scope().error,
                            );
                            self.handle_lambda_return(ret, ctx, lambda, call);
                        } else {
                            eprintln!("cannot call non-lambda {:?}", self.stack.last());
                        }
                    }
                    Instruction::Define {
                        name,
                        fail_if_not_present,
                    } => {
                        // the top of the stack has the value for what we are defining
                        let Some(value) = self.stack.pop() else {
                            let scope = self.scope();
                            eprintln!(
                                "Cannot define nothing!: {} processed {}",
                                scope.label().unwrap_or("<<root>>"),
                                scope.processed
                            );
                            continue;
                        };
                        if fail_if_not_present {
                            if let Err(e) = self
                                .scope()
                                .environment
                                .borrow_mut(ctx.mutation)
                                .rebind_ptr(ctx.mutation, &name, value)
                            {
                                eprintln!("cannot set {name}: {e}");
                            }
                        } else if self
                            .scope()
                            .environment
                            .borrow_mut(ctx.mutation)
                            .define_ptr(ctx.mutation, &name, value, false)
                            .is_err()
                        {
                            let scope = self.scope();
                            // The only error is if that binding is frozen
                            eprintln!(
                                "{name} in current scope is frozen: {} processed {}",
                                scope.label().unwrap_or("<<root>>"),
                                scope.processed
                            );
                        }
                    }
                }
            } else {
                // nothing to execute...
                break;
            }

            self.scope_mut().processed += 1;
        }

        Ok(())
    }
}
