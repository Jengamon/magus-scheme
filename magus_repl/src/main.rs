use std::ops::Neg;

use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use gc_arena::{Collect, Gc, Mutation};
use magus::{
    lexer::Token,
    runtime::{
        convert::IntoValue,
        lambda::{Lambda, LambdaCall, ProcedureError, ProcedureReturn, Typecheck},
        value::ValueType,
    },
    treewalk::{scheme, Context, StackValue, Treewalk, TreewalkArena, TreewalkExecutor},
    user_type, Comment, ContainsDatum, ContainsTrivia, DatumVisitor, ExternalRepresentation, Fuel,
    GAstNode, MagusSyntaxElementRef, Module, Symbol,
};
use rustyline::{
    history::{History, MemHistory},
    Config, Editor, Helper,
};
use yansi::Paint;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input file to read (use `-` for stdin, and not present to use REPL mode)
    file: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    if let Some(file) = args.file {
        if file == "-" {
            use magus::{Label, Labeled, ListOrVector, StringOrSymbol, ToExternal as _};
            println!(
                "{}",
                Labeled(
                    0,
                    ListOrVector::List(
                        &[
                            StringOrSymbol::Symbol("quote").to_external(),
                            Label(0).to_external(),
                            ().to_external()
                        ],
                        false
                    )
                )
                .to_external()
            );
            todo!("read from standard input")
        } else {
            todo!("open and read file")
        }
    } else {
        repl()
    }
}

#[derive(Default)]
struct CommentPrinter {
    count: usize,
}

impl CommentPrinter {
    fn print_comments<C: ContainsTrivia>(&mut self, node: &C) {
        for comment in node.comments() {
            self.count += 1;
            if let Comment::Nested(nest) = &comment {
                println!("Is nested valid? {}", nest.is_valid());
            }
            match comment.syntax() {
                MagusSyntaxElementRef::Token(tok) => {
                    println!("[{:?}] {}", tok.text_range(), tok.text());
                }
                MagusSyntaxElementRef::Node(node) => {
                    println!("[{:?}] {}", node.text_range(), node.text());
                }
            }
        }
    }
}

impl DatumVisitor for CommentPrinter {
    fn visit_list(&mut self, list: &magus::List) {
        println!("is list valid? {}", list.is_valid());
        self.print_comments(list);
        self.visit_composite(list);
    }

    fn visit_bytevector(&mut self, bytevector: &magus::Bytevector) {
        println!(
            "is bytevector valid? {} {:?}",
            bytevector.is_valid(),
            bytevector.bytes().collect::<Vec<_>>()
        );
        self.print_comments(bytevector);
        self.visit_composite(bytevector);
    }

    fn visit_vector(&mut self, vector: &magus::Vector) {
        self.print_comments(vector);
        self.visit_composite(vector);
    }

    fn visit_number(&mut self, number: &magus::Number) {
        println!("got number? {:?}", number.number());
    }

    fn visit_abbreviation(&mut self, abbreviation: &magus::Abbreviation) {
        self.visit_composite(abbreviation)
    }

    fn visit_labeled(&mut self, labeled: &magus::LabeledDatum) {
        println!("Is label circular? -> {}", labeled.is_circular());
        self.visit_composite(labeled)
    }
}

fn read_prompt(readline: &mut Editor<impl Helper, impl History>) -> rustyline::Result<String> {
    let mut input = readline.readline(">> ")?;

    while input.ends_with(',') {
        _ = input.pop();
        input.push('\n');
        input.push_str(&readline.readline(".. ")?);
    }

    Ok(input)
}

// TODO Make SchemeHelper for all the REPL goodies
fn repl() -> anyhow::Result<()> {
    let mut readline =
        rustyline::Editor::<(), _>::with_history(Config::default(), MemHistory::new())?;

    let mut interp = Treewalk::default();

    while let Ok(input) = read_prompt(&mut readline) {
        let src = input.as_str();

        // General parse
        let gast = magus::general_parse(&input);

        let idx = LineIndex::new(src);

        let blocks = (!gast.errors().is_empty())
            .then_some(gast.errors())
            .map(|errors| {
                errors.iter().flat_map(|err| {
                    Block::new(
                        &idx,
                        [Label::new(err.span())
                            .with_text(err.to_string())
                            .with_style(|s| s.red().to_string())],
                    )
                })
            });

        if let Some(blocks) = blocks {
            for block in blocks.map(|blk| blk.map_code(|c| CodeWidth::new(c, c.len()))) {
                println!("{}[repl.scm]", block.prologue());
                print!("{block}");
                println!("{}", block.epilogue());
            }
        }

        if !gast.errors().is_empty() {
            readline.add_history_entry(input)?;
            continue;
        }

        // Tell me your secrets
        let module = Module::cast(gast.syntax()).unwrap();

        #[derive(Default)]
        struct FirstIdent {
            ident: Option<Symbol>,
        }

        impl FirstIdent {
            fn visit_if_unfound<C: ContainsDatum>(&mut self, composite: &C) {
                if self.ident.is_none() {
                    self.visit_composite(composite)
                }
            }
        }

        impl DatumVisitor for FirstIdent {
            fn visit_list(&mut self, list: &magus::List) {
                self.visit_if_unfound(list)
            }

            fn visit_vector(&mut self, vector: &magus::Vector) {
                self.visit_if_unfound(vector)
            }

            fn visit_abbreviation(&mut self, abbreviation: &magus::Abbreviation) {
                self.visit_if_unfound(abbreviation)
            }

            fn visit_labeled(&mut self, labeled: &magus::LabeledDatum) {
                self.visit_if_unfound(labeled)
            }

            fn visit_symbol(&mut self, symbol: &Symbol) {
                if self.ident.is_none() {
                    self.ident = Some(symbol.clone())
                }
            }
        }

        let mut first_ident = FirstIdent::default();
        first_ident.visit_composite(&module);

        // Print the first identifier of the module
        if let Some(ft) = first_ident.ident {
            println!("First identifier of module: {:?}", ft.identifier(false));
        }

        // list all comments
        println!("Comment listing:");
        let mut comment_printer = CommentPrinter::default();
        comment_printer.print_comments(&module);
        comment_printer.visit_composite(&module);
        if comment_printer.count == 0 {
            println!("... no comments");
        }

        // Show what the parser sees
        println!("{:#?}", gast.syntax());

        // Print the programs parsable external representation
        for datum in module.datum() {
            let repr: Result<ExternalRepresentation, ()> = datum.try_into();
            if let Ok(repr) = repr {
                println!("{repr}");
            }
        }
        let tokens = Token::lexer(src);

        for (tok, span) in tokens {
            match tok {
                Ok(Token::Syntax(syntax)) => {
                    println!("[{span:?}] {}", format!("{syntax:?}").cyan())
                }
                Ok(Token::NestedComment(nc)) => println!("[{span:?}] {nc:?}"),
                Err(err) => println!("[{span:?}] {}", err.to_string().red()),
            }
        }

        struct MyCoolType {
            x: i64,
        }
        user_type!(MyCoolType);

        fn get_static_sym<'gc>(
            arena: &TreewalkArena<'gc>,
            mc: &Mutation<'gc>,
            sym: &'static str,
        ) -> magus::value::Symbol {
            arena
                .state
                .interner
                .borrow_mut(mc)
                .get_or_intern_static(sym)
                .into()
        }

        // evaluate using treewalk
        let exec = interp.new_executor(module, 0, |mc, arena, env| {
            let mut env = env.borrow_mut(mc);
            let x_sym = get_static_sym(arena, mc, "x");
            env.define(mc, x_sym, StackValue::external(mc, 3i64), false)
                .unwrap();

            let _ = env.get(x_sym);

            // define a lambda for +
            let all_numbers_typecheck = |op: &'static str| {
                Typecheck::new(move |sig| {
                    if sig.iter().all(|ty| ty == &ValueType::Number) && !sig.is_empty() {
                        Ok(())
                    } else {
                        Err(if sig.is_empty() {
                            anyhow::anyhow!("cannot {op} nothing")
                        } else {
                            anyhow::anyhow!("cant {op} on {sig:?}")
                        })
                    }
                })
            };

            fn plus_impl<'gc>(
                _root: &mut TestRoot<'gc>,
                ctx: &Context<'gc>,
                call: &mut LambdaCall<'gc>,
                _interpreter: &mut TreewalkExecutor<'gc>,
                _fuel: &mut Fuel,
            ) -> Result<ProcedureReturn<'gc>, ProcedureError<'gc>> {
                match call.pop::<&MyCoolType>() {
                    Ok(mct) => {
                        return Ok(ProcedureReturn::Return(
                            _interpreter.current_scope_value(ctx.mutation, mct.x),
                        ));
                    }
                    Err(vp) => {
                        if let Some(vp) = vp {
                            call.stack.push(vp);
                        }
                    }
                };
                // TODO make this nicer...
                let mut total = 0;
                while !call.stack.is_empty() {
                    let Some(v) = call.pop::<i64>().ok() else {
                        unreachable!("typechecking");
                    };
                    // TODO when implementing in standard library, make these checked operations
                    total += v;
                }
                call.stack
                    .push(_interpreter.current_scope_value(ctx.mutation, MyCoolType { x: total }));
                Ok(ProcedureReturn::Suspend)
            }

            #[derive(Collect, Debug, Clone, Copy)]
            #[collect(no_drop)]
            struct TestRoot<'gc> {
                i: magus::value::Value<'gc>,
            }

            let troot = TestRoot {
                i: 3i64.into_value(mc),
            };
            let plus_lambda =
                Lambda::with_root_typecheck(mc, all_numbers_typecheck("add"), troot, plus_impl);
            let sub_lambda = Lambda::with_typecheck(
                mc,
                all_numbers_typecheck("subtract"),
                move |_, ctx, call, _interpreter, _| {
                    let mut data = vec![];
                    while !call.stack.is_empty() {
                        let Some(v) = call.pop::<i64>().ok() else {
                            unreachable!("typechecking");
                        };
                        data.push(v);
                    }
                    let init = data.pop().unwrap();
                    data.reverse();

                    Ok(ProcedureReturn::Return(if !data.is_empty() {
                        // TODO when implementing in standard library, make these checked operations
                        _interpreter.current_scope_value(
                            ctx.mutation,
                            data.into_iter().fold(init, |acc, it| acc - it),
                        )
                    } else {
                        // TODO when implementing in standard library, make these checked operations
                        _interpreter.current_scope_value(ctx.mutation, init.neg())
                    }))
                },
            );
            let mul_lambda = Lambda::with_typecheck(
                mc,
                all_numbers_typecheck("multiply"),
                move |_, ctx, call, _interpreter, _| {
                    let mut total = 1;
                    while !call.stack.is_empty() {
                        let Some(v) = call.pop::<i64>().ok() else {
                            unreachable!("typechecking");
                        };
                        // TODO when implementing in standard library, make these checked operations
                        total *= v;
                    }
                    Ok(ProcedureReturn::Return(
                        _interpreter.current_scope_value(ctx.mutation, total),
                    ))
                },
            );

            let add_sym = get_static_sym(arena, mc, "+");
            let sub_sym = get_static_sym(arena, mc, "-");
            let mul_sym = get_static_sym(arena, mc, "*");
            let div_sym = get_static_sym(arena, mc, "/");
            let define_sym = get_static_sym(arena, mc, "define");
            let setbang_sym = get_static_sym(arena, mc, "set!");
            let lambda_sym = get_static_sym(arena, mc, "lambda");
            env.define(mc, add_sym, StackValue::external(mc, plus_lambda), false)
                .unwrap();
            env.define(mc, sub_sym, StackValue::external(mc, sub_lambda), false)
                .unwrap();
            env.define(
                mc,
                div_sym,
                StackValue::external(mc, arena.scheme.base(mc).op_div(mc)),
                false,
            )
            .unwrap();
            env.define(mc, mul_sym, StackValue::external(mc, mul_lambda), false)
                .unwrap();
            env.define_macro(mc, define_sym, scheme::base::macros::Define)
                .unwrap();
            env.define_macro(mc, setbang_sym, scheme::base::macros::SetBang)
                .unwrap();
            env.define_macro(mc, lambda_sym, scheme::base::macros::Lambda)
                .unwrap();
        });
        let mut fuel = Fuel::with(1);
        let mut running = true;

        // collect all garbage from previous runs
        interp.arena_mut(|arena| {
            arena.collect_all();
        });

        while running {
            fuel.refill(10, 1);
            running = interp
                .run(exec.clone(), |ctx, mut exec| {
                    exec.step(&ctx, &mut fuel).unwrap();
                    println!("== STACK CHECK fuel: {} ==", fuel.remaining());
                    for (idx, ptr) in exec.full_stack().iter().enumerate() {
                        let tc = ptr.touch_count();
                        let resolved = ptr
                            .borrow()
                            .resolve_into(ctx.interner.clone(), ctx.null_ptr);
                        println!("- [{tc}] {idx}: {resolved}");
                    }

                    println!("== SCOPES ==");
                    let scopes: Vec<_> = exec.all_scopes().map(|sc| (sc.environment, sc)).collect();
                    for (_, scope) in &scopes {
                        println!(
                            "--> {} parent {}",
                            scope.label(),
                            scope
                                .environment
                                .borrow()
                                .parent()
                                .and_then(|penv| scopes
                                    .iter()
                                    .find(|sc| Gc::ptr_eq(sc.0, penv))
                                    .and_then(|sc| sc.1.maybe_label()))
                                .unwrap_or("<<none>>")
                        )
                    }

                    for cont in exec.continuation() {
                        println!(">>> REWRITE {cont:?}")
                    }

                    if let Some(call) = exec.lambda_call() {
                        println!(">>>> SUSPENDED: {call}");
                    }

                    let source_data = &[(src, Some(Box::from("repl.scm")))];
                    if let Some(err) = exec.scope().error() {
                        println!(">>>> ERROR: {}", err.clone().display(source_data));
                        // println!(">>>> ERROR: {:?}", err);
                    }
                    println!(
                        "{}",
                        exec.all_scopes()
                            .map(|sc| format!(
                                "{:?}",
                                sc.error()
                                    .map(|err| err.clone().display(source_data).to_string())
                            ))
                            .collect::<Vec<_>>()
                            .join("\n")
                    );

                    let metrics = ctx.mutation.metrics();
                    println!(
                        "MEMORY: {} bytes (debt {})",
                        metrics.total_allocation(),
                        metrics.allocation_debt()
                    );
                    exec.can_continue()
                })
                .unwrap();
        }

        readline.add_history_entry(input)?;
    }

    Ok(())
}
