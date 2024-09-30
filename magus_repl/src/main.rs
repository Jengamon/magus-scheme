use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    lexer::Token, Comment, ContainsDatum, ContainsTrivia, DatumVisitor, ExternalRepresentation,
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

        readline.add_history_entry(input)?;
    }

    Ok(())
}
