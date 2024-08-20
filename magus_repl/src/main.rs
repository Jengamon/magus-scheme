use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    general_parser::{
        Comment, ContainsDatum, ContainsTrivia, DatumVisitor, GAstNode, MagusSyntaxElementRef,
        Module, Symbol,
    },
    lexer::Token,
};
use rustyline::{history::MemHistory, Config};
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
    fn visit_list(&mut self, list: &magus::general_parser::List) {
        println!("is list valid? {}", list.is_valid());
        println!("is list special form? {:?}", list.special_form(false));
        self.print_comments(list);
        self.visit_composite(list);
    }

    fn visit_bytevector(&mut self, bytevector: &magus::general_parser::Bytevector) {
        println!(
            "is bytevector valid? {} {:?}",
            bytevector.is_valid(),
            bytevector.bytes().collect::<Vec<_>>()
        );
        self.print_comments(bytevector);
        self.visit_composite(bytevector);
    }

    fn visit_vector(&mut self, vector: &magus::general_parser::Vector) {
        self.print_comments(vector);
        self.visit_composite(vector);
    }

    fn visit_number(&mut self, number: &magus::general_parser::Number) {
        println!("got number? {:?}", number.number());
    }

    fn visit_abbreviation(&mut self, abbreviation: &magus::general_parser::Abbreviation) {
        self.visit_composite(abbreviation)
    }

    fn visit_labeled(&mut self, labeled: &magus::general_parser::LabeledDatum) {
        println!("Is label circular? -> {}", labeled.is_circular());
        self.visit_composite(labeled)
    }
}

// TODO Make SchemeHelper for all the REPL goodies
fn repl() -> anyhow::Result<()> {
    let mut readline =
        rustyline::Editor::<(), _>::with_history(Config::default(), MemHistory::new())?;

    while let Ok(input) = readline.readline(">> ") {
        let src = input.as_str();

        // General parse
        let gast = magus::general_parser::general_parse(&input);

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
            fn visit_list(&mut self, list: &magus::general_parser::List) {
                self.visit_if_unfound(list)
            }

            fn visit_vector(&mut self, vector: &magus::general_parser::Vector) {
                self.visit_if_unfound(vector)
            }

            fn visit_abbreviation(&mut self, abbreviation: &magus::general_parser::Abbreviation) {
                self.visit_if_unfound(abbreviation)
            }

            fn visit_labeled(&mut self, labeled: &magus::general_parser::LabeledDatum) {
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
