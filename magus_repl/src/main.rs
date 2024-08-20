use clap::Parser;
use codesnake::{Block, CodeWidth, Label, LineIndex};
use magus::{
    general_parser::{
        ContainsComments, DatumVisitor, GAstNode, GAstToken, MagusSyntaxElement,
        MagusSyntaxElementRef, Module, Symbol, SyntaxKind,
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
    fn print_comments<C: ContainsComments>(&mut self, node: &C) {
        for comment in node.comments() {
            self.count += 1;
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
        self.print_comments(list);
        self.visit_composite(list);
    }

    fn visit_vector(&mut self, vector: &magus::general_parser::Vector) {
        self.visit_composite(vector);
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

        if let Some(ft) = gast
            .syntax()
            .children_with_tokens()
            .filter_map(|elem| match elem {
                // idents are within DATUM nodes, so...
                MagusSyntaxElement::Token(_) => None,
                MagusSyntaxElement::Node(n) => {
                    if n.kind() == SyntaxKind::DATUM {
                        Some(n)
                    } else {
                        None
                    }
                }
            })
            .find_map(|node| {
                node.children_with_tokens().find_map(|elem| match elem {
                    MagusSyntaxElement::Token(t) => Symbol::cast(t),
                    _ => None,
                })
            })
        {
            println!("First identifier of module: {:?}", ft.identifier(false));
        }

        // Tell me your secrets
        let module = Module::cast(gast.syntax()).unwrap();

        // list all comments
        println!("Comment listing:");
        let mut comment_printer = CommentPrinter::default();
        comment_printer.visit_composite(&module);
        if comment_printer.count == 0 {
            println!("... no comments");
        }

        // Show what the parser sees
        println!("{:#?}", gast.syntax());
        let idx = LineIndex::new(src);

        let block = (!gast.errors().is_empty())
            .then_some(gast.errors())
            .and_then(|errors| {
                Block::new(
                    &idx,
                    errors.iter().map(|err| {
                        Label::new(err.span())
                            .with_text(err.to_string())
                            .with_style(|s| s.red().to_string())
                    }),
                )
            });

        if let Some(block) = block.map(|blk| blk.map_code(|c| CodeWidth::new(c, c.len()))) {
            println!("{}[repl.scm]", block.prologue());
            print!("{block}");
            println!("{}", block.epilogue());
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
