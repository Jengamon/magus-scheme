//! General parsing starts where the lexer dropped of, and handles nested syntax, while
//! also forming a GAST which is a relatively simple layer on top of a [`rowan`] CST.
use logos::Lexer;

use crate::lexer::Token;

pub struct GeneralParser;

impl GeneralParser {
    pub fn parse(lexer: Lexer<Token>) -> GAst {
        _ = lexer;
        todo!()
    }
}

// Using rowan and CST should (and hopefully does) give us a nice property that
// calculating lines should be possible (as it is a full fidelity structure)

/// GAst top level
#[derive(Debug)]
pub struct GAst(pub(crate) Box<[GAstNode]>);

/// Any possible GAst node
#[derive(Debug)]
pub enum GAstNode {}
