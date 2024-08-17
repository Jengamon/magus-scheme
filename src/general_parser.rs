//! General parsing starts where the lexer dropped of, and handles nested syntax, while
//! also forming a GAST which is a relatively simple layer on top of a [`rowan`] CST.
use logos::Lexer;

pub struct GeneralParser;

// Using rowan and CST should (and hopefully does) give us a nice property that
// calculating lines should be possible (as it is a full fidelity structure)

/// Any possible GAST node
#[derive(Debug)]
pub enum Gast {}
