use crate::token::{Span, Token, TokenKind};
use std::fmt;

#[derive(Debug)]
pub enum ParseErrorKind<'a> {
    UnexpectedToken {
        found: Token<'a>,
        expected: &'static [TokenKind<'static>],
    },
    UnexpectedEnd,
    Todo,
}
impl<'a> fmt::Display for ParseErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorKind::*;
        match *self {
            UnexpectedToken { found, expected: _ } => writeln!(
                f,
                "encountered unexpected token, found \"{}\", expected ",
                found.kind
            ),
            UnexpectedEnd => writeln!(f, "unexpected end of file"),
            Todo => writeln!(f, "this syntax is not implemented yet"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub kind: ParseErrorKind<'a>,
    pub span: Span,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}", self.kind)
    }
}
