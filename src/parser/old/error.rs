use crate::{
    source::Source,
    token::{DelimToken, Span, Token, TokenKind},
};
use std::fmt;

#[derive(Debug)]
pub enum ParseErrorKind<'a> {
    UnexpectedToken {
        found: Token<'a>,
        expected: &'static [&'static str],
    },
    UnexpectedEnd,
    MissingClosingDelim {
        open: Span,
        kind: DelimToken,
    },
    Todo,
}
impl<'a> fmt::Display for ParseErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorKind::*;
        match *self {
            UnexpectedToken { found, expected: _ } => writeln!(
                f,
                "encountered unexpected token, found '{}', expected ",
                found.kind
            ),
            UnexpectedEnd => writeln!(f, "unexpected end of file"),
            MissingClosingDelim { open: _, kind } => writeln!(
                f,
                "missing closing deliminator for '{}'",
                TokenKind::DelimOpen(kind)
            ),
            Todo => write!(f, "this syntax is not implemented yet"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub kind: ParseErrorKind<'a>,
    pub span: Span,
    pub src: Source<'a>,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}:", self.kind)?;
        let line = self.src.get_line(self.span);
        if let Some(line) = line {
            writeln!(f, "\t{}", self.src.line(line))?;
            if let Some(offset) = self.src.get_offset(line, self.span) {
                write!(f, "\t")?;
                for _ in 0..offset - 1 {
                    write!(f, "_")?;
                }
                write!(f, "^")?
            }
        }
        Ok(())
    }
}
