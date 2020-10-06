use common::{
    interner::Interner,
    source::{Source, Span},
};
use lexer::{Error as LexerError, ErrorKind as LexerErrorKind};
use std::io::{Result, Write};
use token::Token;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedLineTerminator,
    Todo {
        file: &'static str,
        line: u32,
        token: Option<String>,
    },
    UnexpectedToken {
        found: Option<Token>,
        expected: &'static [&'static str],
        reason: Option<&'static str>,
    },
    RedeclaredVariable,
    LexerError(LexerErrorKind),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
}

impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Error {
            kind: ErrorKind::LexerError(e.kind),
            origin: e.origin,
        }
    }
}

impl Error {
    pub fn format<F: Write>(&self, mut w: F, source: &Source, interner: &Interner) -> Result<()> {
        write!(w, "error: ")?;
        match self.kind {
            ErrorKind::UnexpectedLineTerminator => {
                writeln!(
                    w,
                    "unexpected line terminator, syntax forbids line terminator at this point",
                )?;
                source.format_span_line(&mut w, self.origin)?;
                source.format_span_block(&mut w, self.origin, None)?;
            }
            ErrorKind::Todo {
                ref file,
                ref line,
                ref token,
            } => {
                write!(
                    w,
                    "parser encountered an unimplemented path in: {}:{}, Sorry!",
                    file, line
                )?;
                if let Some(x) = token {
                    writeln!(w, " token: {}", x)?;
                } else {
                    writeln!(w)?;
                }
                source.format_span_line(&mut w, self.origin)?;
                source.format_span_block(&mut w, self.origin, None)?;
            }
            ErrorKind::RedeclaredVariable => {
                writeln!(w, "redeclared variable")?;
                source.format_span_line(&mut w, self.origin)?;
                source.format_span_block(&mut w, self.origin, Some("variable redeclared here"))?;
            }
            ErrorKind::UnexpectedToken {
                ref found,
                expected,
                reason,
            } => {
                write!(w, "unexpected token")?;
                if let Some(x) = found {
                    write!(w, ": found '{}'", x.kind.format(interner))?;
                }
                match expected.len() {
                    0 => {}
                    1 => {
                        write!(w, " expected '{}'", expected[0])?;
                    }
                    _ => {
                        write!(w, " expected one of: [")?;
                        let mut first = true;
                        for e in expected.iter() {
                            if !first {
                                write!(w, ",")?;
                            } else {
                                first = false;
                            }
                            write!(w, "{}", e)?;
                        }
                        write!(w, "]")?;
                    }
                }
                writeln!(w)?;
                source.format_span_line(&mut w, self.origin)?;
                source.format_span_block(&mut w, self.origin, reason)?;
            }
            ErrorKind::LexerError(_) => todo!(),
        }
        Ok(())
    }
}
