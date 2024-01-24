use common::{interner::Interner, source::{Source, Span}};
use lexer::{Error as LexerError, ErrorKind as LexerErrorKind};
use std::fmt;
use token::Token;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
}

impl Error {
    pub fn format<'a>(self, source: &'a Source, interner: &'a Interner) -> FormattedError<'a> {
        FormattedError {
            error: self,
            source,
            interner,
        }
    }
}

impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Error {
            kind: ErrorKind::LexerError(e.kind),
            origin: e.origin,
        }
    }
}

// An error with data required to provide full context in the error.
pub struct FormattedError<'a> {
    error: Error,
    source: &'a Source,
    interner: &'a Interner,
}

impl fmt::Display for FormattedError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format(f)
    }
}

impl FormattedError<'_> {
    // Format an error according to rust like error messages.
    pub fn format<F: fmt::Write>(&self, mut w: F) -> fmt::Result {
        write!(w, "error: ")?;
        match self.error.kind {
            // Line terminator found where it is explicitly forbiden to have a line terminator
            ErrorKind::UnexpectedLineTerminator => {
                writeln!(
                    w,
                    "unexpected line terminator, syntax forbids line terminator at this point",
                )?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            // Todo marked in the parser
            ErrorKind::Todo {
                file,
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
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            // Variable was redeclared in the same scope.
            ErrorKind::RedeclaredVariable => {
                writeln!(w, "redeclared variable")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source.format_span_block(
                    &mut w,
                    self.error.origin,
                    Some("variable redeclared here"),
                )?;
            }
            // Parser encountered a token it did not expect.
            ErrorKind::UnexpectedToken {
                ref found,
                expected,
                reason,
            } => {
                write!(w, "unexpected token")?;
                if let Some(x) = found {
                    write!(w, ": found '{}'", x.kind.format(self.interner))?;
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
                            if first {
                                first = false;
                            } else {
                                write!(w, ",")?;
                            }
                            if e == &"\n" {
                                write!(w, "\\n")?;
                            } else {
                                write!(w, "{}", e)?;
                            }
                        }
                        write!(w, "]")?;
                    }
                }
                writeln!(w)?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, reason)?;
            }
            ErrorKind::LexerError(LexerErrorKind::InvalidNumber) => {
                writeln!(w, "could not parse number")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            ErrorKind::LexerError(LexerErrorKind::InvalidUnicodeSequence) => {
                writeln!(w, "invalid unicode escape code")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            ErrorKind::LexerError(LexerErrorKind::InvalidEscapeCode) => {
                writeln!(w, "invalid unicode escape code")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            ErrorKind::LexerError(LexerErrorKind::InvalidToken) => {
                writeln!(w, "invalid token")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            ErrorKind::LexerError(LexerErrorKind::UnClosedString) => {
                writeln!(w, "string not closed")?;
                self.source.format_span_line(&mut w, self.error.origin)?;
                self.source
                    .format_span_block(&mut w, self.error.origin, None)?;
            }
            ref x => todo!("error kind: {:?}", x),
        }
        Ok(())
    }
}
