use core::fmt;
#[cfg(feature = "trace_error")]
use std::backtrace::Backtrace;

use common::{
    source::{self, Source},
    span::Span,
    string::String,
};
use token::TokenKind;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// The parser unexpectedly encounted the end of source.
    UnexpectedEnd {
        expected: Vec<TokenKind>,
        message: Option<String>,
    },
    Unexpected {
        expected: Vec<TokenKind>,
        found: TokenKind,
        message: Option<String>,
    },
    DisallowedToken {
        found: TokenKind,
        message: Option<String>,
    },
    InvalidToken,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
    #[cfg(feature = "trace_error")]
    trace: Backtrace,
}

pub struct FormatableError<'a> {
    error: Error,
    source: &'a Source,
}

impl<'a> fmt::Display for FormatableError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.format(self.source, f).map_err(|_| fmt::Error)
    }
}

impl Error {
    pub fn new(kind: ErrorKind, origin: Span) -> Self {
        Error {
            kind,
            origin,
            #[cfg(feature = "trace_error")]
            trace: Backtrace::capture(),
        }
    }

    pub fn display(self, source: &Source) -> FormatableError {
        FormatableError {
            error: self,
            source,
        }
    }

    pub fn format<W: fmt::Write>(
        &self,
        source: &Source,
        w: &mut W,
    ) -> Result<(), source::FormatError> {
        match self.kind {
            ErrorKind::UnexpectedEnd {
                ref expected,
                ref message,
            } => {
                write!(w, "Source ended unexpectedly")?;
                if !expected.is_empty() {
                    if expected.len() > 1 {
                        write!(w, ", expected one of:")?;
                        for e in expected {
                            write!(w, ", {:?}", e)?;
                        }
                    } else {
                        write!(w, ", expected: {:?}", &expected[0])?;
                    }
                }
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin.clone())?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin.clone(),
                    message.as_ref().map(|x| x.encoding()),
                )?;
            }
            ErrorKind::Unexpected {
                ref expected,
                ref found,
                ref message,
            } => {
                write!(w, "unexpected token `{:?}`", found)?;
                if !expected.is_empty() {
                    if expected.len() > 1 {
                        write!(w, ", expected one of:")?;
                        for e in expected {
                            write!(w, ", {:?}", e)?;
                        }
                    } else {
                        write!(w, ", expected: {:?}", &expected[0])?;
                    }
                }
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin.clone())?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin.clone(),
                    message.as_ref().map(|x| x.encoding()),
                )?;
            }
            ErrorKind::DisallowedToken {
                ref found,
                ref message,
            } => {
                write!(w, "token `{:?}` is not allowed in this context", found)?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin.clone())?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin.clone(),
                    message.as_ref().map(|x| x.encoding()),
                )?;
            }
            ErrorKind::InvalidToken => {
                write!(w, "invalid token")?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin.clone())?;
                writeln!(w)?;
                source.render_string_block(w, self.origin.clone(), None)?;
            }
        }
        #[cfg(feature = "trace_error")]
        writeln!(w)?;
        #[cfg(feature = "trace_error")]
        writeln!(w, "{}", self.trace)?;
        Ok(())
    }
}
