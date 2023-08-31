use core::fmt;
#[cfg(feature = "trace_error")]
use std::backtrace::Backtrace;

use common::{
    result::ContextError,
    source::{self, Source},
    span::Span,
    string::{Ascii, String},
};
use token::TokenKind;

/// The type of error generated.
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
    NotAssignable,
    InvalidDestructuringAssigment,
    InvalidToken,
    CoveredObjectLiteral,
}

/// A parser error.
#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub origin: Span,
    #[cfg(feature = "trace_error")]
    trace: Backtrace,
}

impl ContextError<Source> for Error {
    fn display(&self, f: &mut fmt::Formatter, ctx: &Source) -> fmt::Result {
        self.format(ctx, f).map_err(|_| fmt::Error)
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

    pub fn format<W: fmt::Write>(
        &self,
        source: &Source,
        w: &mut W,
    ) -> Result<(), source::FormatError> {
        #[cfg(feature = "trace_error")]
        writeln!(w)?;
        #[cfg(feature = "trace_error")]
        writeln!(w, "{}", self.trace)?;

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
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin,
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
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin,
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
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin,
                    message.as_ref().map(|x| x.encoding()),
                )?;
            }
            ErrorKind::NotAssignable => {
                write!(w, "left hand side expression is not assignable")?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(w, self.origin, None)?;
            }
            ErrorKind::InvalidDestructuringAssigment => {
                write!(w, "invalid destructuring assignment target")?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(w, self.origin, None)?;
            }
            ErrorKind::InvalidToken => {
                write!(w, "invalid token")?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(w, self.origin, None)?;
            }
            ErrorKind::CoveredObjectLiteral => {
                write!(w, "invalid object literal")?;
                writeln!(w)?;
                write!(w, " --> ")?;
                source.render_span_location(w, self.origin)?;
                writeln!(w)?;
                source.render_string_block(
                    w,
                    self.origin,
                    Some(
                        Ascii::const_from_str(
                            "This is only valid syntax in destructuring patterns.",
                        )
                        .into(),
                    ),
                )?;
            }
        }
        Ok(())
    }
}
