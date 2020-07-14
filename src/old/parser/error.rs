use crate::{
    source::{Source, Sourced, Span},
    token::Token,
};
use std::{fmt, num};

#[derive(Debug, Clone)]
pub enum NumberParseErrorKind {
    Integer(num::ParseIntError),
    Float(num::ParseFloatError),
}

impl fmt::Display for NumberParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            NumberParseErrorKind::Integer(ref x) => write!(f, "{}", x),
            NumberParseErrorKind::Float(ref x) => write!(f, "{}", x),
        }
    }
}

pub enum ParseErrorKind<'a> {
    UnexpectedLineTerminator,
    Todo {
        file: &'static str,
        line: u32,
    },
    UnexpectedToken {
        found: Option<Token<'a>>,
        expected: &'static [&'static str],
        reason: Option<&'static str>,
    },
    NumberParseError(&'static str),
}

pub struct ParseError<'a> {
    pub kind: ParseErrorKind<'a>,
    pub origin: Span,
}

impl<'a> ParseError<'a> {}

impl<'a> fmt::Display for Sourced<'a, ParseError<'a>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: ")?;
        match self.value.kind {
            ParseErrorKind::UnexpectedLineTerminator => {
                writeln!(
                    f,
                    "unexpected line terminator, syntax forbids line terminator at this point",
                )?;
                self.source.fmt_span(f, self.value.origin)?;
                self.source.fmt_span_src(f, self.value.origin, None)
            }
            ParseErrorKind::Todo { ref file, ref line } => {
                writeln!(
                    f,
                    "parser encountered an unimplemented path in: {}:{}, Sorry!",
                    file, line
                )?;
                self.source.fmt_span(f, self.value.origin)?;
                self.source.fmt_span_src(f, self.value.origin, None)
            }
            ParseErrorKind::NumberParseError(reason) => {
                writeln!(f, "encountered invalid number: {}", reason)
            }
            ParseErrorKind::UnexpectedToken {
                ref found,
                expected,
                reason,
            } => {
                write!(f, "unexpected token")?;
                if let Some(x) = found {
                    write!(f, ": found '{}'", x.kind)?;
                }
                if expected.len() > 1 {
                    write!(f, " expected one of: [")?;
                    let mut first = true;
                    for e in expected.iter() {
                        if !first {
                            write!(f, ",")?;
                        } else {
                            first = false;
                        }
                        write!(f, "{}", e)?;
                    }
                    write!(f, "]")?;
                } else if expected.len() == 1 {
                    write!(f, " expected '{}'", expected[0])?;
                }
                writeln!(f, "")?;
                self.source.fmt_span(f, self.value.origin)?;
                self.source.fmt_span_src(f, self.value.origin, reason)
            }
        }
    }
}
