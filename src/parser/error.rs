use crate::{
    source::{Source, Span},
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
    NumberParseError {
        kind: NumberParseErrorKind,
    },
}

pub struct ParseError<'a> {
    pub kind: ParseErrorKind<'a>,
    pub origin: Span,
    pub source: Source<'a>,
}

impl<'a> ParseError<'a> {
    fn fmt_span(&self, f: &mut fmt::Formatter<'_>, span: Span) -> fmt::Result {
        write!(f, " --> ")?;
        if let Some(x) = self.source.path() {
            write!(f, "{}", x.display())?;
        } else {
            write!(f, "??")?;
        }
        if let Some((a, _)) = self.source.get_source_position(span) {
            writeln!(f, ":{}:{}", a.line, a.column)?;
        } else {
            writeln!(f, ":??:??")?;
        }
        Ok(())
    }

    fn fmt_span_src(
        &self,
        f: &mut fmt::Formatter<'_>,
        span: Span,
        message: Option<&'static str>,
    ) -> fmt::Result {
        if let Some((a, _)) = self.source.get_source_position(span) {
            let line = self.source.line(a.line);
            let num_chars = line[0 as usize..a.column as usize].chars().count();
            writeln!(f, "\t | ")?;
            writeln!(f, "{}\t | {}", a.line, line)?;
            write!(f, "\t | ")?;
            for _ in 0..num_chars {
                write!(f, " ")?;
            }
            write!(f, "^")?;
            if let Some(m) = message {
                writeln!(f, " {}", m)?;
            } else {
                writeln!(f, "")?;
            }
            writeln!(f, "\t | ")?;
        }
        Ok(())
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error: ")?;
        match self.kind {
            ParseErrorKind::UnexpectedLineTerminator => {
                writeln!(
                    f,
                    "unexpected line terminator, syntax forbids line terminator at this point",
                )?;
                self.fmt_span(f, self.origin)?;
                self.fmt_span_src(f, self.origin, None)
            }
            ParseErrorKind::Todo { file, line } => {
                writeln!(
                    f,
                    "parser encountered an unimplemented path in: {}:{}, Sorry!",
                    file, line
                )?;
                self.fmt_span(f, self.origin)?;
                self.fmt_span_src(f, self.origin, None)
            }
            ParseErrorKind::NumberParseError { ref kind } => {
                writeln!(f, "encountered invalid number: {}", kind)
            }
            ParseErrorKind::UnexpectedToken {
                found,
                expected,
                reason,
            } => {
                found
                    .map(|found| write!(f, "unexpected token: found '{}' expected ", found.kind))
                    .unwrap_or_else(|| write!(f, "unexpected token: found 'EOF' expected"))?;
                if expected.len() > 1 {
                    write!(f, "one of: [")?;
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
                    write!(f, "'{}'", expected[0])?;
                } else {
                    write!(f, "'EOF'")?;
                }
                writeln!(f, "")?;
                self.fmt_span(f, self.origin)?;
                self.fmt_span_src(f, self.origin, reason)
            }
        }
    }
}
