use crate::{
    source::{Source, Span},
    token::Token,
};
use std::fmt;

pub enum ParseErrorKind<'a> {
    UnexpectedLineTerminator,
    Todo {
        file: &'static str,
        line: u32,
    },
    UnexpectedToken {
        found: Option<Token<'a>>,
        expected: &'static [&'static str],
    },
}

impl<'a> fmt::Display for ParseErrorKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ParseErrorKind::UnexpectedLineTerminator => write!(
                f,
                "unexpected line terminator, syntax forbids line terminator at this point",
            ),
            ParseErrorKind::Todo { file, line } => write!(
                f,
                "parser encountered an unimplemented path in: {}:{}, Sorry!",
                file, line
            ),
            ParseErrorKind::UnexpectedToken { found, expected } => {
                found
                    .map(|found| write!(f, "unexpected token: found '{}' expected", found.kind))
                    .unwrap_or_else(|| write!(f, "unexpected token: found 'EOF' expected"))?;
                if expected.len() > 1 {
                    write!(f, " on of: [")?;
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
                Ok(())
            }
        }
    }
}

pub struct ParseError<'a> {
    pub kind: ParseErrorKind<'a>,
    pub origin: Span,
    pub source: Source<'a>,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}", self.kind)?;
        if let Some((a, _)) = self.source.get_source_position(self.origin) {
            writeln!(f, "  --> {}:{}:{}", "??", a.line, a.column)?;
            let line = self.source.line(a.line);
            let num_chars = line[0 as usize..a.column as usize].chars().count();
            writeln!(f, "\t | ")?;
            writeln!(f, "{}\t | {}", a.line, line)?;
            write!(f, "\t | ")?;
            for _ in 0..num_chars {
                write!(f, " ")?;
            }
            writeln!(f, "^")?;
            writeln!(f, "\t | ")?;
        } else {
            writeln!(f, "  --> {}:??:??", "??")?;
        }
        Ok(())
    }
}
