use crate::source::{Source, Span};
use std::fmt;

pub enum ParseErrorKind {
    Todo { file: &'static str, line: u32 },
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ParseErrorKind::Todo { file, line } => write!(
                f,
                "parser encountered an unimplemented path in: {}:{}, Sorry!",
                file, line
            ),
        }
    }
}

pub struct ParseError<'a> {
    pub kind: ParseErrorKind,
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
