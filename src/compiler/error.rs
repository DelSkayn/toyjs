use crate::source::Span;
use std::fmt;

pub enum CompilerErrorKind {
    Todo { file: &'static str, line: u32 },
}

impl fmt::Display for CompilerErrorKind {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _ => {}
        }
        Ok(())
    }
}

pub struct CompilerError {
    loc: Option<Span>,
    kind: CompilerErrorKind,
    source: Source,
}
