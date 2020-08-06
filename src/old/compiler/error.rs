use crate::source::{Source, Span};
use std::fmt;

#[derive(Debug)]
pub enum CompilerErrorKind {
    Todo,
}

impl fmt::Display for CompilerErrorKind {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match self {
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct CompilerError {
    pub span: Span,
    pub kind: CompilerErrorKind,
}

impl CompilerError {}
