use std::{backtrace::Backtrace, fmt, result::Result as StdResult};

use common::{result::ContextError, source::Source, span::Span, string::Ascii};

pub type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub enum Error {
    Limit(Limits),
    NotImplemented(Backtrace),
    Redeclared { span: Span, first_declared: Span },
}

#[derive(Debug)]
pub enum Limits {
    BytecodeSize,
    JumpOffset,
    TooManyScopes,
    TooManyVariables,
    Registers,
    Functions,
    Strings,
}

impl fmt::Display for Limits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Limits::BytecodeSize => write!(
                f,
                "script required more then u32::MAX bytes of instructions"
            ),
            Limits::JumpOffset => {
                write!(f, "script required a jump which exceeded the maximum jump offset allowed by the instruction set.")
            }
            Limits::TooManyScopes => {
                write!(f, "script has more scopes than the limit of u32::MAX - 1")
            }
            Limits::TooManyVariables => {
                write!(f, "script has more symbols than the limit of u32::MAX - 1")
            }
            Limits::Registers => write!(
                f,
                "function in script required more registers then the instruction set limit of 127"
            ),
            Limits::Functions => write!(f, "number of functions in script exceeded limit"),
            Limits::Strings => write!(f, "number of strings in script exceeded limit"),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Limit(limit) => {
                write!(f, "Code exceeded interpreter limits: {limit}")
            }
            Error::NotImplemented(b) => {
                write!(f, "Compiler hit path which was not yet implemented:\n{b}")
            }
            Error::Redeclared { .. } => {
                writeln!(f, "Already existing variable was redeclared")
            }
        }
    }
}

impl ContextError<Source> for Error {
    fn display(&self, f: &mut fmt::Formatter, ctx: &Source) -> fmt::Result {
        match self {
            Error::Limit(limit) => {
                write!(f, "Code exceeded interpreter limits: {limit}")
            }
            Error::NotImplemented(b) => {
                write!(f, "Compiler hit path which was not yet implemented:\n{b}")
            }
            Error::Redeclared {
                span,
                first_declared,
            } => {
                writeln!(f, "Already existing variable was redeclared")?;
                writeln!(
                    f,
                    "{}",
                    ctx.render_span(*span, None)
                        .map_err(|_| fmt::Error)?
                        .as_block()
                )?;
                writeln!(
                    f,
                    "{}",
                    ctx.render_span(
                        *first_declared,
                        Some(Ascii::const_from_str("Was first declared here").into()),
                    )
                    .map_err(|_| fmt::Error)?
                    .as_block()
                )
            }
        }
    }
}
