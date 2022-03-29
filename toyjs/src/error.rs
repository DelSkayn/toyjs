use std::fmt;

use crate::{Ctx, Value};

pub type Result<'js, T> = std::result::Result<T, Error<'js>>;

#[derive(Debug)]
pub enum Error<'js> {
    Parse(String),
    Value(Value<'js>),
}

impl<'js> Error<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, value: vm::Value) -> Self {
        Error::Value(Value::wrap(ctx, value))
    }

    pub(crate) fn into_vm(self, _ctx: Ctx<'js>) -> vm::Value {
        match self {
            Error::Value(x) => x.into_vm(),
            _ => todo!(),
        }
    }
}

impl<'js> std::error::Error for Error<'js> {}

impl<'js> fmt::Display for Error<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Value(x) => {
                let ctx = x.ctx;
                if let Ok(x) = ctx.coerce_string(*x) {
                    writeln!(f, "Uncaught {}", x.as_str())?;
                } else {
                    writeln!(f, "Uncaught {:?}", x)?;
                }
            }
            Error::Parse(x) => {
                writeln!(f, "Uncaught SyntaxError: {}", x)?;
            }
        }
        Ok(())
    }
}
