use std::fmt;

use crate::{Ctx, Value};

pub type Result<'js, T> = std::result::Result<T, Error<'js>>;

#[derive(Debug)]
pub enum Error<'js> {
    Syntax(String),
    Type(String),
    Value(Value<'js>),
}

impl<'js> Error<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, value: vm::Value) -> Self {
        Error::Value(Value::wrap(ctx, value))
    }

    #[doc(hidden)]
    pub fn into_vm(self, ctx: Ctx<'js>) -> vm::Value {
        match self {
            Error::Value(x) => x.into_vm(),
            Error::Syntax(x) => unsafe { (*ctx.ctx).create_syntax_error(x) },
            Error::Type(x) => unsafe { (*ctx.ctx).create_type_error(x) },
        }
    }
}

impl<'js> std::error::Error for Error<'js> {}

impl<'js> fmt::Display for Error<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Value(x) => {
                todo!()
                /*
                let ctx = x.ctx;
                if let Ok(x) = ctx.coerce_string(*x) {
                    write!(f, "Uncaught {}", x.as_str())?;
                } else {
                    write!(f, "Uncaught {:?}", x)?;
                }
                    */
            }
            Error::Syntax(x) => {
                write!(f, "Uncaught SyntaxError: {}", x)?;
            }
            Error::Type(x) => {
                write!(f, "Uncaught TypeError: {}", x)?;
            }
        }
        Ok(())
    }
}
