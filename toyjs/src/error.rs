use std::fmt;

use vm::{cell::CellOwner, gc::Arena};

use crate::{Ctx, Value};

pub type Result<'js, T> = std::result::Result<T, Error<'js>>;

pub enum Error<'js> {
    Syntax(String),
    Type(String),
    Value(Value<'js>),
    Conversion {
        found: &'static str,
        expected: &'static str,
    },
}

impl<'js> Error<'js> {
    pub(crate) fn from_vm(ctx: Ctx<'js>, value: vm::Value<'_, 'js>) -> Self {
        Error::Value(Value::from_vm(ctx, value))
    }

    #[doc(hidden)]
    pub fn into_vm(self, ctx: Ctx<'js>) -> vm::Value<'js, 'js> {
        let arena = unsafe { Arena::new_unchecked(ctx.context.root) };
        let mut owner = unsafe { CellOwner::new(ctx.id) };
        match self {
            Error::Value(x) => x.into_vm(),
            Error::Syntax(x) => {
                let v =
                    ctx.context
                        .realm
                        .create_syntax_error(&mut owner, &arena, ctx.context.atoms, x);
                ctx.root_value(v)
            }
            Error::Type(x) => {
                let v =
                    ctx.context
                        .realm
                        .create_syntax_error(&mut owner, &arena, ctx.context.atoms, x);
                ctx.root_value(v)
            }
            Error::Conversion { found, expected } => {
                let v = ctx.context.realm.create_syntax_error(
                    &mut owner,
                    &arena,
                    ctx.context.atoms,
                    format!("Could not convert from `{}` to `{}`", found, expected),
                );
                ctx.root_value(v)
            }
        }
    }
}

impl<'js> std::error::Error for Error<'js> {}

impl<'js> fmt::Debug for Error<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Value(x) => {
                let ctx = x.ctx;
                if let Ok(x) = ctx.to_string(*x) {
                    write!(f, "Uncaught {}", x.as_str())?;
                } else {
                    write!(f, "Uncaught {:?}", x)?;
                }
            }
            Error::Syntax(x) => {
                write!(f, "Uncaught SyntaxError: {}", x)?;
            }
            Error::Type(x) => {
                write!(f, "Uncaught TypeError: {}", x)?;
            }
            Error::Conversion { found, expected } => {
                write!(
                    f,
                    "Uncaught TypeError: Could not convert from `{}` to `{}`",
                    found, expected
                )?;
            }
        }
        Ok(())
    }
}

impl<'js> fmt::Display for Error<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Value(x) => {
                let ctx = x.ctx;
                if let Ok(x) = ctx.to_string(*x) {
                    write!(f, "Uncaught {}", x.as_str())?;
                } else {
                    write!(f, "Uncaught {:?}", x)?;
                }
            }
            Error::Syntax(x) => {
                write!(f, "Uncaught SyntaxError: {}", x)?;
            }
            Error::Type(x) => {
                write!(f, "Uncaught TypeError: {}", x)?;
            }
            Error::Conversion { found, expected } => {
                write!(
                    f,
                    "Uncaught TypeError: Could not convert from `{}` to `{}`",
                    found, expected
                )?;
            }
        }
        Ok(())
    }
}
