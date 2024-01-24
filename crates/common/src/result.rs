use core::fmt;
use std::error::Error;

/// A trait for errors which need context to be able to display.
pub trait ContextError<Ctx> {
    fn display(&self, f: &mut fmt::Formatter, ctx: &Ctx) -> fmt::Result;

    fn supply_context(self, ctx: &Ctx) -> SuppliedError<Ctx, Self>
    where
        Self: Sized,
    {
        SuppliedError {
            error: self,
            context: ctx,
        }
    }
}

#[derive(Clone, Copy)]
pub struct SuppliedError<'a, C, E> {
    error: E,
    context: &'a C,
}

impl<'a, C, E: fmt::Debug> fmt::Debug for SuppliedError<'a, C, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.fmt(f)
    }
}

impl<'a, C, E: ContextError<C>> fmt::Display for SuppliedError<'a, C, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.error.display(f, self.context)
    }
}

impl<C, E: ContextError<C> + fmt::Debug> Error for SuppliedError<'_, C, E> {}

/// Trait for extending results.
pub trait ContextResultExt<'a, Ctx> {
    type Output;

    fn supply_context(self, c: &'a Ctx) -> Self::Output;
}

impl<'a, C: 'a, T, E: ContextError<C>> ContextResultExt<'a, C> for Result<T, E> {
    type Output = Result<T, SuppliedError<'a, C, E>>;

    fn supply_context(self, c: &'a C) -> Self::Output {
        match self {
            Ok(x) => Ok(x),
            Err(error) => Err(SuppliedError { error, context: c }),
        }
    }
}
