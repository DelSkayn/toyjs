use crate::{error::Result, Atom, Ctx, Error, Function, Object, String, Value};
use std::string::String as StdString;

/// A trait to convert javascript Values to rust.
///
/// # Safety
///
/// The value of `NEEDS_GC` must be true if self contains a gc collected pointer.
pub unsafe trait FromJs<'js>: Sized {
    const NEEDS_GC: bool;

    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self>;
}

unsafe impl<'js> FromJs<'js> for Value<'js> {
    const NEEDS_GC: bool = true;
    fn from_js(_: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        Ok(value)
    }
}

unsafe impl<'js> FromJs<'js> for String<'js> {
    const NEEDS_GC: bool = true;
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        ctx.coerce_string(value)
    }
}

unsafe impl<'js> FromJs<'js> for f64 {
    const NEEDS_GC: bool = false;
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        let value = ctx.coerce_number(value)?;
        Ok(value
            .into_f64()
            .or_else(|| value.into_i32().map(|x| x.into()))
            .expect("coerce number did not return a number"))
    }
}

unsafe impl<'js> FromJs<'js> for i32 {
    const NEEDS_GC: bool = false;
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        let value = ctx.coerce_integer(value)?;
        Ok(value)
    }
}

unsafe impl<'js> FromJs<'js> for () {
    const NEEDS_GC: bool = true;
    fn from_js(_: Ctx<'js>, _: Value<'js>) -> Result<'js, Self> {
        Ok(())
    }
}

pub trait IntoJs<'js> {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js>;
}

impl<'js> IntoJs<'js> for Value<'js> {
    fn into_js(self, _: Ctx<'js>) -> Value<'js> {
        self
    }
}

impl<'js> IntoJs<'js> for String<'js> {
    fn into_js(self, _: Ctx<'js>) -> Value<'js> {
        self.into()
    }
}

impl<'js> IntoJs<'js> for Object<'js> {
    fn into_js(self, _: Ctx<'js>) -> Value<'js> {
        self.into()
    }
}

impl<'js> IntoJs<'js> for Function<'js> {
    fn into_js(self, _: Ctx<'js>) -> Value<'js> {
        self.into()
    }
}

impl<'js> IntoJs<'js> for &str {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js> {
        ctx.create_string(self).into()
    }
}

impl<'js> IntoJs<'js> for StdString {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js> {
        ctx.create_string(self).into()
    }
}

impl<'js> IntoJs<'js> for f64 {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js> {
        unsafe { Value::wrap(ctx, vm::Value::from(self)) }
    }
}

impl<'js> IntoJs<'js> for i32 {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js> {
        unsafe { Value::wrap(ctx, vm::Value::from(self)) }
    }
}

impl<'js> IntoJs<'js> for bool {
    fn into_js(self, ctx: Ctx<'js>) -> Value<'js> {
        unsafe { Value::wrap(ctx, vm::Value::from(self)) }
    }
}

pub trait IntoAtom<'js> {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<Atom<'js>>;
}

impl<'js> IntoAtom<'js> for i32 {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<Atom<'js>> {
        unsafe {
            let atom = (*ctx.ctx)
                .vm()
                .atomize(self.into(), &(*ctx.ctx))
                .map_err(|e| Error::wrap(ctx, e))?;
            Ok(Atom::wrap(ctx, atom))
        }
    }
}

impl<'js> IntoAtom<'js> for f64 {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<Atom<'js>> {
        unsafe {
            let atom = (*ctx.ctx)
                .vm()
                .atomize(self.into(), &(*ctx.ctx))
                .map_err(|e| Error::wrap(ctx, e))?;
            Ok(Atom::wrap(ctx, atom))
        }
    }
}

impl<'js> IntoAtom<'js> for &str {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<Atom<'js>> {
        unsafe {
            let atom = (*ctx.ctx).vm().atomize_string(self);
            Ok(Atom::wrap(ctx, atom))
        }
    }
}

impl<'js> IntoAtom<'js> for Value<'js> {
    fn into_atom(self, ctx: Ctx<'js>) -> Result<Atom<'js>> {
        unsafe {
            let atom = (*ctx.ctx)
                .vm()
                .atomize(self.into_vm(), &(*ctx.ctx))
                .map_err(|e| Error::wrap(ctx, e))?;
            Ok(Atom::wrap(ctx, atom))
        }
    }
}
