use std::string::String as StdString;

use crate::{Ctx, Object, Result, String, Value};

pub unsafe trait FromJs<'js>: Sized {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self>;
}

pub trait IntoJs<'js> {
    fn into_js(self, ctx: Ctx<'js>) -> Result<'js, Value<'js>>;
}

unsafe impl<'js> FromJs<'js> for Value<'js> {
    fn from_js(_: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        Ok(value)
    }
}

unsafe impl<'js> FromJs<'js> for StdString {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        ctx.to_string(value).map(|x| x.into_string())
    }
}

unsafe impl<'js> FromJs<'js> for String<'js> {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        ctx.to_string(value)
    }
}

unsafe impl<'js> FromJs<'js> for &'js str {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Result<'js, Self> {
        ctx.to_string(value).map(|x| x.as_str())
    }
}

unsafe impl<'js> FromJs<'js> for () {
    fn from_js(_: Ctx<'js>, _: Value<'js>) -> Result<'js, Self> {
        Ok(())
    }
}

impl<'js> IntoJs<'js> for Object<'js> {
    fn into_js(self, _ctx: Ctx<'js>) -> Result<'js, Value<'js>> {
        Ok(Value {
            ctx: self.ctx,
            value: self.ptr.into(),
        })
    }
}
