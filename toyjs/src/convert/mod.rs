use crate::{Ctx, Function, Object, String, Value};
use std::string::String as StdString;

pub trait FromJs<'js> {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Self;
}

impl<'js> FromJs<'js> for Value<'js> {
    fn from_js(_: Ctx<'js>, value: Value<'js>) -> Self {
        value
    }
}

impl<'js> FromJs<'js> for f64 {
    fn from_js(ctx: Ctx<'js>, value: Value<'js>) -> Self {
        let value = ctx.coerce_number(value);
        value
            .into_f64()
            .or_else(|| value.into_i32().map(|x| x.into()))
            .expect("coerce number did not return a number")
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
