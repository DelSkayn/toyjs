use crate::{Ctx, Function, Object, String};

#[derive(Debug)]
pub struct Value<'js> {
    ctx: Ctx<'js>,
    value: vm::Value,
}

impl<'js> Value<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, val: vm::Value) -> Self {
        Value { ctx, value: val }
    }

    pub(crate) fn into_vm(self) -> vm::Value {
        self.value
    }

    pub fn undefined(ctx: Ctx<'js>) -> Value<'js> {
        Value {
            ctx,
            value: vm::Value::undefined(),
        }
    }

    pub fn null(ctx: Ctx<'js>) -> Value<'js> {
        Value {
            ctx,
            value: vm::Value::null(),
        }
    }

    pub fn nan(ctx: Ctx<'js>) -> Value<'js> {
        Value {
            ctx,
            value: f64::NAN.into(),
        }
    }

    pub fn is_undefined(self) -> bool {
        self.value.is_undefined()
    }

    pub fn is_null(self) -> bool {
        self.value.is_null()
    }
}

impl<'js> From<String<'js>> for Value<'js> {
    fn from(s: String<'js>) -> Self {
        Value {
            ctx: s.ctx,
            value: s.ptr.into(),
        }
    }
}

impl<'js> From<Function<'js>> for Value<'js> {
    fn from(s: Function<'js>) -> Self {
        Value {
            ctx: s.ctx,
            value: s.ptr.into(),
        }
    }
}

impl<'js> From<Object<'js>> for Value<'js> {
    fn from(s: Object<'js>) -> Self {
        Value {
            ctx: s.ctx,
            value: s.ptr.into(),
        }
    }
}
