use crate::{Ctx, Function, Object, String};

#[derive(Debug, Clone, Copy)]
pub struct Value<'js> {
    pub(crate) ctx: Ctx<'js>,
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
            value: vm::Value::nan(),
        }
    }

    pub fn is_undefined(self) -> bool {
        self.value.is_undefined()
    }

    pub fn is_null(self) -> bool {
        self.value.is_null()
    }

    pub fn is_nan(self) -> bool {
        if let Some(x) = self.value.into_float() {
            x.is_nan()
        } else {
            false
        }
    }

    pub fn is_falseish(self) -> bool {
        unsafe { (*self.ctx.ctx).is_falsish(self.value) }
    }

    pub fn into_f64(self) -> Option<f64> {
        self.value.into_float()
    }

    pub fn into_i32(self) -> Option<i32> {
        self.value.into_int()
    }

    pub fn into_object(self) -> Option<Object<'js>> {
        self.value
            .into_object()
            .map(|o| unsafe { Object::wrap(self.ctx, o) })
    }

    pub fn into_string(self) -> Option<String<'js>> {
        self.value
            .into_string()
            .map(|s| unsafe { String::wrap(self.ctx, s) })
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
