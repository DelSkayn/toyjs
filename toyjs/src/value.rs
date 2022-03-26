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
        if self.value.is_float() {
            self.value.cast_float().is_nan()
        } else {
            false
        }
    }

    pub fn is_falseish(self) -> bool {
        unsafe { (*self.ctx.ctx).is_falsish(self.value) }
    }

    pub fn into_f64(self) -> Option<f64> {
        if self.value.is_float() {
            Some(self.value.cast_float())
        } else {
            None
        }
    }

    pub fn into_i32(self) -> Option<i32> {
        if self.value.is_int() {
            Some(self.value.cast_int())
        } else {
            None
        }
    }

    pub fn into_object(self) -> Option<Object<'js>> {
        if self.value.is_object() {
            unsafe { Some(Object::wrap(self.ctx, self.value.unsafe_cast_object())) }
        } else {
            None
        }
    }

    pub fn into_string(self) -> Option<String<'js>> {
        if self.value.is_string() {
            unsafe { Some(String::wrap(self.ctx, self.value.unsafe_cast_string())) }
        } else {
            None
        }
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
