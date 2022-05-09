use crate::{Ctx, Object, String};

#[derive(Debug, Clone, Copy)]
pub struct Value<'js> {
    pub(crate) value: vm::Value<'js, 'js>,
    pub(crate) ctx: Ctx<'js>,
}

impl<'js> Value<'js> {
    pub(crate) fn from_vm(ctx: Ctx<'js>, v: vm::Value<'_, 'js>) -> Self {
        let value = ctx.root_value(v);
        Value { value, ctx }
    }

    #[doc(hidden)]
    pub fn into_vm(self) -> vm::Value<'js, 'js> {
        self.value
    }

    pub fn into_object(self) -> Option<Object<'js>> {
        if let Some(obj) = self.value.into_object() {
            Some(Object {
                ptr: obj,
                ctx: self.ctx,
            })
        } else {
            None
        }
    }

    pub fn into_string(self) -> Option<String<'js>> {
        if let Some(obj) = self.value.into_string() {
            Some(String {
                ptr: obj,
                ctx: self.ctx,
            })
        } else {
            None
        }
    }

    pub fn into_float(self) -> Option<f64> {
        self.value.into_float()
    }

    pub fn is_falsish(self) -> bool {
        self.value.is_false() || self.value.is_null() || self.value.is_undefined()
    }

    pub fn is_nan(self) -> bool {
        self.value.into_float().map_or(false, |x| x.is_nan())
    }

    pub fn undefined(ctx: Ctx<'js>) -> Self {
        Value {
            ctx,
            value: vm::Value::undefined(),
        }
    }

    pub fn null(ctx: Ctx<'js>) -> Self {
        Value {
            ctx,
            value: vm::Value::null(),
        }
    }

    pub fn nan(ctx: Ctx<'js>) -> Self {
        Value {
            ctx,
            value: vm::Value::nan(),
        }
    }
}

impl<'js> From<String<'js>> for Value<'js> {
    fn from(s: String<'js>) -> Self {
        Self {
            ctx: s.ctx,
            value: s.ptr.into(),
        }
    }
}

impl<'js> From<Object<'js>> for Value<'js> {
    fn from(obj: Object<'js>) -> Self {
        Self {
            ctx: obj.ctx,
            value: obj.ptr.into(),
        }
    }
}
