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

    pub fn is_falsish(self) -> bool {
        self.value.is_false() || self.value.is_null() || self.value.is_undefined()
    }

    pub fn undefined(ctx: Ctx<'js>) -> Self {
        Value {
            ctx,
            value: vm::Value::undefined(),
        }
    }
}
