use vm::Gc;

use crate::{Ctx, Value};

#[derive(Clone, Copy)]
pub struct Object<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<vm::Object>,
}

impl<'js> Object<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<vm::Object>) -> Self {
        Object { ctx, ptr }
    }

    pub fn get(self, key: Value<'js>) -> Value<'js> {
        unsafe {
            let v = self
                .ptr
                .unsafe_index(key.into_vm(), &mut (*self.ctx.ctx).realm);
            self.ctx.push_value(v);
            Value::wrap(self.ctx, v)
        }
    }

    pub fn set(self, key: Value<'js>, value: Value<'js>) {
        unsafe {
            self.ptr
                .unsafe_index_set(key.into_vm(), value.into_vm(), &mut (*self.ctx.ctx).realm);
        }
    }
}
