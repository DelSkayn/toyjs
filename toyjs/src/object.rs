use vm::Gc;

use crate::{
    convert::{FromJs, IntoJs},
    Ctx, Value,
};

#[derive(Clone, Copy)]
pub struct Object<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<vm::Object>,
}

impl<'js> Object<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<vm::Object>) -> Self {
        Object { ctx, ptr }
    }

    pub fn get<K, V>(self, key: K) -> V
    where
        K: IntoJs<'js>,
        V: FromJs<'js>,
    {
        unsafe {
            let v = self
                .ptr
                .index(key.into_js(self.ctx).into_vm(), &mut (*self.ctx.ctx).realm);
            self.ctx.push_value(v);
            V::from_js(self.ctx, Value::wrap(self.ctx, v))
        }
    }

    pub fn set<K, V>(self, key: K, value: V)
    where
        K: IntoJs<'js>,
        V: IntoJs<'js>,
    {
        unsafe {
            self.ptr.index_set(
                key.into_js(self.ctx).into_vm(),
                value.into_js(self.ctx).into_vm(),
                &mut (*self.ctx.ctx).realm,
            );
        }
    }
}
