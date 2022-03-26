use vm::Gc;

use crate::{
    convert::{FromJs, IntoJs},
    error::{Error, Result},
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

    pub(crate) unsafe fn into_vm(self) -> Gc<vm::Object> {
        self.ptr
    }

    pub fn is_error(self) -> bool {
        self.ptr.is_error()
    }

    pub fn get<K, V>(self, key: K) -> Result<'js, V>
    where
        K: IntoJs<'js>,
        V: FromJs<'js>,
    {
        unsafe {
            let v = self
                .ptr
                .index(key.into_js(self.ctx).into_vm(), &(*self.ctx.ctx))
                .map_err(|e| {
                    self.ctx.push_value(e);
                    Error::wrap(self.ctx, e)
                })?;
            if V::NEEDS_GC {
                self.ctx.push_value(v);
            }
            V::from_js(self.ctx, Value::wrap(self.ctx, v))
        }
    }

    pub fn set<K, V>(self, key: K, value: V) -> Result<'js, ()>
    where
        K: IntoJs<'js>,
        V: IntoJs<'js>,
    {
        unsafe {
            self.ptr
                .index_set(
                    key.into_js(self.ctx).into_vm(),
                    value.into_js(self.ctx).into_vm(),
                    &(*self.ctx.ctx),
                )
                .map_err(|e| {
                    self.ctx.push_value(e);
                    Error::wrap(self.ctx, e)
                })?;
            Ok(())
        }
    }
}
