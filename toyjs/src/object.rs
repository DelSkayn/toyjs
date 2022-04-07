use vm::{object2::ObjectKind, Gc};

use crate::{
    convert::{FromJs, IntoAtom, IntoJs},
    error::Result,
    Ctx, Error, Value,
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
        matches!(self.ptr.kind, ObjectKind::Error)
    }

    pub fn get<K, V>(self, key: K) -> Result<'js, V>
    where
        K: IntoAtom<'js>,
        V: FromJs<'js>,
    {
        unsafe {
            let key = key.into_atom(self.ctx)?;
            let v = self
                .ptr
                .index(&(*self.ctx.ctx), key.into_vm())
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
        K: IntoAtom<'js>,
        V: IntoJs<'js>,
    {
        unsafe {
            let key = key.into_atom(self.ctx)?;
            self.ptr
                .index_set(
                    &(*self.ctx.ctx),
                    key.into_vm(),
                    value.into_js(self.ctx).into_vm(),
                )
                .map_err(|e| {
                    self.ctx.push_value(e);
                    Error::wrap(self.ctx, e)
                })?;
            Ok(())
        }
    }
}
