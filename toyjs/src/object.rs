use vm::{cell::CellOwner, gc::Arena, object::PropertyFlags, GcObject};

use crate::{atom::IntoAtom, convert::IntoJs, Ctx, Error, FromJs, Result, Value};

#[derive(Clone, Copy)]
pub struct Object<'js> {
    pub(crate) ptr: GcObject<'js, 'js>,
    pub(crate) ctx: Ctx<'js>,
}

impl<'js> Object<'js> {
    pub(crate) fn from_vm(ctx: Ctx<'js>, ptr: GcObject<'_, 'js>) -> Self {
        unsafe {
            ctx.context.root.push(ptr);
            Object {
                ctx,
                ptr: vm::gc::rebind(ptr),
            }
        }
    }

    pub(crate) fn into_vm(self) -> GcObject<'js, 'js> {
        self.ptr
    }

    pub fn into_value(self) -> Value<'js> {
        Value {
            value: self.ptr.into(),
            ctx: self.ctx,
        }
    }

    pub fn get<K, R>(self, key: K) -> Result<'js, R>
    where
        K: IntoAtom<'js>,
        R: FromJs<'js>,
    {
        let mut owner = unsafe { CellOwner::new(self.ctx.id) };
        let mut arena = unsafe { Arena::new_unchecked(self.ctx.context.root) };

        let key = key.into_atom(self.ctx)?;

        let res = self.ptr.index(
            &mut owner,
            &mut arena,
            self.ctx.context.atoms,
            self.ctx.context.realm,
            key.atom,
        );
        match res {
            Ok(x) => R::from_js(self.ctx, Value::from_vm(self.ctx, x)),
            Err(e) => Err(Error::from_vm(self.ctx, e)),
        }
    }

    pub fn set<K, V>(self, key: K, value: V) -> Result<'js, ()>
    where
        K: IntoAtom<'js>,
        V: IntoJs<'js>,
    {
        let mut owner = unsafe { CellOwner::new(self.ctx.id) };
        let mut arena = unsafe { Arena::new_unchecked(self.ctx.context.root) };

        let key = key.into_atom(self.ctx)?;
        let value = value.into_js(self.ctx)?.into_vm();

        let res = self.ptr.index_set(
            &mut owner,
            &mut arena,
            self.ctx.context.atoms,
            self.ctx.context.realm,
            key.atom,
            value,
        );

        match res {
            Ok(()) => Ok(()),
            Err(e) => Err(Error::from_vm(self.ctx, e)),
        }
    }

    pub fn raw_set_flags<K, V>(self, key: K, value: V, flags: PropertyFlags) -> Result<'js, ()>
    where
        K: IntoAtom<'js>,
        V: IntoJs<'js>,
    {
        let mut owner = unsafe { CellOwner::new(self.ctx.id) };
        let arena = unsafe { Arena::new_unchecked(self.ctx.context.root) };

        let key = key.into_atom(self.ctx)?;
        let value = value.into_js(self.ctx)?.into_vm();

        self.ptr.raw_index_set_flags(
            &mut owner,
            &arena,
            self.ctx.context.atoms,
            key.atom,
            value,
            flags,
        );
        std::mem::forget(key);

        Ok(())
    }
}
