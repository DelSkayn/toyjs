use vm::Gc;

use crate::{convert::FromJs, Ctx, Error, Result, Value};

#[derive(Clone, Copy)]
pub struct Function<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<vm::Object>,
}

impl<'js> Function<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<vm::Object>) -> Self {
        Function { ctx, ptr }
    }

    pub fn call<R: FromJs<'js>>(self) -> Result<'js, R> {
        unsafe {
            let v = (*self.ctx.ctx).enter_call(self.ptr).map_err(|x| {
                self.ctx.push_value(x);
                Error::wrap(self.ctx, x)
            })?;
            if R::NEEDS_GC {
                self.ctx.push_value(v);
            }
            R::from_js(self.ctx, Value::wrap(self.ctx, v))
        }
    }
}
