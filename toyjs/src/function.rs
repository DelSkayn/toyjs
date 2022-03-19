use vm::Gc;

use crate::{Ctx, Value};

#[derive(Clone, Copy)]
pub struct Function<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<vm::Object>,
}

impl<'js> Function<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<vm::Object>) -> Self {
        Function { ctx, ptr }
    }

    pub fn call(self) -> Result<Value<'js>, Value<'js>> {
        unsafe {
            (*self.ctx.ctx)
                .enter_call(self.ptr)
                .map(|x| {
                    self.ctx.push_value(x);
                    Value::wrap(self.ctx, x)
                })
                .map_err(|x| {
                    self.ctx.push_value(x);
                    Value::wrap(self.ctx, x)
                })
        }
    }
}
