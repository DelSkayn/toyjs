use vm::Gc;

use crate::Ctx;

#[derive(Clone, Copy)]
pub struct Function<'js> {
    pub(crate) ctx: Ctx<'js>,
    pub(crate) ptr: Gc<vm::Function>,
}

impl<'js> Function<'js> {
    pub(crate) unsafe fn wrap(ctx: Ctx<'js>, ptr: Gc<vm::Function>) -> Self {
        Function { ctx, ptr }
    }

    //pub fn call(self) -> Result<Value<'js>, Value<'js>> {}
}
