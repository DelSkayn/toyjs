use crate::{Ctx, Value};

#[derive(Clone, Copy)]
pub struct Arguments<'js> {
    len: usize,
    ctx: Ctx<'js>,
}

impl<'js> Arguments<'js> {
    pub unsafe fn from_ctx(ctx: Ctx<'js>) -> Self {
        Arguments {
            len: (*ctx.ctx).realm.stack.frame_size(),
            ctx,
        }
    }

    pub fn get(self, idx: usize) -> Option<Value<'js>> {
        if idx > u8::MAX as usize || idx >= self.len {
            return None;
        }
        unsafe {
            let val = (*self.ctx.ctx).realm.stack.read(idx as u8);
            Some(Value::wrap(self.ctx, val))
        }
    }
}
