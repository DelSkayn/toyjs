use crate::{Ctx, Value};

#[macro_export]
macro_rules! create_static_fn {
    ($ctx:expr,$func:ident) => {{
        mod wrap {
            #[inline]
            pub fn $func(
                realm: &vm::Realm,
                _: &mut vm::realm::ExecutionContext,
            ) -> Result<::vm::Value, ::vm::Value> {
                unsafe {
                    let ctx = $crate::Ctx::wrap(realm);
                    let args = $crate::Arguments::from_ctx(ctx);
                    super::$func(ctx, args)
                        .map(|x| x.into_vm())
                        .map_err(|x| x.into_vm(ctx))
                }
            }
        }

        unsafe { $ctx.create_static_function(wrap::$func) }
    }};
}

#[derive(Clone, Copy)]
pub struct Arguments<'js> {
    len: usize,
    ctx: Ctx<'js>,
}

impl<'js> Arguments<'js> {
    #[doc(hidden)]
    pub unsafe fn from_ctx(ctx: Ctx<'js>) -> Self {
        Arguments {
            len: (*ctx.ctx).stack.frame_size(),
            ctx,
        }
    }

    pub fn get(self, idx: usize) -> Option<Value<'js>> {
        if idx > u8::MAX as usize || idx >= self.len {
            return None;
        }
        unsafe {
            let val = (*self.ctx.ctx).stack.read(idx as u8);
            Some(Value::wrap(self.ctx, val))
        }
    }
}
