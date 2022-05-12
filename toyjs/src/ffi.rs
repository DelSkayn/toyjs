use vm::cell::CellOwner;

use crate::{Ctx, Value};

#[macro_export]
macro_rules! create_static_fn {
    ($ctx:expr,$func:ident) => {{
        mod wrap {
            #[inline]
            pub fn $func<'l, 'cell>(
                arena: &'l mut ::vm::gc::Arena<'_, 'cell>,
                _owner: &mut ::vm::cell::CellOwner<'cell>,
                atoms: &::common::atom::Atoms,
                realm: ::vm::realm::GcRealm<'_, 'cell>,
                _ctx: &::vm::realm::ExecutionContext<'_, 'cell>,
            ) -> Result<::vm::Value<'l, 'cell>, ::vm::Value<'l, 'cell>> {
                unsafe {
                    let roots = arena.roots();
                    let _guard = roots.frame();

                    let context = $crate::Context::construct(realm, arena.roots(), atoms);

                    let ctx = $crate::Ctx::wrap(&context);

                    let args = $crate::Arguments::from_ctx(ctx);
                    let res: Result<vm::Value, vm::Value> = super::$func(ctx, args)
                        .map(|x| x.into_vm())
                        .map_err(|x| x.into_vm(ctx));

                    std::mem::transmute(res)
                }
            }
        }

        $ctx.create_static_function(wrap::$func)
    }};
}

#[derive(Clone, Copy)]
pub struct Arguments<'js> {
    ctx: Ctx<'js>,
}

impl<'js> Arguments<'js> {
    #[doc(hidden)]
    pub unsafe fn from_ctx(ctx: Ctx<'js>) -> Self {
        Arguments { ctx }
    }

    pub fn get(self, idx: u32) -> Option<Value<'js>> {
        let owner = unsafe { CellOwner::new(self.ctx.id) };

        self.ctx.context.realm.arg(&owner, idx).map(|value| Value {
            value,
            ctx: self.ctx,
        })
    }
}
