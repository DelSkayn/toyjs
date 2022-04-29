use crate::{
    gc::{Trace, Tracer},
    instructions::GcByteCode,
    realm::ExecutionContext,
    Realm, Value,
};

use super::{Object, ObjectKind};

pub const RECURSIVE_FUNC_PANIC: &str = "tried to call mutable function recursively";

pub type MutableFn = Box<
    dyn for<'a, 'gc, 'cell> FnMut(
        &'a Realm<'gc, 'cell>,
        &'a mut ExecutionContext<'gc, 'cell>,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>>,
>;
pub type SharedFn = Box<
    dyn for<'a, 'gc, 'cell> Fn(
        &'a Realm<'gc, 'cell>,
        &'a mut ExecutionContext<'gc, 'cell>,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>>,
>;

pub type StaticFn = for<'gc, 'cell> unsafe fn(
    &Realm<'gc, 'cell>,
    &mut ExecutionContext<'gc, 'cell>,
) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>>;

pub struct VmFunction<'gc, 'cell> {
    pub bc: GcByteCode<'gc, 'cell>,
    pub function: u16,
    //pub upvalues: Box<[Gc<UpvalueObject>]>,
}

unsafe impl<'gc, 'cell> Trace for VmFunction<'gc, 'cell> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Tracer) {
        ctx.mark(self.bc);
        //self.upvalues.iter().copied().for_each(|x| ctx.mark(x));
    }
}

impl<'gc, 'cell> Object<'gc, 'cell> {
    #[inline]
    pub(crate) fn as_vm_function(&self) -> &VmFunction<'gc, 'cell> {
        match self.kind {
            ObjectKind::VmFn(ref x) => x,
            _ => panic!("not a vm function"),
        }
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        matches!(
            self.kind,
            ObjectKind::VmFn(_)
                | ObjectKind::SharedFn(_)
                | ObjectKind::StaticFn(_)
                | ObjectKind::MutableFn(_)
        )
    }

    /*
    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.flags.contains(ObjectFlags::CONSTRUCTOR)
    }
    */
}
