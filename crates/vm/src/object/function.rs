use crate::{
    cell::CellOwner,
    gc::{Arena, Rebind, Trace, Tracer},
    instructions::GcByteCode,
    realm::{ExecutionContext, GcRealm, GcUpvalueObject},
    Value,
};
use common::atom::Atoms;

use super::{Object, ObjectKind};

pub const RECURSIVE_FUNC_PANIC: &str = "tried to call mutable function recursively";

pub type MutableFn = Box<
    dyn for<'gc, 'cell> FnMut(
        &mut Arena<'gc, 'cell>,
        &mut CellOwner<'cell>,
        &Atoms,
        GcRealm<'gc, 'cell>,
        &ExecutionContext<'gc, 'cell>,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>>,
>;
pub type SharedFn = Box<
    dyn for<'a, 'gc, 'cell> Fn(
        &mut Arena<'gc, 'cell>,
        &mut CellOwner<'cell>,
        &Atoms,
        GcRealm<'gc, 'cell>,
        &ExecutionContext<'gc, 'cell>,
    ) -> Result<Value<'gc, 'cell>, Value<'gc, 'cell>>,
>;

pub type StaticFn = for<'l, 'cell> unsafe fn(
    &'l mut Arena<'_, 'cell>,
    &mut CellOwner<'cell>,
    &Atoms,
    GcRealm<'_, 'cell>,
    &ExecutionContext<'_, 'cell>,
) -> Result<Value<'l, 'cell>, Value<'l, 'cell>>;

#[derive(Debug)]
pub struct VmFunction<'gc, 'cell> {
    pub bc: GcByteCode<'gc, 'cell>,
    pub function: u16,
    pub upvalues: Box<[GcUpvalueObject<'gc, 'cell>]>,
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
        self.upvalues.iter().copied().for_each(|x| ctx.mark(x));
    }
}

unsafe impl<'a, 'gc, 'cell> Rebind<'a> for VmFunction<'gc, 'cell> {
    type Output = VmFunction<'a, 'cell>;
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
