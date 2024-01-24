
use dreck::{Bound, Owner, Root, Trace, Tracer};

use crate::{
    instructions::GcByteCode,
    realm::{ExecutionContext, GcRealm, GcUpvalueObject},
    GcObject, Value,
};
use common::atom::Atoms;

use super::{Object, ObjectKind};

pub enum FunctionKind<'gc, 'own, 'a> {
    None,
    VmFn(&'a VmFunction<'gc, 'own>),
    SharedFn(&'a SharedFn),
    StaticFn(StaticFn),
}

pub type SharedFn = Box<
    dyn for<'l, 'own> Fn(
        &'l mut Root<'own>,
        &mut Owner<'own>,
        &Atoms,
        GcRealm<'_, 'own>,
        &ExecutionContext<'_, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>>,
>;

pub type StaticFn = for<'l, 'own> unsafe fn(
    &'l mut Root<'own>,
    &mut Owner<'own>,
    &Atoms,
    GcRealm<'_, 'own>,
    &ExecutionContext<'_, 'own>,
) -> Result<Value<'l, 'own>, Value<'l, 'own>>;

pub struct VmFunction<'gc, 'own> {
    pub bc: GcByteCode<'gc, 'own>,
    pub function: u16,
    pub upvalues: Box<[GcUpvalueObject<'gc, 'own>]>,
}

unsafe impl<'gc, 'own> Trace<'own> for VmFunction<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, ctx: Tracer<'a,'own>) {
        ctx.mark(self.bc);
        self.upvalues.iter().copied().for_each(|x| ctx.mark(x));
    }
}

unsafe impl<'a, 'gc, 'own> Bound<'a> for VmFunction<'gc, 'own> {
    type Rebound = VmFunction<'a, 'own>;
}

impl<'gc, 'own> Object<'gc, 'own> {
    #[inline]
    pub(crate) fn as_vm_function(&self) -> &VmFunction<'gc, 'own> {
        match self.kind {
            ObjectKind::VmFn(ref x) => x,
            _ => panic!("not a vm function"),
        }
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        matches!(
            self.kind,
            ObjectKind::VmFn(_) | ObjectKind::SharedFn(_) | ObjectKind::StaticFn(_)
        )
    }
}

impl<'gc, 'own> Object<'gc, 'own> {
    /// TODO: Safety is still kinda unsure.
    pub fn as_function_kind<'l>(
        this: &'l GcObject<'gc,'own>,
        _owner: &Owner<'own>,
    ) -> FunctionKind<'gc, 'own, 'l> {
        todo!()
    }
}
