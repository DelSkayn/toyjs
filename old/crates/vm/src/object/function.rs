use dreck::{Bound, Owner, Trace, Tracer};

use crate::{
    exec::ExecutionContext, instructions::GcByteCode, realm::stack::GcUpvalueObject, value::Value,
};

use super::{GcObject, Object, ObjectKind};

pub enum FunctionKind<'gc, 'own, 'a> {
    None,
    VmFn(&'a VmFunction<'gc, 'own>),
    //SharedFn(&'a SharedFn),
    StaticFn(StaticFn),
}

pub type SharedFn = Box<
    dyn for<'r, 'gc, 'own> Fn(
        &'r mut ExecutionContext<'gc, 'own>,
    ) -> Result<Value<'r, 'own>, Value<'r, 'own>>,
>;

pub type StaticFn = for<'r, 'gc, 'own> fn(
    &'r mut ExecutionContext<'gc, 'own>,
) -> Result<Value<'r, 'own>, Value<'r, 'own>>;

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

    fn trace<'a>(&self, ctx: Tracer<'a, 'own>) {
        ctx.mark(self.bc);
        self.upvalues.iter().copied().for_each(|x| ctx.mark(x));
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for VmFunction<'from, 'own> {
    type Rebound = VmFunction<'to, 'own>;
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
            ObjectKind::VmFn(_) /*| ObjectKind::SharedFn(_) */ | ObjectKind::StaticFn(_)
        )
    }
}

impl<'gc, 'own> Object<'gc, 'own> {
    /// TODO: Safety is still kinda unsure.
    pub fn as_function_kind<'l>(
        this: &'l GcObject<'gc, 'own>,
        _owner: &Owner<'own>,
    ) -> FunctionKind<'gc, 'own, 'l> {
        todo!()
    }
}
