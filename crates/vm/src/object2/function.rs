use crate::{
    gc::Trace,
    instructions::ByteCode,
    realm::{ExecutionContext, UpvalueObject},
    Gc, Realm, Value,
};
use std::{cell::RefCell, fmt};

use super::{Object, ObjectFlags, ObjectKind};

pub const RECURSIVE_FUNC_PANIC: &str = "tried to call mutable function recursively";

pub type MutableFn =
    Box<dyn for<'a> FnMut(&'a Realm, &'a mut ExecutionContext) -> Result<Value, Value>>;
pub type SharedFn =
    Box<dyn for<'a> Fn(&'a Realm, &'a mut ExecutionContext) -> Result<Value, Value>>;
pub type StaticFn = unsafe fn(&Realm, &mut ExecutionContext) -> Result<Value, Value>;

pub struct VmFunction {
    pub bc: Gc<ByteCode>,
    pub function: u16,
    pub upvalues: Box<[Gc<UpvalueObject>]>,
}

unsafe impl Trace for VmFunction {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        #[cfg(feature = "dump-gc-trace")]
        println!("MARK: obj.function");
        ctx.mark(self.bc);
        self.upvalues.iter().copied().for_each(|x| ctx.mark(x));
    }
}

pub enum FunctionKind {
    Vm(VmFunction),
    Mutable(RefCell<MutableFn>),
    Shared(SharedFn),
    Static(StaticFn),
}

impl fmt::Debug for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "functionkind")
    }
}

impl Object {
    #[inline]
    pub(crate) fn as_vm_function(&self) -> &VmFunction {
        match self.kind {
            ObjectKind::VmFn(ref x) => x,
            _ => panic!("not a vm function"),
        }
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        match self.kind {
            ObjectKind::VmFn(_)
            | ObjectKind::SharedFn(_)
            | ObjectKind::StaticFn(_)
            | ObjectKind::MutableFn(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.flags.contains(ObjectFlags::CONSTRUCTOR)
    }
}
