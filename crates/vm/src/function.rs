use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::Object,
    realm::{Arguments, RealmCtx, UpvalueObject},
    value::BoundValue,
    Gc,
};
use std::cell::RefCell;

pub const RECURSIVE_FUNC_PANIC: &'static str = "tried to call mutable function recursively";

pub type MutableFn = Box<dyn for<'a> FnMut(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a>>;
pub type NativeFn = Box<dyn for<'a> Fn(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a>>;

pub struct VmFunction {
    pub bc: Gc<ByteCode>,
    pub function: u16,
    pub upvalues: Box<[Gc<UpvalueObject>]>,
}

pub enum FunctionKind {
    Vm(VmFunction),
    Mutable(RefCell<MutableFn>),
    Native(NativeFn),
}

pub struct Function {
    pub kind: FunctionKind,
    object: Object,
}

unsafe impl Trace for Function {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }
    fn trace(&self, ctx: crate::gc::Ctx) {
        match self.kind {
            FunctionKind::Vm(ref func) => {
                ctx.mark(func.bc);
                func.upvalues.iter().for_each(|x| ctx.mark(*x))
            }
            _ => {}
        }
        self.object.trace(ctx);
    }
}

impl Function {
    /// Create a function from a mutable rust closure.
    ///
    /// Mutable closures cannot be called recursively.
    /// The implementation will panic in the case that the function is called from within
    /// The function call.
    pub fn from_native_mut<F>(f: F) -> Self
    where
        F: for<'a> FnMut(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a> + 'static,
    {
        let kind = FunctionKind::Mutable(RefCell::new(Box::new(f)));
        Function {
            kind,
            object: Object::new(),
        }
    }

    /// Create a function from a immutable rust closure.
    pub fn from_native<F>(f: F) -> Self
    where
        F: for<'a> Fn(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a> + 'static,
    {
        let kind = FunctionKind::Native(Box::new(f));
        Function {
            kind,
            object: Object::new(),
        }
    }

    /// Create a functions from a function in a bytecode set.
    pub fn from_vm(vm: VmFunction) -> Self {
        Function {
            kind: FunctionKind::Vm(vm),
            object: Object::new(),
        }
    }

    pub fn as_object(&self) -> &Object {
        &self.object
    }

    pub(crate) fn as_vm_function(&self) -> &VmFunction {
        match self.kind {
            FunctionKind::Vm(ref x) => x,
            _ => panic!("not a vm function"),
        }
    }
}
