use crate::{
    gc::Trace, instructions::ByteCode, object::Object, realm::UpvalueObject, Gc, Realm, Value,
};
use std::cell::RefCell;

pub const RECURSIVE_FUNC_PANIC: &str = "tried to call mutable function recursively";

pub type MutableFn = Box<dyn FnMut(&mut Realm) -> Result<Value, Value>>;
pub type NativeFn = Box<dyn Fn(&mut Realm) -> Result<Value, Value>>;
pub type ConstructorFn = fn(&mut Realm, Value, Value) -> Result<Value, Value>;

pub struct VmFunction {
    pub bc: Gc<ByteCode>,
    pub function: u16,
    pub upvalues: Box<[Gc<UpvalueObject>]>,
}

pub enum FunctionKind {
    Vm(VmFunction),
    Mutable(RefCell<MutableFn>),
    Native(NativeFn),
    Constructor(ConstructorFn),
}

pub struct Function {
    pub kind: FunctionKind,
    pub constructor: bool,
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
        if let FunctionKind::Vm(ref func) = self.kind {
            ctx.mark(func.bc);
            func.upvalues.iter().for_each(|x| ctx.mark(*x))
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
    pub unsafe fn from_native_mut<F>(f: F) -> Self
    where
        F: FnMut(&mut Realm) -> Result<Value, Value> + 'static,
    {
        let kind = FunctionKind::Mutable(RefCell::new(Box::new(f)));
        Function {
            kind,
            constructor: false,
            object: Object::new(None),
        }
    }

    /// Create a function from a immutable rust closure.
    pub unsafe fn from_native<F>(f: F) -> Self
    where
        F: Fn(&mut Realm) -> Result<Value, Value> + 'static,
    {
        let kind = FunctionKind::Native(Box::new(f));
        Function {
            kind,
            constructor: false,
            object: Object::new(None),
        }
    }

    /// Create a functions from a function in a bytecode set.
    pub fn from_vm(vm: VmFunction) -> Self {
        Function {
            kind: FunctionKind::Vm(vm),
            constructor: false,
            object: Object::new(None),
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
