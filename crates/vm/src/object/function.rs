use common::collections::HashMap;

use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::Object,
    realm::{ExecutionContext, UpvalueObject},
    Gc, Realm, Value,
};
use std::{
    cell::{Cell, RefCell, UnsafeCell},
    fmt,
};

use super::ObjectFlags;

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

unsafe impl Trace for FunctionKind {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        if let FunctionKind::Vm(ref x) = self {
            #[cfg(feature = "dump-gc-trace")]
            println!("MARK: obj.function");
            ctx.mark(x.bc);
            x.upvalues.iter().copied().for_each(|x| ctx.mark(x));
        }
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
    pub fn new_function(
        prototype: Option<Gc<Object>>,
        flags: ObjectFlags,
        function: FunctionKind,
    ) -> Self {
        Self {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            flags,
            function: Some(function),
            map_backdown: Cell::new(None),
        }
    }

    pub unsafe fn alloc_function(
        realm: &Realm,
        prototype: Option<Gc<Object>>,
        flags: ObjectFlags,
        function: FunctionKind,
    ) -> Gc<Self> {
        realm
            .vm()
            .allocate(Self::new_function(prototype, flags, function))
    }

    /// Create a functions from a function in a bytecode set.
    pub fn new_constructor(prototype: Option<Gc<Object>>, function: FunctionKind) -> Self {
        Self::new_function(prototype, ObjectFlags::CONSTRUCTOR, function)
    }

    pub unsafe fn alloc_constructor(
        realm: &Realm,
        prototype: Option<Gc<Object>>,
        function: FunctionKind,
    ) -> Gc<Self> {
        realm
            .vm()
            .allocate(Self::new_constructor(prototype, function))
    }

    #[inline]
    pub(crate) fn as_vm_function(&self) -> &VmFunction {
        match self.function {
            Some(FunctionKind::Vm(ref x)) => x,
            _ => panic!("not a vm function"),
        }
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.function.is_some()
    }
}
