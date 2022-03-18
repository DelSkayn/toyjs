use common::collections::HashMap;

use crate::{instructions::ByteCode, object::Object, realm::UpvalueObject, Gc, Realm, Value};
use std::cell::{RefCell, UnsafeCell};

pub const RECURSIVE_FUNC_PANIC: &str = "tried to call mutable function recursively";

pub type MutableFn = Box<dyn FnMut(&mut Realm) -> Result<Value, Value>>;
pub type SharedFn = Box<dyn Fn(&mut Realm) -> Result<Value, Value>>;
pub type StaticFn = fn(&mut Realm) -> Result<Value, Value>;
pub type ConstructorFn = fn(&mut Realm, Value, Value) -> Result<Value, Value>;

pub struct VmFunction {
    pub bc: Gc<ByteCode>,
    pub function: u16,
    pub upvalues: Box<[Gc<UpvalueObject>]>,
}

pub enum FunctionKind {
    Vm(VmFunction),
    Mutable(RefCell<MutableFn>),
    Shared(SharedFn),
    Static(StaticFn),
    Constructor(ConstructorFn),
}

impl Object {
    pub fn new_function(function: FunctionKind, prototype: Option<Gc<Object>>) -> Self {
        Self {
            prototype,
            values: UnsafeCell::new(HashMap::default()),
            array: UnsafeCell::new(Vec::new()),
            function: Some(function),
        }
    }

    /// Create a function from a mutable rust closure.
    ///
    /// Mutable closures cannot be called recursively.
    /// The implementation will panic in the case that the function is called from within
    /// The function call.
    pub unsafe fn from_mutable<F>(realm: &Realm, f: F) -> Self
    where
        F: FnMut(&mut Realm) -> Result<Value, Value> + 'static,
    {
        let kind = FunctionKind::Mutable(RefCell::new(Box::new(f)));
        Self::new_function(kind, realm.builtin.function_proto)
    }

    /// Create a function from a immutable rust closure.
    pub unsafe fn from_shared<F>(realm: &Realm, f: F) -> Self
    where
        F: Fn(&mut Realm) -> Result<Value, Value> + 'static,
    {
        let kind = FunctionKind::Shared(Box::new(f));
        Self::new_function(kind, realm.builtin.function_proto)
    }

    /// Create a functions from a function in a bytecode set.
    pub fn from_static(realm: &Realm, vm: StaticFn) -> Self {
        let kind = FunctionKind::Static(vm);
        Self::new_function(kind, realm.builtin.function_proto)
    }

    /// Create a functions from a function in a bytecode set.
    pub fn from_constructor(realm: &Realm, func: ConstructorFn) -> Self {
        let kind = FunctionKind::Constructor(func);
        Self::new_function(kind, realm.builtin.function_proto)
    }

    /// Create a functions from a function in a bytecode set.
    pub fn from_vm(realm: &Realm, vm: VmFunction) -> Self {
        let kind = FunctionKind::Vm(vm);
        Self::new_function(kind, realm.builtin.function_proto)
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
