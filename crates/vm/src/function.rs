use crate::{
    gc::Trace,
    instructions::ByteCode,
    object::Object,
    realm::{Arguments, RealmCtx},
    value::BoundValue,
    Gc,
};
use std::cell::RefCell;

pub const RECURSIVE_FUNC_PANIC: &'static str = "tried to call mutable function recursively";

pub type MutableFn = Box<dyn for<'a> FnMut(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a>>;
pub type NativeFn = Box<dyn for<'a> Fn(RealmCtx<'a>, Arguments<'a>) -> BoundValue<'a>>;

pub enum FunctionKind {
    Runtime { bc: Gc<ByteCode>, function: u32 },
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
            FunctionKind::Runtime { bc, .. } => ctx.mark(bc),
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
    pub fn from_bc(bc: Gc<ByteCode>, func: u32) -> Function {
        Function {
            kind: FunctionKind::Runtime { bc, function: func },
            object: Object::new(),
        }
    }

    pub fn as_object(&self) -> &Object {
        &self.object
    }
}
