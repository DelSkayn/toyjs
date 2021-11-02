use crate::{
    gc::Trace,
    instructions::{ByteCode, InstructionReader},
    object::Object,
    realm::Realm,
    stack::Stack,
    Gc, JSValue,
};
use std::cell::RefCell;

pub struct Arguments<'a> {
    stack: &'a Stack,
}

pub type MutableFn = Box<dyn FnMut(&mut Realm) -> JSValue>;
pub type NativeFn = Box<dyn Fn(&mut Realm) -> JSValue>;

pub enum FunctionKind {
    Runtime { bc: Gc<ByteCode>, function: usize },
    Mutable(RefCell<MutableFn>),
    Native(NativeFn),
}

pub struct Function {
    kind: FunctionKind,
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
    pub unsafe fn from_native_mut<F>(f: F) -> Self
    where
        F: FnMut(&mut Realm) -> JSValue + 'static,
    {
        let kind = FunctionKind::Mutable(RefCell::new(Box::new(f)));
        Function {
            kind,
            object: Object::new(),
        }
    }

    pub unsafe fn from_native<F>(f: F) -> Self
    where
        F: Fn(&mut Realm) -> JSValue + 'static,
    {
        let kind = FunctionKind::Native(Box::new(f));
        Function {
            kind,
            object: Object::new(),
        }
    }

    pub fn from_bc(bc: Gc<ByteCode>, func: usize) -> Function {
        Function {
            kind: FunctionKind::Runtime { bc, function: func },
            object: Object::new(),
        }
    }

    pub unsafe fn call(&self, realm: &mut Realm) -> JSValue {
        match self.kind {
            FunctionKind::Runtime { bc, function } => {
                let func = bc.functions[function];
                realm.stack.enter_call(func.registers);
                let mut reader = InstructionReader::new(&bc.instructions, func.offset, func.size);
                let res = realm.execute(&mut reader, bc);
                realm.stack.exit_call();
                res
            }
            FunctionKind::Native(ref func) => (**func)(realm),
            FunctionKind::Mutable(ref func) => {
                (**func
                    .try_borrow_mut()
                    .expect("recursively called mutable native function"))(realm)
            }
        }
    }

    pub fn as_object(&self) -> &Object {
        &self.object
    }
}
