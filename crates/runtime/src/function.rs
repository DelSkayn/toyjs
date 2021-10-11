use crate::{
    gc::Trace, instructions::InstructionReader, object::Object, ByteCode, ByteFunction, Gc,
    JSValue, Realm,
};

pub struct Function {
    bc: Gc<ByteCode>,
    byte_func: ByteFunction,
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
        ctx.mark(self.bc);
        self.object.trace(ctx);
    }
}

impl Function {
    pub fn from_bc(bc: Gc<ByteCode>, func: usize) -> Function {
        Function {
            bc,
            byte_func: bc.functions[func],
            object: Object::new(),
        }
    }

    pub unsafe fn call(&self, realm: &mut Realm) -> JSValue {
        realm.stack.enter_call(self.byte_func.registers);
        let mut reader = InstructionReader::new(
            &self.bc.instructions,
            self.byte_func.offset,
            self.byte_func.size,
        );
        let res = realm.execute(&mut reader, self.bc);

        realm.stack.exit_call();
        res
    }

    pub fn as_object(&self) -> &Object {
        &self.object
    }
}
