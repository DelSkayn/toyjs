use crate::{
    bytecode::{Bytecode, RuntimeFunction},
    exec::RunningExecution,
    gc::Gc,
    Environment, ExecutionContext, JSValue, Object,
};
use std::{mem, rc::Rc};

#[derive(Debug)]
pub struct Function {
    object: Gc<Object>,
    function: RuntimeFunction,
}

impl Function {
    pub unsafe fn new<'a, R>(id: u32, exec: &mut RunningExecution<'a, R>) -> Self {
        let object = Object::new();
        let object = exec.gc.allocate(object);
        Function {
            object,
            function: RuntimeFunction {
                module: exec.module,
                function: id,
            },
        }
    }
}
