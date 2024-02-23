use bc::ByteCodeReader;
use dreck::{Owner, Trace};

mod dispatch;

use crate::{
    object::{BcFunction, GcObject},
    stack::Stack,
    value::Value,
    GcContext,
};

pub struct ExecutionFrame<'gc, 'own> {
    context: GcContext<'gc, 'own>,
    stack: Stack<'gc, 'own>,
}

unsafe impl<'gc, 'own> Trace<'own> for ExecutionFrame<'gc, 'own> {
    type Gc<'r> = ExecutionFrame<'r, 'own>;

    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, marker: dreck::Marker<'own, '_>) {
        marker.mark(self.context);
        self.stack.trace(marker);
    }
}

impl<'gc, 'own> ExecutionFrame<'gc, 'own> {
    pub fn new(context: GcContext<'gc, 'own>) -> Self {
        ExecutionFrame {
            context,
            stack: Stack::new(),
        }
    }

    pub fn enter(
        &mut self,
        function: GcObject<'gc, 'own>,
        owner: &mut Owner<'own>,
    ) -> Result<Value<'gc, 'own>, Value<'gc, 'own>> {
        let global = self.context.borrow(owner).global;

        let Some(BcFunction { id, bc }) = function.borrow(owner).bc_function() else {
            panic!("called enter with a non function object");
        };

        let bc = bc.borrow(owner);

        let size = bc.functions[id.0 as usize].registers;
        let offset = bc.functions[id.0 as usize].offset;

        self.stack.push(global.into()).unwrap();
        self.stack.push(function.into()).unwrap();

        // TODO
        unsafe {
            self.stack.push_entry_frame(size).unwrap();
        }

        //self.stack.pop();
        //self.stack.pop();

        let reader = ByteCodeReader::from_bc(&bc.instructions[offset as usize..]);
        let reader = unsafe { reader.detach() };

        let res = unsafe { self.dispatch(reader, owner) };

        unsafe {
            self.stack.pop_entry_frame();
        }

        res
    }
}
