use std::mem;

use dreck::{rebind, root};

use crate::{
    object::{GcObject, ObjectKind},
    realm::{
        stack::{FrameType, Stack},
        InstructionReader,
    },
    value::Value,
};

use super::ExecutionContext;

impl<'r, 'l: 'r, 'gc, 'own: 'gc> ExecutionContext<'l, 'gc, 'own> {
    pub fn internal_call(
        &'r mut self,
        reader: &mut InstructionReader<'gc, 'own>,
        dst: u8,
        function: GcObject<'_, 'own>,
    ) -> Result<(), Value<'r, 'own>> {
        let function: GcObject<'gc, 'own> = unsafe { dreck::rebind(function) };
        let old_function = mem::replace(&mut self.function, function);

        match unsafe { self.function.borrow(self.owner).kind() } {
            ObjectKind::VmFn(x) => {
                let size = x.bc.borrow(self.owner).functions[x.function as usize].registers;
                let new_reader = InstructionReader::<'_, 'own>::new(self.owner, x.bc, x.function);
                let old_reader = mem::replace(reader, new_reader);

                Stack::push_frame(
                    self.stack,
                    self.owner,
                    self.root,
                    FrameType::Internal {
                        reader: old_reader,
                        function: old_function,
                        dst,
                        size,
                    },
                );
                Ok(())
            }
            ObjectKind::StaticFn(ref x) => {
                let x = *x;
                Stack::push_frame(
                    self.stack,
                    self.owner,
                    self.root,
                    FrameType::Entry { size: 0 },
                );
                self.root.write_barrier(self.stack);
                let res = rebind!(self.root, x(self));
                Stack::pop_frame(self.stack, self.owner, self.root);
                self.function = old_function;
                unsafe {
                    self.stack.unsafe_borrow_mut(self.owner).write(dst, res?);
                };
                Ok(())
            }
            _ => {
                self.function = old_function;
                Err(self.type_error("tried to call non function object"))
            }
        }
    }
}
