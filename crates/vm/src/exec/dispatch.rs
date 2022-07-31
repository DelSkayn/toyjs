use crate::{
    instructions::Instruction,
    object::{Object, ObjectFlags, ObjectKind},
    realm::InstructionReader,
    value::Value,
};

use super::ExecutionContext;

impl<'l, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    #[inline]
    unsafe fn r(&mut self, reg: u8) -> Value<'gc, 'own> {
        self.stack.borrow(self.owner).read(reg)
    }

    // Shorthand for Gc::unsafe_borrow_mut
    //
    // Should only be used as long as before each collection and return from the loop the a
    // write_barrier is done for the realm
    #[inline]
    unsafe fn w<'a>(&mut self, reg: u8, value: impl Into<Value<'a, 'own>>) {
        self.stack
            .unsafe_borrow_mut(self.owner)
            .write(reg, value.into())
    }

    pub(super) unsafe fn dispatch(
        &mut self,
        mut reader: InstructionReader<'gc, 'own>,
    ) -> Result<Value<'l, 'own>, Value<'l, 'own>> {
        loop {
            match reader.next(self.owner) {
                Instruction::LoadConst { dst, cons } => {
                    let cons = reader.constant(cons, self.owner);
                    self.w(dst, cons);
                }
                Instruction::LoadGlobal { dst } => {
                    let global = self.realm.borrow(self.owner).global;
                    self.w(dst, global);
                }
                Instruction::LoadThis { dst } => self.w(dst, self.this),
                Instruction::LoadTarget { dst } => self.w(dst, self.new_target),
                Instruction::Move { dst, src } => {
                    let v = self.r(src);
                    self.w(dst, v);
                }
                Instruction::CreateObject { dst } => {
                    self.root.write_barrier(self.stack);
                    self.root.collect(self.owner);
                    let obj = Object::new_gc(
                        self.root,
                        None,
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    );
                    self.stack
                        .unsafe_borrow_mut(self.owner)
                        .write(dst, obj.into());
                }
                x => todo!("{x:?}"),
            }
        }
    }
}
