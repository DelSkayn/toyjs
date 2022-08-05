use dreck::rebind;

use crate::{
    instructions::Instruction,
    object::{Object, ObjectFlags, ObjectKind},
    realm::InstructionReader,
    value::Value,
};

use super::ExecutionContext;

// shorthand for writing to a register.
// used when ExecutionContext::w leads to lifetime conflict.
macro_rules! w {
    ($self:expr,$reg:expr,$value:expr) => {
        $self
            .stack
            .unsafe_borrow_mut($self.owner)
            .write($reg, $value.into())
    };
}

// dispatch try. will unwind the stack if an error is thrown.
macro_rules! dtry {
    ($self:expr,$value:expr) => {
        match $value {
            Ok(x) => x,
            Err(_) => todo!(),
        }
    };
}

impl<'l, 'gc, 'own> ExecutionContext<'l, 'gc, 'own> {
    #[inline]
    unsafe fn r(&'l self, reg: u8) -> Value<'gc, 'own> {
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
                    w!(self, dst, obj);
                }

                Instruction::IndexAssign { obj, key, src } => {
                    let obj = self.r(obj);
                    let key = self.r(key);
                    let value = self.r(src);
                    if let Some(obj) = obj.into_object() {
                        dtry!(self, Object::index_set_value(obj, self, key, value))
                    } else {
                        todo!()
                    }
                }
                Instruction::Index { dst, obj, key } => {
                    let obj = self.r(obj);
                    let key = self.r(key);
                    if let Some(obj) = obj.into_object() {
                        let v = dtry!(self, Object::index_value(obj, self, key));
                        let v = rebind!(self.root, v);
                        w!(self, dst, v);
                    } else {
                        todo!()
                    }
                }
                Instruction::GlobalAssign { key, src } => {
                    let obj = self.realm.borrow(self.owner).global;
                    let key = self.r(key);
                    let value = self.r(src);
                    dtry!(self, Object::index_set_value(obj, self, key, value));
                }
                Instruction::GlobalIndex { dst, key } => {
                    let obj = self.realm.borrow(self.owner).global;
                    let key = self.r(key);
                    let v = dtry!(self, Object::index_value(obj, self, key));
                    let v = rebind!(self.root, v);
                    w!(self, dst, v);
                }
                x => todo!("{x:?}"),
            }
        }
    }
}
