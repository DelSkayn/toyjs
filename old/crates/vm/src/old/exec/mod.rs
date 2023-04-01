use common::atom::Atoms;
use dreck::{Gc, Owner, Root};

use crate::{
    instructions::GcByteCode,
    instructions::Instruction,
    object::{ObjectFlags, ObjectKind},
    realm::{GcRealm, Stack},
    GcObject, Object, Value,
};

#[derive(Clone, Copy)]
pub struct InstructionReader<'gc, 'cell> {
    bc: GcByteCode<'gc, 'cell>,
    cur: *const Instruction,
    #[cfg(debug_assertions)]
    first: *const Instruction,
    #[cfg(debug_assertions)]
    last: *const Instruction,
}

impl<'gc, 'cell> InstructionReader<'gc, 'cell> {
    /// # Safety
    ///
    /// `func` must be a smaller the amount of function present in the bytecode.
    /// `bc` must contain valid bytecode.
    ///
    pub unsafe fn new_unsafe(owner: &Owner<'cell>, bc: GcByteCode<'gc, 'cell>, func: u16) -> Self {
        debug_assert!(bc.borrow(owner).functions.len() > func as usize);
        let function = bc.borrow(owner).functions.get_unchecked(func as usize);
        let cur = bc
            .borrow(owner)
            .instructions
            .as_ptr()
            .add(function.offset as usize);

        #[cfg(debug_assertions)]
        let first = cur;
        #[cfg(debug_assertions)]
        let last = cur.add(function.size as usize);
        InstructionReader {
            bc,
            cur,
            #[cfg(debug_assertions)]
            first,
            #[cfg(debug_assertions)]
            last,
        }
    }

    pub fn bc(&self) -> GcByteCode<'gc, 'cell> {
        self.bc
    }

    /// # Safety
    ///
    /// User must ensure that the reader does not read past the end of the instruction buffer
    pub unsafe fn next(&mut self, _owner: &Owner<'cell>) -> Instruction {
        #[cfg(debug_assertions)]
        assert!(self.cur < self.last);
        let res = self.cur.read();
        self.cur = self.cur.add(1);
        res
    }

    pub unsafe fn jump(&mut self, mut offset: i16) {
        offset -= 1;
        #[cfg(debug_assertions)]
        assert!(self.cur.offset(offset.into()) < self.last);
        #[cfg(debug_assertions)]
        assert!(self.cur.offset(offset.into()) >= self.first);
        self.cur = self.cur.offset(offset.into());
    }

    /// # Safety
    ///
    /// `idx` must be smaller or equal to the amount of constants in the containted bytecode.
    pub unsafe fn constant(&self, idx: u16, owner: &Owner<'cell>) -> Value<'gc, 'cell> {
        debug_assert!(self.bc.borrow(owner).constants.len() > idx as usize);
        *self.bc.borrow(owner).constants.get_unchecked(idx as usize)
    }
}

pub struct ExecutionContext<'l, 'gc, 'cell> {
    pub realm: GcRealm<'gc, 'cell>,
    pub stack: Gc<'gc, 'cell, Stack<'gc, 'cell>>,
    pub atoms: &'l Atoms,
    pub arena: &'l mut Root<'cell>,
    pub owner: &'l mut Owner<'cell>,
    pub this: Value<'gc, 'cell>,
    pub new_target: Value<'gc, 'cell>,
    pub function: GcObject<'gc, 'cell>,
}

impl<'l, 'gc, 'cell> ExecutionContext<'l, 'gc, 'cell> {
    pub fn collect(&mut self) {
        self.arena.write_barrier(self.stack);
        self.arena.collect(self.owner);
    }

    #[allow(unused_unsafe)]
    pub unsafe fn run(
        &mut self,
        instructions: &mut InstructionReader<'gc, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let mut instr = *instructions;
        loop {
            let op = instr.next(self.owner);
            match op {
                Instruction::CreateObject { dst } => {
                    self.collect();
                    let obj = Object::new_gc(
                        self.arena,
                        None,
                        ObjectFlags::ORDINARY,
                        ObjectKind::Ordinary,
                    );
                    self.stack
                        .borrow_mut_untraced(self.owner)
                        .write(dst, obj.into());
                }
                Instruction::Move { dst, src } => {
                    let src = self.stack.borrow(self.owner).read(src);
                    self.stack.borrow_mut_untraced(self.owner).write(dst, src);
                }
                Instruction::Call { dst, func } => {
                    let func = self.stack.borrow(self.owner).read(func);
                    if let Some(func) = func.into_object() {
                        let v = self.call(func)?;
                        self.stack.borrow_mut_untraced(self.owner).write(dst, v);
                    }
                }
                Instruction::CallMethod { dst, obj, key } => {
                    let obj = self.stack.borrow(self.owner).read(obj);
                    let key = self.stack.borrow(self.owner).read(key);
                    if let Some(obj) = obj.into_object() {
                        let func = rebind_try!(
                            self.arena,
                            Object::index_value(
                                obj, self.owner, self.arena, self.atoms, self.realm, key,
                            )
                        );
                    }
                }
                _ => {}
            }
        }
    }

    pub fn call(
        &mut self,
        func: GcObject<'gc, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        let func = std::mem::replace(&mut self.function, func);
        let new_target = std::mem::replace(&mut self.new_target, Value::undefined());

        let res = match unsafe { func.borrow(self.owner).kind() } {
            ObjectKind::VmFn(vm_fn) => {
                let bc = vm_fn.bc;
                let mut instr =
                    unsafe { InstructionReader::new_unsafe(self.owner, bc, vm_fn.function) };

                unsafe { self.run(&mut instr) }
            }
            _ => todo!(),
        };

        self.function = func;
        self.new_target = new_target;

        res
    }
}
