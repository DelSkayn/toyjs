use crate::{
    cell::CellOwner,
    gc::Arena,
    instructions::{GcByteCode, Instruction},
    object::ObjectKind,
    rebind_value, Object, Realm, Value,
};

use super::ExecutionContext;

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
    ///
    pub unsafe fn new_unsafe(
        owner: &CellOwner<'cell>,
        bc: GcByteCode<'gc, 'cell>,
        func: u16,
    ) -> Self {
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
    pub unsafe fn next(&mut self, _owner: &CellOwner<'cell>) -> Instruction {
        #[cfg(debug_assertions)]
        assert!(self.cur < self.last);
        let res = self.cur.read();
        self.cur = self.cur.add(1);
        res
    }

    /// # Safety
    ///
    /// `idx` must be smaller or equal to the amount of constants in the containted bytecode.
    pub unsafe fn constant(&self, idx: u16, owner: &CellOwner<'cell>) -> Value<'gc, 'cell> {
        debug_assert!(self.bc.borrow(owner).constants.len() > idx as usize);
        *self.bc.borrow(owner).constants.get_unchecked(idx as usize)
    }
}

impl<'gc, 'cell: 'gc> Realm<'gc, 'cell> {
    pub unsafe fn run<'l>(
        &self,
        arena: &'l mut Arena<'_, 'cell>,
        owner: &mut CellOwner<'cell>,
        mut instr: InstructionReader<'_, 'cell>,
        ctx: ExecutionContext<'_, 'cell>,
    ) -> Result<Value<'l, 'cell>, Value<'l, 'cell>> {
        loop {
            match instr.next(owner) {
                Instruction::LoadConst { dst, cons } => {
                    let con = instr.constant(cons, owner);
                    self.stack.write(dst, con);
                }
                Instruction::LoadGlobal { dst } => {
                    self.stack.write(dst, self.global.into());
                }
                Instruction::LoadThis { dst } => {
                    self.stack.write(dst, ctx.this);
                }
                Instruction::LoadTarget { dst } => {
                    self.stack.write(dst, ctx.new_target);
                }
                Instruction::Move { dst, src } => {
                    self.stack.write(dst, self.stack.read(src));
                }
                Instruction::CreateObject { dst } => {
                    let object = arena.add(Object::new(None, ObjectKind::Ordinary));
                    self.stack.write(dst, object.into());
                }
                Instruction::CreateArray { dst } => {
                    let object = arena.add(Object::new(None, ObjectKind::Ordinary));
                    self.stack.write(dst, object.into());
                }
                Instruction::Return { ret } => {
                    let res = self.stack.read(ret);
                    return Ok(rebind_value!(arena, res));
                }
                Instruction::ReturnUndefined { _ignore } => {
                    return Ok(Value::undefined());
                }
                x => todo!("Instruction {}", x),
            }
        }
    }
}
