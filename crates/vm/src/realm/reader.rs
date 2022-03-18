use crate::{
    gc::Trace,
    instructions::{ByteCode, ByteFunction, Instruction},
    Gc, Value,
};

#[derive(Debug, Clone, Copy)]
pub struct InstructionReader {
    pub(crate) bc: Gc<ByteCode>,
    cur: *const Instruction,
    #[cfg(debug_assertions)]
    first: *const Instruction,
    #[cfg(debug_assertions)]
    last: *const Instruction,
}

unsafe impl Trace for InstructionReader {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: crate::gc::Ctx) {
        ctx.mark(self.bc);
    }
}

impl InstructionReader {
    pub fn from_bc(bc: Gc<ByteCode>, function: u16) -> Self {
        let func = &bc.functions[function as usize];
        unsafe {
            let first = bc.instructions.as_ptr().add(func.offset as usize);
            Self {
                bc,
                cur: first,
                #[cfg(debug_assertions)]
                first,
                #[cfg(debug_assertions)]
                last: first.add(func.size.try_into().unwrap()),
            }
        }
    }

    pub unsafe fn next(&mut self) -> Instruction {
        #[cfg(debug_assertions)]
        debug_assert!(self.cur < self.last);
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

    pub unsafe fn absolute_jump(&mut self, offset: usize) {
        #[cfg(debug_assertions)]
        debug_assert!(self.first.add(offset) < self.last);
        self.cur = self.bc.instructions.as_ptr().add(offset);
    }

    pub unsafe fn absolute_offset(&self, relative: i16) -> isize {
        self.cur
            .offset(isize::from(relative) - 1)
            .offset_from(self.bc.instructions.as_ptr())
    }

    pub unsafe fn constant(&self, id: u32) -> Value {
        debug_assert!(
            (id as usize) < self.bc.constants.len(),
            "assert `id as usize < assert self.bc.constants.len()` failed. len: `{}`, id: `{}`",
            self.bc.constants.len(),
            id
        );
        *self.bc.constants.get_unchecked(id as usize)
    }

    pub unsafe fn function(&self, id: u16) -> &ByteFunction {
        &self.bc.functions[id as usize]
    }
}
