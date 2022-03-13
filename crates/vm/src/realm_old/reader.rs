use std::mem;

use crate::{function::Function, gc::Trace, instructions::ByteCode, Gc, Value};

#[derive(Debug, Clone, Copy)]
pub struct InstructionReader {
    bc: Gc<ByteCode>,
    cur: *const u8,
    #[cfg(debug_assertions)]
    first: *const u8,
    #[cfg(debug_assertions)]
    last: *const u8,
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
    pub fn from_bc(bc: Gc<ByteCode>, function: u32) -> Self {
        let func = bc.functions[function as usize];
        unsafe {
            let first = bc
                .instructions
                .as_ptr()
                .add(func.offset as usize)
                .cast::<u8>();
            Self {
                bc,
                cur: first,
                #[cfg(debug_assertions)]
                first,
                #[cfg(debug_assertions)]
                last: first.add(func.size as usize * mem::size_of::<u32>()),
            }
        }
    }

    pub unsafe fn read_u8(&mut self) -> u8 {
        debug_assert!(self.cur < self.last);
        let res = self.cur.read();
        self.cur = self.cur.add(mem::size_of::<u8>());
        res
    }

    pub unsafe fn read_i8(&mut self) -> i8 {
        debug_assert!(self.cur < self.last);
        let res = self.cur.cast::<i8>().read();
        self.cur = self.cur.add(mem::size_of::<i8>());
        res
    }

    pub unsafe fn read_u16(&mut self) -> u16 {
        debug_assert!(self.cur.add(mem::size_of::<u16>()) <= self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<u16>()) == 0);
        let res = self.cur.cast::<u16>().read();
        self.cur = self.cur.add(mem::size_of::<u16>());
        res
    }

    pub unsafe fn read_i16(&mut self) -> i16 {
        debug_assert!(self.cur.add(mem::size_of::<i16>()) <= self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<i16>()) == 0);
        let res = self.cur.cast::<i16>().read();
        self.cur = self.cur.add(mem::size_of::<i16>());
        res
    }

    pub unsafe fn read_u32(&mut self) -> u32 {
        debug_assert!(self.cur.add(mem::size_of::<u32>()) <= self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<u32>()) == 0);
        let res = self.cur.cast::<u32>().read();
        self.cur = self.cur.add(mem::size_of::<u32>());
        res
    }

    pub unsafe fn read_i32(&mut self) -> i32 {
        debug_assert!(self.cur.add(mem::size_of::<i32>()) <= self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<i32>()) == 0);
        let res = self.cur.cast::<i32>().read();
        self.cur = self.cur.add(mem::size_of::<i32>());
        res
    }

    pub unsafe fn jump(&mut self, offset: i32) {
        debug_assert!(self.cur.align_offset(mem::align_of::<u32>()) == 0);
        let offset = (offset - 1) as isize * mem::size_of::<u32>() as isize;
        debug_assert!(self.cur.offset(offset) < self.last);
        debug_assert!(self.cur.offset(offset) >= self.first);
        self.cur = self.cur.offset(offset);
    }

    pub unsafe fn constant(&self, id: u32) -> Value {
        *self.bc.constants.get_unchecked(id as usize)
    }

    pub unsafe fn function(&self, id: u32) -> Function {
        Function::from_bc(self.bc, id)
    }
}
