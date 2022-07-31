use dreck::{Bound, Owner, Trace};

use crate::{
    instructions::{GcByteCode, Instruction},
    value::Value,
};

pub struct InstructionReader<'gc, 'own> {
    bc: GcByteCode<'gc, 'own>,
    cur: *const Instruction,
    #[cfg(debug_assertions)]
    first: *const Instruction,
    #[cfg(debug_assertions)]
    last: *const Instruction,
}

unsafe impl<'gc, 'own> Trace<'own> for InstructionReader<'gc, 'own> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace<'a>(&self, tracer: dreck::Tracer<'a, 'own>) {
        tracer.mark(self.bc)
    }
}

unsafe impl<'from, 'to, 'own> Bound<'to> for InstructionReader<'from, 'own> {
    type Rebound = InstructionReader<'to, 'own>;
}

impl<'gc, 'own> InstructionReader<'gc, 'own> {
    /// # Safety
    ///
    /// `func` must be a smaller the amount of function present in the bytecode.
    /// `bc` must contain valid bytecode.
    ///
    pub fn new(owner: &Owner<'own>, bc: GcByteCode<'gc, 'own>, func: u16) -> Self {
        let function = bc
            .borrow(owner)
            .functions
            .get(func as usize)
            .expect("invalid function id");
        unsafe {
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
    }

    pub fn bc(&self) -> GcByteCode<'gc, 'own> {
        self.bc
    }

    /// # Safety
    ///
    /// User must ensure that the reader does not read past the end of the instruction buffer
    pub unsafe fn next(&mut self, _owner: &Owner<'own>) -> Instruction {
        #[cfg(debug_assertions)]
        assert!(self.cur < self.last);
        let res = self.cur.read();
        self.cur = self.cur.add(1);
        res
    }

    /// # Safety
    ///
    /// User must ensure that the reader does not jump past the end or before the beginning of the
    /// instruction buffer.
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
    pub unsafe fn constant(&self, idx: u16, owner: &Owner<'own>) -> Value<'gc, 'own> {
        debug_assert!(self.bc.borrow(owner).constants.len() > idx as usize);
        *self.bc.borrow(owner).constants.get_unchecked(idx as usize)
    }
}
