use super::Instruction;
use std::{marker::PhantomData, mem};

/// A set of instruction encoded in a dense byte format.
pub struct InstructionBuffer(Box<[u32]>);

impl InstructionBuffer {
    pub fn from_instructions(instr: &[Instruction]) -> Self {
        let mut buffer = Vec::new();
        for i in instr.iter() {
            i.write_to_buffer(&mut buffer);
        }
        Self(buffer.into_boxed_slice())
    }
}

/// A struct which can be used for reading instruction data.
/// One must use caution when using the reader as it has many unsafe functions
/// and can easily trigger undefined behaviour if not used well.
#[derive(Clone, Copy)]
pub struct InstructionReader<'a> {
    cur: *const u8,
    first: *const u8,
    last: *const u8,
    marker: PhantomData<&'a InstructionBuffer>,
}

impl<'a> InstructionReader<'a> {
    pub fn new(buffer: &'a InstructionBuffer) -> Self {
        InstructionReader {
            cur: buffer.0.as_ptr() as *const u8,
            first: buffer.0.as_ptr() as *const u8,
            last: unsafe { buffer.0.as_ptr().add(buffer.0.len()) as *const u8 },
            marker: PhantomData,
        }
    }

    pub fn at_end(&self) -> bool {
        !(self.cur < self.last)
    }

    /// Read a [`u8`] value from the instructions.
    ///
    /// # Panic
    /// Will panic if one tries to read past the end of the instruction buffer.
    pub fn safe_read_u8(&mut self) -> u8 {
        assert!(self.cur < self.last);
        unsafe {
            let res = *self.cur;
            self.cur = self.cur.add(mem::size_of::<u8>());
            res
        }
    }

    /// Read a [`u16`] value from the instructions.
    ///
    /// # Panic
    /// Will panic if one tries to read past the end of the instruction buffer or if
    /// the reader is not currently aligned with a u16 value
    pub fn safe_read_u16(&mut self) -> u16 {
        unsafe {
            assert!(self.cur < self.last.sub(1));
            assert!(self.cur.align_offset(mem::align_of::<u16>()) == 0);
            let res = *(self.cur as *const u16);
            self.cur = self.cur.add(mem::size_of::<u16>());
            res
        }
    }

    /// Read a [`i16`] value from the instructions.
    ///
    /// # Panic
    /// Will panic if one tries to read past the end of the instruction buffer or if
    /// the reader is not currently aligned with a i16 value
    pub fn safe_read_i16(&mut self) -> i16 {
        unsafe {
            assert!(self.cur < self.last.sub(1));
            assert!(self.cur.align_offset(mem::align_of::<i16>()) == 0);
            let res = *(self.cur as *const i16);
            self.cur = self.cur.add(mem::size_of::<i16>());
            res
        }
    }

    /// Read a [`u32`] value from the instructions.
    ///
    /// # Panic
    /// Will panic if one tries to read past the end of the instruction buffer or if
    /// the reader is not currently aligned with a u32 value
    pub fn safe_read_u32(&mut self) -> u32 {
        unsafe {
            assert!(self.cur < self.last.sub(3));
            assert!(self.cur.align_offset(mem::align_of::<u32>()) == 0);
            let res = *(self.cur as *const u32);
            self.cur = self.cur.add(mem::size_of::<u32>());
            res
        }
    }

    /// Read a [`i32`] value from the instructions.
    ///
    /// # Panic
    /// Will panic if one tries to read past the end of the instruction buffer or if
    /// the reader is not currently aligned with a i32 value
    pub fn safe_read_i32(&mut self) -> i32 {
        unsafe {
            assert!(self.cur < self.last.sub(3));
            assert!(self.cur.align_offset(mem::align_of::<i32>()) == 0);
            let res = *(self.cur as *const i32);
            self.cur = self.cur.add(mem::size_of::<i32>());
            res
        }
    }

    /// Jump in the instructions. Offset is in 4 bytes.
    /// # Panic
    /// Will panic if one tries to jump outside the buffer of if the reader is currently not u32
    /// alligned
    pub fn safe_jump(&mut self, offset: i32) {
        let offset = (offset - 1) * mem::size_of::<u32>() as i32;
        unsafe {
            assert!(self.cur.align_offset(mem::align_of::<i32>()) == 0);
            assert!((self.first..self.last).contains(&self.cur.offset(offset as isize)));
            self.cur = self.cur.offset(offset as isize);
        }
    }

    /// Jump in the instructions. Offset is in 4 bytes.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn jump(&mut self, offset: i32) {
        debug_assert!(self.cur.align_offset(mem::align_of::<i32>()) == 0);
        debug_assert!((self.first..self.last).contains(&self.cur.offset(offset as isize)));
        let offset = (offset - 1) * mem::size_of::<u32>() as i32;
        self.cur = self.cur.offset(offset as isize);
    }

    /// Read a [`u8`] value from the instructions.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn read_u8(&mut self) -> u8 {
        debug_assert!(self.cur < self.last);
        let res = *self.cur;
        self.cur = self.cur.add(mem::size_of::<u8>());
        res
    }

    /// Read a [`u16`] value from the instructions.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn read_u16(&mut self) -> u16 {
        debug_assert!(self.cur < self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<u16>()) == 0);
        let res = *(self.cur as *const u16);
        self.cur = self.cur.add(mem::size_of::<u16>());
        res
    }

    /// Read a [`i16`] value from the instructions.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn read_i16(&mut self) -> i16 {
        debug_assert!(self.cur < self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<i16>()) == 0);
        let res = *(self.cur as *const i16);
        self.cur = self.cur.add(mem::size_of::<i16>());
        res
    }

    /// Read a [`u32`] value from the instructions.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn read_u32(&mut self) -> u32 {
        debug_assert!(self.cur < self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<u32>()) == 0);
        let res = *(self.cur as *const u32);
        self.cur = self.cur.add(mem::size_of::<u32>());
        res
    }

    /// Read a [`i32`] value from the instructions.
    /// Will only panic when debug assertions are enabled.
    pub unsafe fn read_i32(&mut self) -> i32 {
        debug_assert!(self.cur < self.last);
        debug_assert!(self.cur.align_offset(mem::align_of::<i32>()) == 0);
        let res = *(self.cur as *const i32);
        self.cur = self.cur.add(mem::size_of::<i32>());
        res
    }
}
