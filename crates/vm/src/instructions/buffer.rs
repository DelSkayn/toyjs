use super::Instruction;
use std::{fmt, marker::PhantomData, mem};

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

    /// The amount of 4 byte values in the instruction buffer.
    pub fn size(&self) -> usize {
        self.0.len()
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

pub struct InvalidJump;

impl fmt::Display for InvalidJump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Tried to jump to an invalid location into the instruction buffer"
        )
    }
}

impl<'a> InstructionReader<'a> {
    pub fn new(buffer: &'a InstructionBuffer, offset: usize, size: usize) -> Self {
        unsafe {
            InstructionReader {
                cur: buffer.0.as_ptr().add(offset) as *const u8,
                first: buffer.0.as_ptr().add(offset) as *const u8,
                last: buffer.0.as_ptr().add(offset + size) as *const u8,
                marker: PhantomData,
            }
        }
    }

    pub fn at_end(&self) -> bool {
        !(self.cur < self.last)
    }

    /// Read a [`u8`] value from the instructions if the instructions are u8 alligned and the
    /// reader has enough values left..
    pub fn try_read_u8(&mut self) -> Option<u8> {
        unsafe {
            if self.at_end() {
                return None;
            }
            Some(self.read_u8())
        }
    }

    /// Read a [`u16`] value from the instructions if the instructions are u16 alligned and the
    /// reader has enough values left..
    pub fn try_read_u16(&mut self) -> Option<u16> {
        unsafe {
            if self.cur <= self.last.sub(mem::size_of::<u16>())
                && self.cur.align_offset(mem::align_of::<u16>()) == 0
            {
                Some(self.read_u16())
            } else {
                None
            }
        }
    }

    /// Read a [`i16`] value from the instructions if the instructions are i16 alligned and the
    /// reader has enough values left..
    pub fn try_read_i16(&mut self) -> Option<i16> {
        unsafe {
            if self.cur <= self.last.sub(mem::size_of::<i16>())
                && self.cur.align_offset(mem::align_of::<i16>()) == 0
            {
                Some(self.read_i16())
            } else {
                None
            }
        }
    }

    /// Read a [`u32`] value from the instructions if the instructions are u32 alligned and the
    /// reader has enough values left..
    pub fn try_read_u32(&mut self) -> Option<u32> {
        unsafe {
            if self.cur <= self.last.sub(mem::size_of::<u32>())
                && self.cur.align_offset(mem::align_of::<u32>()) == 0
            {
                Some(self.read_u32())
            } else {
                None
            }
        }
    }

    /// Read a [`i32`] value from the instructions if the instructions are i32 alligned and the
    /// reader has enough values left..
    pub fn try_read_i32(&mut self) -> Option<i32> {
        unsafe {
            if self.cur <= self.last.sub(mem::size_of::<i32>())
                && self.cur.align_offset(mem::align_of::<i32>()) == 0
            {
                Some(self.read_i32())
            } else {
                None
            }
        }
    }

    /// Jump in the instructions. Offset is in 4 bytes.
    /// Will fail if current instruction reader is not 4 byte alligned or the jump is to an invalid
    /// offset.
    pub fn try_jump(&mut self, offset: i32) -> Result<(), InvalidJump> {
        let offset = (offset - 1) * mem::size_of::<u32>() as i32;
        unsafe {
            if self.cur.align_offset(mem::align_of::<i32>()) == 0
                && (self.first..self.last).contains(&self.cur.offset(offset as isize))
            {
                self.cur = self.cur.offset(offset as isize);
                Ok(())
            } else {
                Err(InvalidJump)
            }
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
