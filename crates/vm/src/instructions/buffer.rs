use super::Instruction;
use std::{fmt, marker::PhantomData, mem};

/// A set of instruction encoded in a dense byte format.
#[derive(Debug)]
pub struct InstructionBuffer(Box<[u32]>);

impl InstructionBuffer {
    pub fn from_instructions(instr: &[Instruction]) -> Self {
        let mut buffer = Vec::new();
        for i in instr.iter() {
            i.write_to_buffer(&mut buffer);
        }
        Self(buffer.into_boxed_slice())
    }

    pub fn as_ptr(&self) -> *const u32 {
        self.0.as_ptr()
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
    pub fn new(buffer: &'a InstructionBuffer, offset: u32, end: u32) -> Self {
        unsafe {
            InstructionReader {
                cur: buffer.0.as_ptr().add(offset as usize) as *const u8,
                first: buffer.0.as_ptr().add(offset as usize) as *const u8,
                last: buffer.0.as_ptr().add(end as usize) as *const u8,
                marker: PhantomData,
            }
        }
    }

    pub fn at_end(&self) -> bool {
        !(self.cur < self.last)
    }

    pub fn offset(&mut self) -> u32 {
        debug_assert!(
            self.cur.align_offset(mem::align_of::<u32>()) == 0,
            "tried to get offset of instruction reader which was not 32 bit aligned"
        );
        unsafe { (self.cur.offset_from(self.cur) as usize / mem::size_of::<u32>()) as u32 }
    }

    pub unsafe fn restore_offset(&mut self, offset: u32) {
        self.cur = self.first.add(offset as usize * mem::size_of::<u32>());
        debug_assert!(
            self.cur < self.last,
            "restored offset is larger then instruction buffer"
        );
    }

    /// Read a [`u8`] value from the instructions if the instructions are u8 alligned and the
    /// reader has enough values left..
    pub fn try_read_u8(&mut self) -> Option<u8> {
        unsafe {
            if self.at_end() {
                return None;
            }
            let res = self.cur.read();
            self.cur = self.cur.add(1);
            Some(res)
        }
    }

    /// Read a [`u16`] value from the instructions if the instructions are u16 alligned and the
    /// reader has enough values left..
    pub fn try_read_u16(&mut self) -> Option<u16> {
        unsafe {
            if self.cur <= self.last.sub(mem::size_of::<u16>())
                && self.cur.align_offset(mem::align_of::<u16>()) == 0
            {
                let res = self.cur.cast::<u16>().read();
                self.cur = self.cur.add(mem::size_of::<u16>());
                Some(res)
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
                let res = self.cur.cast::<i16>().read();
                self.cur = self.cur.add(mem::size_of::<i16>());
                Some(res)
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
                let res = self.cur.cast::<u32>().read();
                self.cur = self.cur.add(mem::size_of::<u32>());
                Some(res)
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
                let res = self.cur.cast::<i32>().read();
                self.cur = self.cur.add(mem::size_of::<i32>());
                Some(res)
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
}
