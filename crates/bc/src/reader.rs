use std::{marker::PhantomData, ptr::NonNull};

use bytemuck::Pod;

use crate::{Instruction, Offset};

/// An instruction reader which has a safe interface.
#[derive(Clone, Copy)]
pub struct SafeByteCodeReader<'a> {
    ip: *const u8,
    validate: BcValid<'a>,
    bc: PhantomData<&'a [u8]>,
}

impl<'a> SafeByteCodeReader<'a> {
    pub fn from_bc(bc: &'a [u8]) -> Self {
        SafeByteCodeReader {
            ip: bc.as_ptr(),
            validate: BcValid::from_bc(bc),
            bc: PhantomData,
        }
    }

    pub fn offset(&self) -> usize {
        unsafe { self.ip.offset_from(self.validate.first) as usize }
    }

    pub fn read<D: Pod>(&mut self) -> Option<D> {
        unsafe {
            if self
                .validate
                .is_valid(self.ip.add(std::mem::size_of::<D>() - 1))
            {
                let res = self.ip.cast::<D>().read_unaligned();
                self.ip = self.ip.add(std::mem::size_of::<D>());
                Some(res)
            } else {
                None
            }
        }
    }

    pub fn read_u8(&mut self) -> Option<u8> {
        unsafe {
            if self.validate.is_valid(self.ip) {
                let res = self.ip.read();
                self.ip = self.ip.add(1);
                Some(res)
            } else {
                None
            }
        }
    }

    pub fn read_instruction(&mut self) -> Option<Instruction> {
        Instruction::read(self)
    }
}

#[derive(Clone, Copy)]
pub struct BcValid<'a> {
    first: *const u8,
    offset: usize,
    bc: PhantomData<&'a [u8]>,
}

impl<'a> BcValid<'a> {
    pub fn from_bc(bc: &'a [u8]) -> Self {
        Self {
            first: bc.as_ptr(),
            offset: bc.len(),
            bc: PhantomData,
        }
    }

    /// Check if a pointer is within the instruction buffer.
    ///
    /// # Safety
    ///
    /// Pointer must originate from the same buffer as instruction buffer the reader reads.
    #[inline(always)]
    pub unsafe fn is_valid(&self, ptr: *const u8) -> bool {
        let offset = unsafe { ptr.offset_from(self.first) };
        offset >= 0 && offset as usize <= self.offset
    }

    /// # Safety
    ///
    /// TODO
    pub unsafe fn detach(self) -> BcValid<'static> {
        BcValid {
            first: self.first,
            offset: self.offset,
            bc: PhantomData,
        }
    }
}

/// An instruction reader for the fast, unsafe reading of instruction buffers.
#[derive(Clone, Copy)]
pub struct ByteCodeReader<'a> {
    ip: NonNull<u8>,
    #[cfg(feature = "slow_checks")]
    validate: BcValid<'a>,
    bc: PhantomData<&'a [u8]>,
}

impl<'a> ByteCodeReader<'a> {
    pub fn from_bc(bc: &'a [u8]) -> Self {
        ByteCodeReader {
            ip: NonNull::from(&bc[0]),
            #[cfg(feature = "slow_checks")]
            validate: BcValid::from_bc(bc),
            bc: PhantomData,
        }
    }

    /// # Safety
    ///
    /// TODO
    pub unsafe fn detach(self) -> ByteCodeReader<'static> {
        ByteCodeReader {
            ip: self.ip,
            #[cfg(feature = "slow_checks")]
            validate: self.validate.detach(),
            bc: PhantomData,
        }
    }

    /// Jump the instruction pointer,
    ///
    /// # Safety
    ///
    /// User must ensure that the jump remains within the instruction buffer.
    pub unsafe fn jump(&mut self, offset: Offset) {
        let ip = self.ip.as_ptr().offset(offset.0 as isize);
        self.check_valid(ip);
        self.ip = NonNull::new_unchecked(ip);
    }

    /// Read data from the instruction buffer.
    ///
    /// # Safety
    ///
    /// User must ensure that no data is read past the end of the instruction buffer.
    pub unsafe fn read<D: Pod>(&mut self) -> D {
        let res = self.ip.cast::<D>().as_ptr().read_unaligned();
        let ip = self.ip.as_ptr().add(std::mem::size_of::<D>());
        self.check_valid(ip);
        self.ip = NonNull::new_unchecked(ip);
        res
    }

    /// Check if a pointer is within the instruction buffer, only if the `slow_checks` feature is
    /// enabled.
    ///
    /// # Safety
    ///
    /// Pointer must originate from the same buffer as instruction buffer the reader reads.
    pub unsafe fn check_valid(&self, _ptr: *const u8) {
        #[cfg(feature = "slow_checks")]
        assert!(self.validate.is_valid(_ptr), "pointer outside valid range");
    }
}
