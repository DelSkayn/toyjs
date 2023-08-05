use std::marker::PhantomData;

use bytemuck::Pod;

use crate::Offset;

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

    pub fn read<D: Pod>(&mut self) -> Option<D> {
        unsafe {
            if self
                .validate
                .is_valid(self.ip.add(std::mem::size_of::<D>()))
            {
                let res = self.ip.cast::<D>().read_unaligned();
                let ip = self.ip.add(1);
                self.ip = ip;
                Some(res)
            } else {
                None
            }
        }
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
}

/// An instruction reader for the fast, unsafe reading of instruction buffers.
#[derive(Clone, Copy)]
pub struct ByteCodeReader<'a> {
    ip: *const u8,
    #[cfg(feature = "slow_checks")]
    validate: BcValid<'a>,
    bc: PhantomData<&'a [u8]>,
}

impl<'a> ByteCodeReader<'a> {
    pub fn from_bc(bc: &'a [u8]) -> Self {
        ByteCodeReader {
            ip: bc.as_ptr(),
            #[cfg(feature = "slow_checks")]
            validate: BcValid::from_bc(bc),
            bc: PhantomData,
        }
    }

    /// Jump the instruction pointer,
    ///
    /// # Safety
    ///
    /// User must ensure that the jump remains within the instruction buffer.
    pub unsafe fn jump(&mut self, offset: Offset) {
        let ip = self.ip.offset(offset.0 as isize);
        self.check_valid(ip);
        self.ip = ip;
    }

    /// Read data from the instruction buffer.
    ///
    /// # Safety
    ///
    /// User must ensure that no data is read past the end of the instruction buffer.
    pub unsafe fn read<D: Pod>(&mut self) -> D {
        let res = self.ip.cast::<D>().read_unaligned();
        let ip = self.ip.add(1);
        self.check_valid(ip);
        self.ip = ip;
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
