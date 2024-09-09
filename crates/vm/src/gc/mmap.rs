use std::ptr;

use rustix::{
    io::Result,
    mm::{self, Advice, MapFlags, ProtFlags},
};

use crate::gc::asan;

#[derive(Debug)]
pub enum AlignedMmap {
    /// Allocation alloced only a single segment.
    Single(*mut u8),
    Double(*mut u8),
}

fn ptr_is_aligned_to_inner(ptr: *mut (), alignment: usize) -> bool {
    assert!(alignment > 0, "zero is not a valid alignment");
    debug_assert!(
        alignment.is_power_of_two(),
        "alignment must be a power of two"
    );
    ((ptr as usize) & (alignment - 1)) == 0
}

pub fn ptr_is_aligned_to<T>(ptr: *mut T, alignment: usize) -> bool {
    ptr_is_aligned_to_inner(ptr as *mut (), alignment)
}

pub unsafe fn mmap_aligned(alignment: usize, prot: ProtFlags) -> Result<AlignedMmap> {
    assert!(
        alignment.is_power_of_two(),
        "alignment must be a power of two"
    );

    // Map two times the alignment.
    // This way if we get back a pointer which is not aligned we can unmap the unaligned region.
    let ptr =
        mm::mmap_anonymous(ptr::null_mut(), alignment * 2, prot, MapFlags::PRIVATE)?.cast::<u8>();

    if ptr_is_aligned_to(ptr, alignment) {
        asan::unpoison_region(ptr.cast(), alignment * 2);
        return Ok(AlignedMmap::Double(ptr));
    }

    let postfix_len = (ptr as usize) & (alignment - 1);
    let prefix_len = alignment - postfix_len;

    mm::munmap(ptr.cast(), prefix_len)?;

    let ptr = ptr.add(prefix_len);

    mm::munmap(ptr.add(alignment).cast(), postfix_len)?;
    asan::unpoison_region(ptr.cast(), alignment);

    Ok(AlignedMmap::Single(ptr))
}

pub unsafe fn mfree(addr: *mut (), size: usize) -> Result<()> {
    mm::madvise(addr.cast(), size, Advice::LinuxFree)?;
    asan::poison_region(addr.cast(), size);
    Ok(())
}

pub unsafe fn munmap(addr: *mut (), size: usize) -> Result<()> {
    mm::munmap(addr.cast(), size)?;
    asan::poison_region(addr.cast(), size);
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn aligned_rw() {
        const PAGE_SIZE: usize = 4096;

        // write and read a value to the start and end of a memory region.
        unsafe fn test_rw(ptr: *mut u8, size: usize) {
            const VALUE: u64 = 0xDEAD_BEEF_CAFE_BABA;

            let ptr = ptr.cast::<u64>();

            // write_volatile so that they are never optimized out.
            ptr.write_volatile(VALUE);
            assert_eq!(ptr.read_volatile(), VALUE);

            let ptr = ptr
                .cast::<u8>()
                .add(size - std::mem::size_of::<u64>())
                .cast::<u64>();

            // write_volatile so that they are never optimized out.
            ptr.write_volatile(VALUE);
            assert_eq!(ptr.read_volatile(), VALUE);
        }

        let mut ptrs = Vec::new();

        for _ in 0..4 {
            match unsafe { mmap_aligned(PAGE_SIZE, ProtFlags::READ | ProtFlags::WRITE).unwrap() } {
                AlignedMmap::Single(x) => {
                    assert_eq!(x.align_offset(PAGE_SIZE), 0);
                    unsafe { test_rw(x, PAGE_SIZE) }
                    ptrs.push(x);
                }
                AlignedMmap::Double(x) => {
                    assert_eq!(x.align_offset(PAGE_SIZE), 0);
                    unsafe { test_rw(x, PAGE_SIZE) }
                    unsafe { test_rw(x.add(PAGE_SIZE), PAGE_SIZE) }
                    ptrs.push(x);
                    ptrs.push(unsafe { x.add(PAGE_SIZE) });
                }
            }
        }

        for p in ptrs.drain(..) {
            unsafe { mfree(p.cast(), PAGE_SIZE).unwrap() }
        }

        for _ in 0..4 {
            match unsafe {
                mmap_aligned(PAGE_SIZE * 8, ProtFlags::READ | ProtFlags::WRITE).unwrap()
            } {
                AlignedMmap::Single(x) => {
                    assert_eq!(x.align_offset(PAGE_SIZE * 8), 0);
                    unsafe { test_rw(x, PAGE_SIZE * 8) }
                    ptrs.push(x);
                }
                AlignedMmap::Double(x) => {
                    assert_eq!(x.align_offset(PAGE_SIZE * 8), 0);
                    unsafe { test_rw(x, PAGE_SIZE * 8) }
                    unsafe { test_rw(x.add(PAGE_SIZE * 8), PAGE_SIZE * 8) }
                    ptrs.push(x);
                    ptrs.push(unsafe { x.add(PAGE_SIZE * 8) });
                }
            }
        }

        for p in ptrs.drain(..) {
            unsafe { mfree(p.cast(), PAGE_SIZE * 8).unwrap() }
        }
    }
}
