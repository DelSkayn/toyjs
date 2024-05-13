use std::ptr;

use self::os::ProtFlags;
use crate::native::os::{AdviseFlags, MapFlags};

pub mod os;
pub mod sanitizer;

#[derive(Debug)]
pub enum AlignedAlloc {
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
    ((ptr as usize) & (alignment - 1)) != 0
}

pub fn ptr_is_aligned_to<T>(ptr: *mut T, alignment: usize) -> bool {
    ptr_is_aligned_to_inner(ptr as *mut (), alignment)
}

pub unsafe fn mmap_aligned(alignment: usize, prot: ProtFlags) -> Result<AlignedAlloc, os::Error> {
    assert!(
        alignment.is_power_of_two(),
        "alignment must be a power of two"
    );

    let ptr = os::mmap(
        ptr::null_mut(),
        alignment,
        prot,
        MapFlags::ANONYMOUS | MapFlags::PRIVATE,
        -1,
        0,
    )?
    .cast::<u8>();

    if ptr_is_aligned_to(ptr, alignment) {
        sanitizer::unpoison_region(ptr.cast(), alignment * 2);
        return Ok(AlignedAlloc::Double(ptr));
    }

    let postfix_len = (ptr as usize) & (alignment - 1);
    let prefix_len = alignment - postfix_len;

    os::madvise(ptr.cast(), prefix_len, AdviseFlags::FREE)?;

    let ptr = ptr.add(prefix_len);

    os::madvise(ptr.add(alignment).cast(), postfix_len, AdviseFlags::FREE)?;
    sanitizer::unpoison_region(ptr.cast(), alignment);

    Ok(AlignedAlloc::Single(ptr))
}

pub unsafe fn mfree(addr: *mut (), size: usize) -> Result<(), os::Error> {
    os::madvise(addr, size, AdviseFlags::FREE)?;
    sanitizer::poison_region(addr.cast(), size);
    Ok(())
}
