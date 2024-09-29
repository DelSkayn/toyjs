pub enum SegmentAlloc {
    Single(Raw<u8>),
    Double(Raw<u8>),
}

#[cfg(feature = "mmap")]
mod mmap {
    use std::ptr;

    use common::ptr::Raw;
    use rustix::mm::{self, MapFlags, ProtFlags};

    use super::SegmentAlloc;
    use crate::util::asan;

    fn ptr_is_aligned_to_inner(ptr: *mut (), alignment: usize) -> bool {
        assert!(alignment > 0, "zero is not a valid alignment");
        debug_assert!(
            alignment.is_power_of_two(),
            "alignment must be a power of two"
        );
        ((ptr as usize) & (alignment - 1)) == 0
    }

    pub fn ptr_is_aligned_to(ptr: *mut u8, alignment: usize) -> bool {
        ptr_is_aligned_to_inner(ptr as *mut (), alignment)
    }

    pub unsafe fn alloc_segment(alignment: usize) -> Option<SegmentAlloc> {
        assert!(
            alignment.is_power_of_two(),
            "alignment must be a power of two"
        );

        // Map two times the alignment.
        // This way if we get back a pointer which is not aligned we can unmap the unaligned region.
        let ptr = mm::mmap_anonymous(
            ptr::null_mut(),
            alignment * 2,
            ProtFlags::WRITE | ProtFlags::READ,
            MapFlags::PRIVATE,
        )
        .ok()?
        .cast::<u8>();

        let ptr = Raw::from_ptr(ptr)?;

        //TODO: Handle errors here better.
        if ptr_is_aligned_to(ptr.into_ptr(), alignment) {
            asan::unpoison_region(ptr.into_ptr().cast(), alignment * 2);
            return Some(SegmentAlloc::Double(ptr));
        }

        let postfix_len = ptr.addr().get() & (alignment - 1);
        let prefix_len = alignment - postfix_len;

        // This should only fail if we hand it invalid arguments which is implementation bug so
        // unwrap.
        mm::munmap(ptr.cast().as_ptr(), prefix_len).unwrap();

        let ptr = ptr.add(prefix_len);

        // This should only fail if we hand it invalid arguments which is implementation bug so
        // unwrap.
        mm::munmap(ptr.add(alignment).cast().as_ptr(), postfix_len).unwrap();
        asan::unpoison_region(ptr.cast().as_ptr(), alignment);

        Some(SegmentAlloc::Single(ptr))
    }

    pub unsafe fn free_segment(segment_ptr: Raw<u8>, alignment: usize) {
        // munmap should only return an error if we hand it invalid arguments.
        // Doing so should be a panic.
        mm::munmap(segment_ptr.cast().as_ptr(), alignment).unwrap()
    }
}
use common::ptr::Raw;
#[cfg(feature = "mmap")]
pub use mmap::*;

#[cfg(not(feature = "mmap"))]
mod alloc {
    use std::alloc::Layout;

    use common::ptr::Raw;

    use super::SegmentAlloc;

    pub unsafe fn alloc_segment(alignment: usize) -> Option<SegmentAlloc> {
        let layout = Layout::from_size_align(alignment, alignment).unwrap();

        let alloc = Raw::from_ptr(std::alloc::alloc(layout))?;

        Some(SegmentAlloc::Single(alloc))
    }

    pub unsafe fn free_segment(segment_ptr: Raw<u8>, alignment: usize) {
        let layout = Layout::from_size_align(alignment, alignment).unwrap();
        std::alloc::dealloc(segment_ptr.as_ptr(), layout);
    }
}
#[cfg(not(feature = "mmap"))]
pub use alloc::*;
