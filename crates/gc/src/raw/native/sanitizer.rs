#[cfg(not(unstable_sanitize))]
pub use self::stub::*;
#[cfg(not(unstable_sanitize))]
mod stub {
    pub unsafe fn poison_region(addr: *mut (), size: usize) {
        let _ = (addr, size);
    }

    pub unsafe fn unpoison_region(addr: *mut (), size: usize) {
        let _ = (addr, size);
    }

    pub unsafe fn describe_address(addr: *mut ()) {
        let _ = (addr,);
    }

    pub unsafe fn address_is_poisoned(addr: *mut ()) -> bool {
        false
    }
}

#[cfg(unstable_sanitize)]
pub use non_stub::*;
#[cfg(unstable_sanitize)]
mod non_stub {
    use std::mem;

    use libc::{c_int, c_void, size_t};

    extern "C" {
        fn __asan_poison_memory_region(addr: *const c_void, size: size_t) -> c_void;
        fn __asan_unpoison_memory_region(addr: *const c_void, size: size_t) -> c_void;
        fn __asan_describe_address(addr: *const c_void) -> c_void;
        fn __asan_address_is_poisoned(addr: *const c_void) -> c_int;
    }

    pub unsafe fn poison_region(addr: *mut (), size: usize) {
        const {
            assert!(mem::size_of::<size_t>() == mem::size_of::<usize>());
            assert!(size_t::MAX == usize::MAX);
        }

        __asan_poison_memory_region(addr.cast::<c_void>() as *const c_void, size as size_t);
    }

    pub unsafe fn unpoison_region(addr: *mut (), size: usize) {
        const {
            assert!(mem::size_of::<size_t>() == mem::size_of::<usize>());
            assert!(size_t::MAX == usize::MAX);
        }

        __asan_unpoison_memory_region(addr.cast::<c_void>() as *const c_void, size as size_t);
    }

    pub unsafe fn describe_address(addr: *mut ()) {
        __asan_describe_address(addr.cast::<c_void>() as *const c_void);
    }

    pub unsafe fn address_is_poisoned(addr: *mut ()) -> bool {
        dbg!(__asan_address_is_poisoned(
            addr.cast::<c_void>() as *const c_void
        )) != 0
    }
}
