use bitflags::bitflags;
use libc::{c_int, c_void, off_t, size_t, sysconf, _SC_PAGESIZE};

bitflags! {
    pub struct ProtFlags: c_int {
        const EXEC = libc::PROT_EXEC;
        const READ = libc::PROT_READ;
        const WRITE = libc::PROT_WRITE;
        const NONE = libc::PROT_NONE;
    }
}

bitflags! {
    pub struct MapFlags: c_int {
        const ANONYMOUS = libc::MAP_ANONYMOUS;
        const PRIVATE = libc::MAP_PRIVATE;
        const HUGE = libc::MAP_HUGETLB;
        const HUGE2MB = libc::MAP_HUGE_2MB;
        const HUGE1GB = libc::MAP_HUGE_1GB;
        const STACK = libc::MAP_STACK;
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
#[repr(i32)]
pub enum Error {
    Acces = libc::EACCES,
    Again = libc::EAGAIN,
    BadFIle = libc::EBADF,
    Exist = libc::EEXIST,
    Invalid = libc::EINVAL,
    Overflow = libc::EOVERFLOW,
    NoDevice = libc::ENODEV,
    NoMemory = libc::ENOMEM,
    Permission = libc::EPERM,
    TxtBsy = libc::ETXTBSY,
    Other,
}

pub unsafe fn mmap(
    addr: *mut (),
    len: usize,
    prot: ProtFlags,
    map: MapFlags,
    fd: c_int,
    offset: off_t,
) -> Result<*mut (), Error> {
    let res = libc::mmap(addr.cast(), len, prot.bits(), map.bits(), fd, offset);
    if (-1isize as usize as *mut c_void) == res {
        let err = match libc::__errno_location().read() {
            libc::EACCES => Error::Acces,
            libc::EAGAIN => Error::Again,
            libc::EBADF => Error::BadFIle,
            libc::EEXIST => Error::Exist,
            libc::EINVAL => Error::Invalid,
            libc::EOVERFLOW => Error::Overflow,
            libc::ENODEV => Error::NoDevice,
            libc::ENOMEM => Error::NoMemory,
            libc::EPERM => Error::Permission,
            libc::ETXTBSY => Error::TxtBsy,
            _ => Error::Other,
        };
        return Err(err);
    }
    Ok(res.cast())
}

bitflags! {
    pub struct AdviseFlags: c_int {
        const NORMAL = libc::MADV_NORMAL;
        const WILL_NEED = libc::MADV_WILLNEED;
        const WILL_DONTNEED = libc::MADV_DONTNEED;
        const FREE = libc::MADV_FREE;
    }
}

pub unsafe fn madvise(addr: *mut (), len: usize, flags: AdviseFlags) -> Result<(), Error> {
    if libc::madvise(addr.cast(), len as size_t, flags.bits()) < 0 {
        let err = match libc::__errno_location().read() {
            libc::EACCES => Error::Acces,
            libc::EAGAIN => Error::Again,
            libc::EBADF => Error::BadFIle,
            libc::EEXIST => Error::Exist,
            libc::EINVAL => Error::Invalid,
            libc::EOVERFLOW => Error::Overflow,
            libc::ENODEV => Error::NoDevice,
            libc::ENOMEM => Error::NoMemory,
            libc::EPERM => Error::Permission,
            libc::ETXTBSY => Error::TxtBsy,
            _ => Error::Other,
        };
        return Err(err);
    }
    Ok(())
}

pub fn pagesize() -> usize {
    let r = unsafe { sysconf(_SC_PAGESIZE) };
    if r < -1 {
        0
    } else {
        r as usize
    }
}
