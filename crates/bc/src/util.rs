use std::mem::MaybeUninit;

use bytemuck::Pod;

pub fn from_slice<P: Pod>(b: &[u8]) -> P {
    assert!(b.len() >= std::mem::size_of::<P>());
    unsafe {
        let mut res = MaybeUninit::<P>::uninit();
        std::ptr::copy_nonoverlapping(
            b.as_ptr(),
            res.as_mut_ptr().cast(),
            std::mem::size_of::<P>(),
        );
        res.assume_init()
    }
}

pub fn to_slice<P: Pod>(b: &mut [u8], p: P) {
    assert!(b.len() >= std::mem::size_of::<P>());
    unsafe {
        std::ptr::copy_nonoverlapping(
            (&p as *const P).cast::<u8>(),
            b.as_mut_ptr(),
            std::mem::size_of::<P>(),
        );
    }
}
