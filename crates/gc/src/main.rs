use std::{alloc::Layout, ptr};

const SEGMENT_SIZE: usize = 4 * 1024 * 1024;

fn main() {
    unsafe {
        dbg!(std::alloc::alloc(
            Layout::from_size_align(SEGMENT_SIZE, SEGMENT_SIZE).unwrap()
        ));
    }
}
