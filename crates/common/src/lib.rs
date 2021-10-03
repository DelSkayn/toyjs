#![feature(allocator_api)]

#[macro_use]
pub mod slotmap;
pub mod bitmap;
pub mod cell_vec;
pub mod interner;
pub mod source;

pub mod collections {
    pub use fxhash::FxHashMap as HashMap;
    pub use fxhash::FxHashSet as HashSet;
}

#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{
            const ASSERT: bool = $x;
            ASSERT
        } as usize] = [];
    };
}
