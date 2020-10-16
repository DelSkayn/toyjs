#[macro_use]
pub mod index;
pub mod bitmap;
pub mod bump_list;
pub mod interner;
pub mod source;

pub mod collections {
    pub use fxhash::FxHashMap as HashMap;
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
