#[macro_use]
pub mod index;
pub mod bitmap;
pub mod interner;
pub mod source;

pub mod collections {
    pub use fxhash::FxHashMap as HashMap;
}
