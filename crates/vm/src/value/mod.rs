//! The javascript value implementation.

#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
mod nan_tagged;
#[cfg(all(not(feature = "tagged-union"), target_pointer_width = "64"))]
pub use nan_tagged::Value;

#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
mod tagged_union;
#[cfg(any(feature = "tagged-union", not(target_pointer_width = "64")))]
pub use tagged_union::Value;
