#[cfg(not(feature = "untagged"))]
mod tagged;
#[cfg(not(feature = "untagged"))]
pub use tagged::*;

#[cfg(feature = "untagged")]
mod untagged;
#[cfg(feature = "untagged")]
pub use untagged::*;
