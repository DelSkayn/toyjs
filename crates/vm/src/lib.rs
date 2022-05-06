#![allow(dead_code)]
#![allow(clippy::new_without_default)]
#![allow(clippy::missing_safety_doc)]

pub mod cell;
pub mod gc;
pub mod instructions;
pub mod object;
pub mod realm;
pub mod value;

pub use object::{GcObject, Object};
pub use realm::Realm;
pub use value::Value;
