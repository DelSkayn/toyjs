#![allow(dead_code)]
#![allow(clippy::new_without_default)]
#![allow(clippy::missing_safety_doc)]


#[macro_export]
macro_rules! rebind_try {
    ($arena:expr, $value:expr) => {
        match $value {
            Ok(x) => x,
            Err(e) => return Err(dreck::rebind!($arena, e)),
        }
    };
}

pub mod exec;
pub mod instructions;
pub mod object;
pub mod realm;
pub mod value;

pub use object::{GcObject, Object};
pub use realm::Realm;
pub use value::Value;
