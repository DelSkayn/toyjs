#![allow(dead_code)]

macro_rules! root_value {
    ($root:expr, $value:ident) => {
        let mut __guard = None;
        unsafe {
            if let Some(x) = $value.into_object() {
                __guard = Some(dreck::Root::root_gc($root, x));
            } else if let Some(x) = $value.into_string() {
                __guard = Some(dreck::Root::root_gc($root, x));
            } else {
                debug_assert!($value.to_static().is_some())
            }
        }
        let $value = unsafe { dreck::rebind_to(&__guard, $value) };
    };
}

pub mod exec;
pub mod instructions;
pub mod object;
pub mod realm;
pub mod value;
