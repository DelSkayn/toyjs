use std::ptr::NonNull;

use crate::Value;

pub struct Stack {
    root: NonNull<Value>,
    frame: *mut Value,
    stack: *mut Value,
    capacity: usize,
}
