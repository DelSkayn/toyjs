//! The javascript value implementation.

mod tagged_union;
pub use tagged_union::Value;

use crate::{gc::GcRoot, object::Object};

/// A value which is rooted an thus remains alive between collection cycles.
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ValueRoot<'gc, 'cell>(Value<'gc, 'cell>);

impl<'gc, 'cell> std::ops::Deref for ValueRoot<'gc, 'cell> {
    type Target = Value<'gc, 'cell>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'gc, 'cell> ValueRoot<'gc, 'cell> {
    #[inline]
    pub fn into_object(self) -> Option<GcRoot<'gc, 'cell, Object>> {
        unsafe { self.0.into_object().map(|x| GcRoot::assume_rooted(x)) }
    }

    #[inline]
    pub fn into_string(self) -> Option<GcRoot<'gc, 'cell, String>> {
        unsafe { self.0.into_string().map(|x| GcRoot::assume_rooted(x)) }
    }
}

impl<'gc, 'cell> From<GcRoot<'gc, 'cell, String>> for ValueRoot<'gc, 'cell> {
    fn from(v: GcRoot<'gc, 'cell, String>) -> Self {
        ValueRoot(Value::from(*v))
    }
}

impl<'gc, 'cell> From<GcRoot<'gc, 'cell, Object>> for ValueRoot<'gc, 'cell> {
    fn from(v: GcRoot<'gc, 'cell, Object>) -> Self {
        ValueRoot(Value::from(*v))
    }
}
