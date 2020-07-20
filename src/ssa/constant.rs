use crate::interner::StringId;
use std::{cmp, hash, mem};

pub struct Undefined;
pub struct Null;

#[derive(Clone, Debug)]
pub enum Constant {
    String(StringId),
    Float(f64),
    Integer(i32),
    Boolean(bool),
    Undefined,
    Null,
}

impl cmp::PartialEq<Constant> for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match *self {
            Constant::String(a) => {
                if let Constant::String(b) = *other {
                    a == b
                } else {
                    false
                }
            }
            Constant::Float(a) => {
                if let Constant::Float(b) = *other {
                    a.to_bits() == b.to_bits()
                } else {
                    false
                }
            }
            Constant::Integer(a) => {
                if let Constant::Integer(b) = *other {
                    a == b
                } else {
                    false
                }
            }
            Constant::Boolean(a) => {
                if let Constant::Boolean(b) = *other {
                    a == b
                } else {
                    false
                }
            }
            Constant::Null => {
                if let Constant::Null = *other {
                    true
                } else {
                    false
                }
            }
            Constant::Undefined => {
                if let Constant::Undefined = *other {
                    true
                } else {
                    false
                }
            }
        }
    }
}

impl hash::Hash for Constant {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match *self {
            Constant::String(ref x) => x.hash(state),
            Constant::Float(ref x) => x.to_bits().hash(state),
            Constant::Integer(ref x) => x.hash(state),
            Constant::Boolean(ref x) => x.hash(state),
            Constant::Null => {}
            Constant::Undefined => {}
        }
    }
}

impl Eq for Constant {}

impl From<StringId> for Constant {
    fn from(v: StringId) -> Self {
        Constant::String(v)
    }
}

impl From<f64> for Constant {
    fn from(v: f64) -> Self {
        Constant::Float(v)
    }
}
impl From<i32> for Constant {
    fn from(v: i32) -> Self {
        Constant::Integer(v)
    }
}
impl From<bool> for Constant {
    fn from(v: bool) -> Self {
        Constant::Boolean(v)
    }
}
impl From<Undefined> for Constant {
    fn from(_: Undefined) -> Self {
        Constant::Undefined
    }
}
impl From<Null> for Constant {
    fn from(_: Null) -> Self {
        Constant::Null
    }
}
