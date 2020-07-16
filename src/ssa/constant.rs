use std::{cmp, hash, mem};

#[derive(Clone)]
pub enum Constant {
    String(String),
    Float(f64),
    Integer(i32),
}

impl cmp::PartialEq<Constant> for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match *self {
            Constant::String(ref a) => {
                if let Constant::String(ref b) = *other {
                    a == b
                } else {
                    false
                }
            }
            Constant::Float(ref a) => {
                if let Constant::Float(ref b) = *other {
                    a.to_bits() == b.to_bits()
                } else {
                    false
                }
            }
            Constant::Integer(ref a) => {
                if let Constant::Integer(ref b) = *other {
                    a == b
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
        }
    }
}

impl Eq for Constant {}

impl From<String> for Constant {
    fn from(v: String) -> Self {
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
