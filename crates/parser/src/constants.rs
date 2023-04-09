use core::hash::{Hash, Hasher};

use common::interner::Interner;

#[derive(Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    Undefined,
}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Constant::Number(x) => x.to_bits().hash(state),
            Constant::String(x) => x.hash(state),
            Constant::Boolean(x) => x.hash(state),
            Constant::Null | Constant::Undefined => {}
        }
    }
}

impl Eq for Constant {}
impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Constant::Number(a), Constant::Number(b)) => a.to_bits() == b.to_bits(),
            (Constant::String(a), Constant::String(b)) => a == b,
            (Constant::Boolean(a), Constant::Boolean(b)) => a == b,
            (Constant::Null, Constant::Null) => true,
            (Constant::Undefined, Constant::Undefined) => true,
            _ => false,
        }
    }
}
