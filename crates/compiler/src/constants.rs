use ast::Literal;
use common::{collections::HashMap, interner::StringId, newtype_index};
use std::{cmp, hash, mem};

newtype_index! {
    pub struct ConstantId
}

pub struct Constants {
    constants: Vec<Constant>,
    ids: HashMap<Constant, ConstantId>,
}

impl Constants {
    pub fn new() -> Self {
        Constants {
            constants: Vec::new(),
            ids: HashMap::default(),
        }
    }

    pub fn add(&mut self, constant: Constant) -> ConstantId {
        let constants = &mut self.constants;
        *self.ids.entry(constant.clone()).or_insert_with(|| {
            let id = ConstantId::from(constants.len());
            constants.push(constant);
            id
        })
    }

    pub fn lookup(&self, id: ConstantId) -> &Constant {
        &self.constants[usize::from(id)]
    }
}

#[derive(Clone, Debug)]
pub enum Constant {
    String(StringId),
    Float(f64),
    Integer(i32),
    Boolean(bool),
    Undefined,
    Null,
}

impl Constant {
    pub fn from_literal(literal: Literal) -> Self {
        match literal {
            Literal::String(x) => Constant::String(x),
            Literal::Float(x) => Constant::Float(x),
            Literal::Integer(x) => Constant::Integer(x),
            Literal::Boolean(x) => Constant::Boolean(x),
        }
    }
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
