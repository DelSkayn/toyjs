use ast::Literal;
use common::{collections::HashMap, interner::StringId, newtype_index};
use std::{cmp, hash, mem};

newtype_index! {
    pub struct ConstantId
}

newtype_index! {
    pub struct ConstStringId
}

pub struct Constants {
    next_id: usize,
    next_string_id: usize,
    pub ids: HashMap<Constant, ConstantId>,
    pub string_ids: HashMap<StringId, ConstStringId>,
}

impl Constants {
    pub fn new() -> Self {
        Constants {
            next_id: 0,
            next_string_id: 0,
            ids: HashMap::default(),
            string_ids: HashMap::default(),
        }
    }

    pub fn add(&mut self, constant: Constant) -> ConstantId {
        let next_id = &mut self.next_id;
        *self.ids.entry(constant).or_insert_with(|| {
            let id = *next_id;
            *next_id += 1;
            ConstantId::from(id)
        })
    }

    pub fn add_string(&mut self, string: StringId) -> ConstStringId {
        let next_id = &mut self.next_string_id;
        *self.string_ids.entry(string).or_insert_with(|| {
            let id = *next_id;
            *next_id += 1;
            ConstStringId::from(id)
        })
    }
}

#[derive(Clone, Debug)]
pub enum Constant {
    Float(f64),
    Integer(i32),
    Boolean(bool),
    Undefined,
    Null,
}

impl cmp::PartialEq<Constant> for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match *self {
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
            Constant::Float(ref x) => x.to_bits().hash(state),
            Constant::Integer(ref x) => x.hash(state),
            Constant::Boolean(ref x) => x.hash(state),
            Constant::Null => {}
            Constant::Undefined => {}
        }
    }
}

impl Eq for Constant {}
