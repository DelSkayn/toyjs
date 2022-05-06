use std::alloc::Allocator;

use ast::Literal;
use common::{
    atom::{Atom, Atoms},
    collections::HashMap,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use vm::{gc, Value};

newtype_key! {
    pub struct ConstantId(pub(crate) u32);
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum LiteralOrAtom {
    Literal(Literal),
    Atom(Atom),
}

pub struct Constants<'a, 'rt, 'cell, A: Allocator> {
    constants: SlotStack<Value<'a, 'cell>, ConstantId, A>,
    map: HashMap<LiteralOrAtom, ConstantId>,
    gc: &'a gc::Arena<'rt, 'cell>,
    atoms: &'a Atoms,
}

impl<'a, 'rt, 'cell, A: Allocator> Constants<'a, 'rt, 'cell, A> {
    pub fn new_in(atoms: &'a Atoms, gc: &'a gc::Arena<'rt, 'cell>, alloc: A) -> Self {
        Constants {
            constants: SlotStack::new_in(alloc),
            map: HashMap::default(),
            gc,
            atoms,
        }
    }

    pub fn push_string(&mut self, s: impl AsRef<str>) -> ConstantId {
        let id = self.atoms.atomize_string(s.as_ref());
        self.push_constant(Literal::String(id))
    }

    pub fn push_atom(&mut self, s: Atom) -> ConstantId {
        *self
            .map
            .entry(LiteralOrAtom::Atom(s))
            .or_insert_with(|| self.constants.push(Value::from(s)))
    }

    pub fn push_constant(&mut self, literal: Literal) -> ConstantId {
        *self
            .map
            .entry(LiteralOrAtom::Literal(literal))
            .or_insert_with(|| match literal {
                Literal::Null => self.constants.push(Value::null()),
                Literal::Undefined => self.constants.push(Value::undefined()),
                Literal::Float(x) => self.constants.push(Value::from(x)),
                Literal::Boolean(x) => self.constants.push(Value::from(x)),
                Literal::Integer(x) => self.constants.push(Value::from(x)),
                Literal::String(x) => {
                    let s = self.gc.add(self.atoms.lookup(x).unwrap());
                    self.constants.push(s.into())
                }
            })
    }

    pub fn into_constants(self) -> Box<[Value<'a, 'cell>], A> {
        self.constants.into_vec().into_boxed_slice()
    }
}
