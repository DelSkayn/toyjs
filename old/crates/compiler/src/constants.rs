use std::{alloc::Allocator, collections::hash_map::Entry};

use ast::Literal;
use common::{
    collections::HashMap,
    interner::{Interner, StringId},
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use dreck::Root;
use vm::{atom::Atoms, value::Value};

newtype_key! {
    pub struct ConstantId(pub(crate) u32);
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum LiteralOrAtom {
    Literal(Literal),
    Atom(StringId),
}

pub struct Constants<'l, 'own, A: Allocator> {
    root: &'l Root<'own>,
    interner: &'l mut Interner,
    atoms: &'l mut Atoms<'l, 'own>,
    constants: SlotStack<Value<'l, 'own>, ConstantId, A>,
    map: HashMap<LiteralOrAtom, ConstantId>,
}

impl<'l, 'own, A: Allocator> Constants<'l, 'own, A> {
    pub fn new_in(
        root: &'l Root<'own>,
        atoms: &'l mut Atoms<'l, 'own>,
        interner: &'l mut Interner,
        alloc: A,
    ) -> Self {
        Constants {
            root,
            interner,
            atoms,
            constants: SlotStack::new_in(alloc),
            map: HashMap::default(),
        }
    }

    pub fn push_string(&mut self, s: impl AsRef<str>) -> ConstantId {
        let id = self.interner.intern(s.as_ref());
        self.push_literal(Literal::String(id))
    }

    pub fn push_atom(&mut self, s: StringId) -> ConstantId {
        match self.map.entry(LiteralOrAtom::Atom(s)) {
            Entry::Occupied(x) => *x.get(),
            Entry::Vacant(x) => {
                let atom = self
                    .atoms
                    .atomize_string(self.root, self.interner.lookup(s));
                *x.insert(self.constants.push(Value::from(atom)))
            }
        }
    }

    pub fn push_literal(&mut self, literal: Literal) -> ConstantId {
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
                    let str = self.interner.lookup(x);
                    let atom = self.atoms.atomize_string(self.root, str);
                    self.constants.push(Value::from(atom))
                }
            })
    }

    pub fn into_constants(self) -> Box<[Value<'l, 'own>], A> {
        todo!()
        //self.constants.into_vec().into_boxed_slice()
    }
}
