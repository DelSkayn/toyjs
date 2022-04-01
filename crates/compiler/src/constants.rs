use std::alloc::Allocator;

use ast::Literal;
use common::{
    collections::HashMap,
    interner::{Interner, StringId},
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use vm::{atom::Atoms, gc::GcArena, Value};

newtype_key! {
    pub struct ConstantId(pub(crate) u32);
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum LiteralOrAtom {
    Literal(Literal),
    Atom(StringId),
}

pub struct Constants<'a, A: Allocator> {
    constants: SlotStack<Value, ConstantId, A>,
    map: HashMap<LiteralOrAtom, ConstantId>,
    gc: &'a GcArena,
    interner: &'a mut Interner,
    atoms: &'a Atoms,
}

impl<'a, A: Allocator> Constants<'a, A> {
    pub fn new_in(interner: &'a mut Interner, atoms: &'a Atoms, gc: &'a GcArena, alloc: A) -> Self {
        Constants {
            constants: SlotStack::new_in(alloc),
            map: HashMap::default(),
            gc,
            interner,
            atoms,
        }
    }

    pub fn push_string(&mut self, s: impl AsRef<str>) -> ConstantId {
        let id = self.interner.intern(s.as_ref());
        self.push_constant(Literal::String(id))
    }

    pub fn push_atom(&mut self, s: StringId) -> ConstantId {
        *self.map.entry(LiteralOrAtom::Atom(s)).or_insert_with(|| {
            let atom = self.atoms.atomize_string(
                self.interner
                    .lookup(s)
                    .expect("symbol name string no longer exists"),
            );
            self.constants.push(Value::from(atom))
        })
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
                    let s = self
                        .gc
                        .allocate(self.interner.lookup(x).unwrap().to_string());
                    self.constants.push(s.into())
                }
            })
    }

    pub fn into_constants(self) -> Box<[Value], A> {
        self.constants.into_vec().into_boxed_slice()
    }
}
