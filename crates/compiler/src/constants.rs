use std::alloc::Allocator;

use ast::Literal;
use common::{
    collections::HashMap,
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use vm::{gc::GcArena, Value};

newtype_key! {
    pub struct ConstantId(pub(crate) u32);
}

pub struct Constants<'a, A: Allocator> {
    constants: SlotStack<Value, ConstantId, A>,
    map: HashMap<Literal, ConstantId>,
    gc: &'a GcArena,
    interner: &'a mut Interner,
}

impl<'a, A: Allocator> Constants<'a, A> {
    pub fn new_in(interner: &'a mut Interner, gc: &'a GcArena, alloc: A) -> Self {
        Constants {
            constants: SlotStack::new_in(alloc),
            map: HashMap::default(),
            gc,
            interner,
        }
    }

    pub fn push_string(&mut self, s: impl AsRef<str>) -> ConstantId {
        let id = self.interner.intern(s.as_ref());
        self.push_constant(Literal::String(id))
    }

    pub fn push_constant(&mut self, literal: Literal) -> ConstantId {
        let constants = &mut self.constants;
        let gc = self.gc;
        let interner = &*self.interner;
        *self.map.entry(literal).or_insert_with(|| match literal {
            Literal::Null => constants.push(Value::null()),
            Literal::Undefined => constants.push(Value::undefined()),
            Literal::Float(x) => constants.push(Value::from(x)),
            Literal::Boolean(x) => constants.push(Value::from(x)),
            Literal::Integer(x) => constants.push(Value::from(x)),
            Literal::String(x) => {
                let str = gc.allocate(
                    interner
                        .lookup(x)
                        .expect("symbol name string no longer exists")
                        .to_string(),
                );
                constants.push(Value::from(str))
            }
        })
    }

    pub fn into_constants(self) -> Box<[Value], A> {
        self.constants.into_vec().into_boxed_slice()
    }
}
