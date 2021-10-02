use std::alloc::Allocator;

use ast::Literal;
use common::{
    collections::HashMap,
    interner::Interner,
    newtype_key,
    slotmap::{SlotKey, SlotStack},
};
use runtime::{gc::GcArena, JSValue};

newtype_key! {
    pub struct ConstantId(pub(crate) u32);
}

pub struct Constants<'a, A: Allocator> {
    constants: SlotStack<JSValue, ConstantId, A>,
    map: HashMap<Literal, ConstantId>,
    gc: &'a GcArena,
    interner: &'a Interner,
}

impl<'a, A: Allocator> Constants<'a, A> {
    pub fn new_in(interner: &'a Interner, gc: &'a GcArena, alloc: A) -> Self {
        Constants {
            constants: SlotStack::new_in(alloc),
            map: HashMap::default(),
            gc,
            interner,
        }
    }

    pub fn push_constant(&mut self, literal: Literal) -> ConstantId {
        let constants = &mut self.constants;
        let gc = self.gc;
        let interner = self.interner;
        *self.map.entry(literal).or_insert_with(|| match literal {
            Literal::Float(x) => constants.push(JSValue::from(x)),
            Literal::Boolean(x) => constants.push(JSValue::from(x)),
            Literal::Integer(x) => constants.push(JSValue::from(x)),
            Literal::String(x) => {
                let str = gc.allocate(
                    interner
                        .lookup(x)
                        .expect("symbol name string no longer exists")
                        .to_string(),
                );
                constants.push(JSValue::from(str))
            }
        })
    }

    pub fn into_constants(self) -> Box<[JSValue], A> {
        self.constants.into_vec().into_boxed_slice()
    }
}
