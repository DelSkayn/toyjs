use crate::{
    //exec::RunningExecution,
    gc::{Ctx, Gc, Trace},
    value::JSValue,
};
use common::collections::HashMap;
use std::{cell::RefCell, cmp::PartialEq, hash, mem};

#[derive(Debug)]
pub struct Object {
    prototype: Option<Gc<Object>>,
    function: Option<()>,
    values: HashMap<String, JSValue>,
    array: Vec<JSValue>,
}

impl Object {
    pub fn new() -> Self {
        Object {
            prototype: None,
            values: HashMap::default(),
            function: None,
            array: Vec::new(),
        }
    }

    /*
    pub unsafe fn get<R>(&self, key: JSValue, ctx: &mut RunningExecution<R>) -> JSValue {
        let string = ctx.convert_string(key);
        self.values
            .get(&string)
            .copied()
            .unwrap_or(JSValue::undefined())
    }

    pub unsafe fn set<R>(&mut self, key: JSValue, value: JSValue, ctx: &mut RunningExecution<R>) {
        let string = ctx.convert_string(key);
        self.values.insert(string, value);
    }
    */

    pub unsafe fn iter(&self) -> impl Iterator<Item = (&String, &JSValue)> {
        self.values.iter()
    }
}

unsafe impl Trace for Object {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, ctx: Ctx) {
        if let Some(x) = self.prototype {
            ctx.mark(x)
        }

        for v in self.values.values() {
            v.trace(ctx);
        }

        for v in self.array.iter() {
            v.trace(ctx);
        }
    }
}
