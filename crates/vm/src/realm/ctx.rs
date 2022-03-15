use std::{borrow::Cow, cell::Cell, marker::PhantomData};

use crate::{function::Function, gc::BoundGc, object::Object, value::BoundValue, Realm, Value};

pub struct Arguments<'a> {
    pub(crate) frame: *mut Value,
    pub(crate) end: *mut Value,
    pub(crate) marker: PhantomData<Cell<&'a ()>>,
}

impl<'a> Arguments<'a> {
    pub fn get(&self, idx: usize) -> Option<BoundValue<'a>> {
        unsafe {
            let ptr = self.frame.add(idx);
            if ptr < self.end {
                Some(BoundValue::bind(ptr.read()))
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        unsafe { self.end.offset_from(self.frame) as usize }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> ArgumentsIter<'a> {
        ArgumentsIter {
            cur: self.frame,
            end: self.end,
            marker: PhantomData,
        }
    }
}

pub struct ArgumentsIter<'a> {
    cur: *mut Value,
    end: *mut Value,
    marker: PhantomData<Cell<&'a ()>>,
}

impl<'a> Iterator for ArgumentsIter<'a> {
    type Item = BoundValue<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.cur < self.end {
                let res = self.cur.read();
                self.cur = self.cur.add(1);
                Some(BoundValue::bind(res))
            } else {
                None
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a> ExactSizeIterator for ArgumentsIter<'a> {
    fn len(&self) -> usize {
        unsafe { self.end.offset_from(self.cur) as usize }
    }
}

#[derive(Clone, Copy)]
pub struct RealmCtx<'a> {
    pub(crate) realm: *mut Realm,
    pub(super) marker: PhantomData<Cell<&'a ()>>,
}

impl<'a> RealmCtx<'a> {
    pub fn global(&self) -> BoundGc<'a, Object> {
        unsafe { BoundGc::bind((*self.realm).global) }
    }

    pub fn coerce_string(&self, value: BoundValue<'a>) -> Cow<'a, str> {
        unsafe { (*self.realm).coerce_string(value.unbind()) }
    }

    pub fn create_function<F>(&self, f: F) -> BoundGc<'a, Function>
    where
        F: for<'b> Fn(RealmCtx<'b>, Arguments<'b>) -> BoundValue<'b> + 'static,
    {
        unsafe {
            let value = (*self.realm).create_function(f);
            (*self.realm).stack.push_temp(Value::from(value));
            BoundGc::bind(value)
        }
    }

    pub fn create_object(&self) -> BoundGc<'a, Object> {
        unsafe {
            let value = (*self.realm).create_object();
            (*self.realm).stack.push_temp(Value::from(value));
            BoundGc::bind(value)
        }
    }

    pub fn create_string<S: Into<String>>(&self, s: S) -> BoundGc<'a, String> {
        unsafe {
            let value = (*self.realm).create_string(s.into());
            (*self.realm).stack.push_temp(Value::from(value));
            BoundGc::bind(value)
        }
    }

    pub fn frame<'b: 'a, R, F: FnOnce(RealmCtx<'b>) -> R>(&self, f: F) -> R {
        unsafe {
            (*self.realm).stack.enter(0);
            let res = f(RealmCtx {
                realm: self.realm,
                marker: PhantomData,
            });
            (*self.realm).stack.pop();
            res
        }
    }
}
