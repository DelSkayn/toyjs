use std::{
    alloc::Global, borrow::Cow, cell::Cell, marker::PhantomData, string::String as StdString,
};

use ast::SymbolTable;
use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

use crate::{ffi::Arguments, Function, Object, String, Value};

pub(crate) struct ContextInner {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub alloc: Bump,
    pub realm: vm::Realm,
}

impl ContextInner {
    pub fn new() -> Self {
        ContextInner {
            symbol_table: SymbolTable::new(),
            interner: Interner::new(),
            alloc: Bump::new(),
            realm: vm::Realm::new(),
        }
    }

    pub unsafe fn push_frame(&mut self) {
        self.realm.stack.enter(0);
    }

    pub unsafe fn pop_frame(&mut self) {
        self.realm.stack.pop();
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Ctx<'js> {
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) ctx: *mut ContextInner,
    marker: PhantomData<Cell<&'js ()>>,
}

impl<'js> Ctx<'js> {
    // # Safety
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) unsafe fn wrap(ctx: &mut ContextInner) -> Self {
        Ctx {
            ctx,
            marker: PhantomData,
        }
    }

    // Push a value onto the vm stack inorder to keep the value alive
    pub(crate) fn push_value(self, value: vm::Value) {
        unsafe {
            if value.requires_gc() {
                (*self.ctx).realm.stack.push(value);
            }
        }
    }

    pub fn global(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).realm.global();
            Object::wrap(self, object)
        }
    }

    pub fn create_string(self, s: impl Into<StdString>) -> String<'js> {
        unsafe {
            let string = (*self.ctx).realm.create_string(s.into());
            (*self.ctx).realm.stack.push(string.into());
            String::wrap(self, string)
        }
    }

    pub fn create_object(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).realm.create_object();
            (*self.ctx).realm.stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    pub fn coerce_string(self, v: Value<'js>) -> Cow<'js, str> {
        unsafe { (*self.ctx).realm.coerce_string(v.into_vm()) }
    }

    pub fn create_function<F>(self, f: F) -> Function<'js>
    where
        F: for<'a> Fn(Ctx<'a>, Arguments<'a>) -> Value<'a> + 'static,
    {
        unsafe {
            let inner = self.ctx;
            let function = (*self.ctx).realm.create_function(move |_| {
                let ctx = Ctx {
                    ctx: inner,
                    marker: PhantomData,
                };
                let args = Arguments::from_ctx(ctx);
                f(ctx, args).into_vm()
            });
            (*self.ctx).realm.stack.push(function.into());
            Function::wrap(self, function)
        }
    }

    pub fn eval(self, s: impl Into<StdString>) -> Result<Value<'js>, Value<'js>> {
        unsafe {
            let source = Source::from_string(s.into());
            let lexer = Lexer::new(&source, &mut (*self.ctx).interner);
            let ast = Parser::parse_script(lexer, &mut (*self.ctx).symbol_table, Global)
                .map_err(|_| Value::undefined(self))?;
            let bytecode = Compiler::compile_script(
                &ast,
                &(*self.ctx).symbol_table,
                &mut (*self.ctx).interner,
                &(*self.ctx).realm.gc,
                Global,
            );
            let bytecode = (*self.ctx).realm.gc.allocate(bytecode);
            std::mem::drop(ast);
            let value = (*self.ctx).realm.eval(bytecode).map_err(|x| {
                self.push_value(x);
                Value::wrap(self, x)
            })?;
            self.push_value(value);
            Ok(Value::wrap(self, value))
        }
    }
}

/*
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
*/
