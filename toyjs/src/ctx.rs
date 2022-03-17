use std::{
    alloc::Global, borrow::Cow, cell::Cell, marker::PhantomData, string::String as StdString,
};

use ast::SymbolTable;
use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

use crate::{ffi::Arguments, Context, Function, Object, String, Value};

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
    marker: PhantomData<Cell<&'js Context>>,
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

    /// Returns the global object of the current context.
    pub fn global(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).realm.global();
            Object::wrap(self, object)
        }
    }

    /// Creates a javascript string from a rust string.
    pub fn create_string(self, s: impl Into<StdString>) -> String<'js> {
        unsafe {
            let string = (*self.ctx).realm.create_string(s.into());
            (*self.ctx).realm.stack.push(string.into());
            String::wrap(self, string)
        }
    }

    /// Creates a new empty object
    pub fn create_object(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).realm.create_object();
            (*self.ctx).realm.stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_string(self, v: Value<'js>) -> Cow<'js, str> {
        unsafe { (*self.ctx).realm.coerce_string(v.into_vm()) }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_number(self, v: Value<'js>) -> Value<'js> {
        unsafe { Value::wrap(self, (*self.ctx).realm.coerce_number(v.into_vm())) }
    }

    pub fn coerce_integer(self, v: Value<'js>) -> i32 {
        unsafe { (*self.ctx).realm.convert_int(v.into_vm()) }
    }

    /// Creates a new function from a rust closure
    pub fn create_function<F>(self, f: F) -> Function<'js>
    where
        F: for<'a> Fn(Ctx<'a>, Arguments<'a>) -> Result<Value<'a>, Value<'a>> + 'static,
    {
        unsafe {
            let inner = self.ctx;
            let function = (*self.ctx).realm.create_function(move |_| {
                let ctx = Ctx {
                    ctx: inner,
                    marker: PhantomData,
                };
                let args = Arguments::from_ctx(ctx);
                f(ctx, args).map(Value::into_vm).map_err(Value::into_vm)
            });
            (*self.ctx).realm.stack.push(function.into());
            Function::wrap(self, function)
        }
    }

    /// Evaluates the given value as a script
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
