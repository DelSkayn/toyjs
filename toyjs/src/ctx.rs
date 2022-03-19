use std::{
    alloc::Global, borrow::Cow, cell::Cell, marker::PhantomData, string::String as StdString,
};

use ast::SymbolTable;
use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use vm::gc::Trace;

use crate::{ffi::Arguments, Context, Function, Object, String, Value};

#[doc(hidden)]
pub struct UserData {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    pub alloc: Bump,
}

unsafe impl Trace for UserData {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn trace(&self, _: vm::gc::Ctx) {}
}

impl UserData {
    pub fn new() -> Self {
        UserData {
            symbol_table: SymbolTable::new(),
            interner: Interner::new(),
            alloc: Bump::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Ctx<'js> {
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) ctx: *mut vm::Realm,
    marker: PhantomData<Cell<&'js Context>>,
}

impl<'js> Ctx<'js> {
    // # Safety
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) unsafe fn wrap(ctx: &mut vm::Realm) -> Self {
        Ctx {
            ctx,
            marker: PhantomData,
        }
    }

    // Push a value onto the vm stack inorder to keep the value alive
    pub(crate) unsafe fn push_value(self, value: vm::Value) {
        if value.requires_gc() {
            (*self.ctx).stack.push(value);
        }
    }

    pub(crate) unsafe fn push_frame(&mut self) {
        (*self.ctx).stack.enter(0);
    }

    pub(crate) unsafe fn pop_frame(&mut self) {
        (*self.ctx).stack.pop();
    }

    pub(crate) unsafe fn user_data(self) -> *mut UserData {
        (*self.ctx).user_data.downcast_mut().unwrap()
    }

    /// Returns the global object of the current context.
    pub fn global(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).global();
            Object::wrap(self, object)
        }
    }

    /// Creates a javascript string from a rust string.
    pub fn create_string(self, s: impl Into<StdString>) -> String<'js> {
        unsafe {
            let string = (*self.ctx).create_string(s.into());
            (*self.ctx).stack.push(string.into());
            String::wrap(self, string)
        }
    }
    pub fn create_object(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).create_object();
            (*self.ctx).stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    /// Creates a new empty object
    pub fn create_object_proto(self, prototype: Option<Object<'js>>) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).create_object_proto(prototype.map(|x| x.into_vm()));
            (*self.ctx).stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_string(self, v: Value<'js>) -> Cow<'js, str> {
        unsafe { (*self.ctx).to_string(v.into_vm()) }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_number(self, v: Value<'js>) -> Value<'js> {
        unsafe { Value::wrap(self, (*self.ctx).to_number(v.into_vm())) }
    }

    pub fn coerce_integer(self, v: Value<'js>) -> i32 {
        unsafe { (*self.ctx).to_int32(v.into_vm()) }
    }

    #[doc(hidden)]
    /// Creates a new function from a rust closure
    pub unsafe fn create_static_function(
        self,
        f: fn(
            &mut vm::Realm,
            exec: &mut vm::realm::ExecutionContext,
        ) -> Result<vm::Value, vm::Value>,
    ) -> Function<'js>
where {
        let function = (*self.ctx).create_static_function(f);
        (*self.ctx).stack.push(function.into());
        Function::wrap(self, function)
    }

    /// Creates a new function from a rust closure
    pub fn create_shared_function<F>(self, f: F) -> Function<'js>
    where
        F: for<'a> Fn(Ctx<'a>, Arguments<'a>) -> Result<Value<'a>, Value<'a>> + 'static,
    {
        unsafe {
            let function = (*self.ctx).create_shared_function(move |realm, _| {
                let ctx = Ctx::wrap(realm);
                let args = Arguments::from_ctx(ctx);
                f(ctx, args).map(Value::into_vm).map_err(Value::into_vm)
            });
            (*self.ctx).stack.push(function.into());
            Function::wrap(self, function)
        }
    }

    pub fn compile(self, s: impl Into<StdString>) -> Result<Function<'js>, Value<'js>> {
        unsafe {
            let source = Source::from_string(s.into());
            let user_data = self.user_data();
            let lexer = Lexer::new(&source, &mut (*user_data).interner);
            let ast = Parser::parse_script(lexer, &mut (*user_data).symbol_table, Global)
                .map_err(|_| Value::undefined(self))?;

            let bytecode = Compiler::compile_script(
                &ast,
                &(*user_data).symbol_table,
                &mut (*user_data).interner,
                &(*self.ctx).gc,
                Global,
            );
            let bytecode = (*self.ctx).gc.allocate(bytecode);
            let function = (*self.ctx).construct_script_function(bytecode);
            (*self.ctx).stack.push(function.into());
            Ok(Function::wrap(self, function))
        }
    }

    /// Evaluates the given value as a script
    pub fn eval(self, s: impl Into<StdString>) -> Result<Value<'js>, Value<'js>> {
        unsafe {
            let source = Source::from_string(s.into());
            let user_data = self.user_data();
            let lexer = Lexer::new(&source, &mut (*user_data).interner);
            let ast = Parser::parse_script(lexer, &mut (*user_data).symbol_table, Global)
                .map_err(|_| Value::undefined(self))?;
            let bytecode = Compiler::compile_script(
                &ast,
                &(*user_data).symbol_table,
                &mut (*user_data).interner,
                &(*self.ctx).gc,
                Global,
            );
            let bytecode = (*self.ctx).gc.allocate(bytecode);
            std::mem::drop(ast);
            let value = (*self.ctx).eval(bytecode).map_err(|x| {
                self.push_value(x);
                Value::wrap(self, x)
            })?;
            self.push_value(value);
            Ok(Value::wrap(self, value))
        }
    }
}
