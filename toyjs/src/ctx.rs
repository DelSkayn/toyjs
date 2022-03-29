use std::{alloc::Global, cell::Cell, marker::PhantomData, string::String as StdString};

use ast::SymbolTable;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use vm::{
    gc::Trace,
    object::{ObjectFlags, SharedFn},
};

use crate::{
    convert::FromJs, error::Result, ffi::Arguments, Context, Error, Function, Object, String, Value,
};

#[doc(hidden)]
pub struct UserData {
    pub symbol_table: SymbolTable<Global>,
    pub interner: Interner,
    //pub alloc: Bump,
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
            //alloc: Bump::new(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Ctx<'js> {
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) ctx: *const vm::Realm,
    marker: PhantomData<Cell<&'js Context>>,
}

impl<'js> Ctx<'js> {
    // # Safety
    // If created this pointer should remain valid for the entire duration of 'js
    pub(crate) unsafe fn wrap(ctx: *const vm::Realm) -> Self {
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

    pub(crate) unsafe fn user_data(self) -> *mut UserData {
        (*(*self.ctx).user_data).downcast_mut().unwrap()
    }

    /// Returns the global object of the current context.
    pub fn global(self) -> Object<'js> {
        unsafe {
            let object = (*self.ctx).global;
            Object::wrap(self, object)
        }
    }

    /// Creates a javascript string from a rust string.
    pub fn create_string(self, s: impl Into<StdString>) -> String<'js> {
        unsafe {
            let string = (*self.ctx).vm().allocate(s.into());
            (*self.ctx).stack.push(string.into());
            String::wrap(self, string)
        }
    }
    pub fn create_object(self) -> Object<'js> {
        unsafe {
            let object = vm::Object::alloc(
                &(*self.ctx),
                (*self.ctx).builtin.object_proto,
                ObjectFlags::empty(),
            );
            (*self.ctx).stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    /// Creates a new empty object
    pub fn create_object_proto(self, prototype: Option<Object<'js>>) -> Object<'js> {
        unsafe {
            let object = vm::Object::alloc(
                &(*self.ctx),
                prototype.map(|x| x.into_vm()),
                ObjectFlags::empty(),
            );
            (*self.ctx).stack.push(object.into());
            Object::wrap(self, object)
        }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_string(self, v: Value<'js>) -> Result<'js, String<'js>> {
        unsafe {
            (*self.ctx)
                .to_string(v.into_vm())
                .map(|x| String::wrap(self, x))
                .map_err(|e| Error::wrap(self, e))
        }
    }

    /// Coerces a javascript value into a string
    pub fn coerce_number(self, v: Value<'js>) -> Result<'js, Value<'js>> {
        unsafe {
            (*self.ctx)
                .to_number(v.into_vm())
                .map(|x| Value::wrap(self, x))
                .map_err(|e| Error::wrap(self, e))
        }
    }

    pub fn coerce_integer(self, v: Value<'js>) -> Result<'js, i32> {
        unsafe {
            (*self.ctx)
                .to_int32(v.into_vm())
                .map_err(|e| Error::wrap(self, e))
        }
    }

    #[doc(hidden)]
    /// Creates a new function from a rust closure
    pub unsafe fn create_static_function(
        self,
        f: fn(
            &vm::Realm,
            exec: &mut vm::realm::ExecutionContext,
        ) -> std::result::Result<vm::Value, vm::Value>,
    ) -> Function<'js>
where {
        let function = vm::Object::alloc_function(
            &(*self.ctx),
            (*self.ctx).builtin.function_proto,
            ObjectFlags::empty(),
            vm::object::FunctionKind::Static(f),
        );
        self.push_value(function.into());
        Function::wrap(self, function)
    }

    /// Creates a new function from a rust closure
    pub fn create_shared_function<F>(self, f: F) -> Function<'js>
    where
        F: for<'a> Fn(Ctx<'a>, Arguments<'a>) -> Result<'a, Value<'a>> + 'static,
    {
        unsafe {
            let func: SharedFn = Box::new(move |realm: &vm::Realm, _| {
                let ctx = Ctx::wrap(realm);
                let args = Arguments::from_ctx(ctx);
                f(ctx, args).map(Value::into_vm).map_err(|e| e.into_vm(ctx))
            });
            let function = vm::Object::alloc_function(
                &(*self.ctx),
                (*self.ctx).builtin.function_proto,
                ObjectFlags::empty(),
                vm::object::FunctionKind::Shared(func),
            );
            self.push_value(function.into());
            Function::wrap(self, function)
        }
    }

    pub fn compile(self, s: impl Into<StdString>) -> Result<'js, Function<'js>> {
        unsafe {
            let source = Source::from_string(s.into());
            let user_data = self.user_data();
            let lexer = Lexer::new(&source, &mut (*user_data).interner);
            let ast = Parser::parse_script(lexer, &mut (*user_data).symbol_table, Global)
                .map_err(|e| e.format(&source, &(*user_data).interner).to_string())
                .map_err(Error::Parse)?;

            let bytecode = Compiler::compile_script(
                &ast,
                &(*user_data).symbol_table,
                &mut (*user_data).interner,
                (*self.ctx).vm().gc(),
                Global,
            );
            let bytecode = (*self.ctx).vm().allocate(bytecode);
            let function = (*self.ctx).construct_script_function(bytecode);
            self.push_value(function.into());
            Ok(Function::wrap(self, function))
        }
    }

    /// Evaluates the given value as a script
    pub fn eval<R: FromJs<'js>, S: Into<StdString>>(self, s: S) -> Result<'js, R> {
        unsafe {
            let v = self.eval_inner(s.into())?;
            if R::NEEDS_GC {
                self.push_value(v);
            }
            R::from_js(self, Value::wrap(self, v))
        }
    }

    unsafe fn eval_inner(self, s: StdString) -> Result<'js, vm::Value> {
        let source = Source::from_string(s);
        let user_data = self.user_data();
        let lexer = Lexer::new(&source, &mut (*user_data).interner);
        let ast = Parser::parse_script(lexer, &mut (*user_data).symbol_table, Global)
            .map_err(|e| e.format(&source, &(*user_data).interner).to_string())
            .map_err(Error::Parse)?;
        let bytecode = Compiler::compile_script(
            &ast,
            &(*user_data).symbol_table,
            &mut (*user_data).interner,
            (*self.ctx).vm().gc(),
            Global,
        );
        let bytecode = (*self.ctx).vm().allocate(bytecode);
        std::mem::drop(ast);
        (*self.ctx).eval(bytecode).map_err(|e| {
            self.push_value(e);
            Error::wrap(self, e)
        })
    }
}
