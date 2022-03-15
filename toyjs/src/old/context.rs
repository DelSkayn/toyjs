use crate::{Inner, ToyJs, Value};
use ast::SymbolTable;
use bumpalo::Bump;
use common::source::Source;
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use runtime::{
    bytecode::RuntimeFunction,
    exec::ExecutionContext,
    gc::{Ctx as GcCtx, Gc, Trace},
    object::Object,
};
use std::{
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    mem,
    sync::Arc,
};

// Super nice trick taken from the rlua library.
// Can be used to pin a lifetime so that all functions which use
// that lifetime can only us that single lifetime and not one which
// is variant over that lifetime
pub type Invariant<'a> = PhantomData<Cell<&'a ()>>;

#[derive(Copy, Clone)]
pub struct Ctx<'js> {
    context: *mut ContextInner,
    __marker: Invariant<'js>,
}

impl<'js> Ctx<'js> {
    pub fn exec(self, source: String, dump: bool) -> Value {
        unsafe {
            let bump = Bump::new();
            let mut variables = SymbolTable::new_in(&bump);
            let source = Source::from_string(source.to_string());
            let inner = (*self.context).js.0.bypass();
            let lexer = Lexer::new(&source, &mut inner.interner);
            let script = match Parser::parse_script(lexer, &mut variables, &bump) {
                Ok(x) => x,
                Err(e) => {
                    source.finish_lines();
                    println!("{}", e.format(&source, &inner.interner));
                    panic!();
                }
            };
            if dump {
                println!("=== AST ===\n{:#?}", script)
            }
            todo!()
        }
    }

    unsafe fn _global(self) -> Gc<Object> {
        (*self.context).global
    }

    unsafe fn _runtime(&self) -> &ToyJs {
        &(*self.context).js
    }
}

pub struct ContextInner {
    global: Gc<Object>,
    js: ToyJs,
}

#[derive(Clone)]
pub struct Context(Arc<UnsafeCell<ContextInner>>);

unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Context {
    pub fn new(js: &ToyJs) -> Self {
        unsafe {
            let lock = js.0.lock().unwrap();
            let inner = ContextInner {
                global: lock.gc.allocate(Object::new()),
                js: js.clone(),
            };
            mem::drop(lock);
            Context(Arc::new(UnsafeCell::new(inner)))
        }
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: FnOnce(Ctx) -> R,
    {
        unsafe {
            let inner = self.0.get();
            let guard = (*inner).js.0.lock().unwrap();
            let ctx = Ctx {
                context: inner,
                __marker: PhantomData,
            };
            let res = f(ctx);
            (*inner).js.0.bypass().root.use_stack.clear();
            mem::drop(guard);
            res
        }
    }
}

unsafe impl Trace for Context {
    fn trace(&self, ctx: GcCtx) {
        unsafe { ctx.mark((*self.0.get()).global) }
    }
}
