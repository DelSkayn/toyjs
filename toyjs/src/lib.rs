#![allow(dead_code)]
#![feature(allocator_api)]

use std::{alloc::Global, cell::RefCell, fmt, mem, string::String as StdString};

use ast::SymbolTable;
use common::interner::Interner;
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use vm::{
    atom::Atoms,
    cell::{CellOwner, Id},
    gc::{Arena, OwnedGc, Roots},
    realm::GcRealm,
};

mod lock;
use lock::Lock;

mod value;
pub use value::Value;

mod string;
pub use string::String;

mod error;
pub use error::{Error, Result};

struct ToyJsInner {
    atoms: Atoms,
    roots: Roots<'static>,
}

pub struct ToyJs(Lock<ToyJsInner>);

impl ToyJs {
    pub fn new() -> Self {
        ToyJs(Lock::new(ToyJsInner {
            atoms: Atoms::new(),
            roots: Roots::new(),
        }))
    }
}

pub struct Realm {
    vm: Lock<ToyJsInner>,
    realm: OwnedGc<'static, 'static, vm::Realm<'static, 'static>>,
    interner: RefCell<Interner>,
}

impl Realm {
    pub fn new(toyjs: &ToyJs) -> Self {
        let vm = toyjs.0.clone();
        let borrow = vm.lock();
        let mut owner = unsafe { CellOwner::new(Id::new()) };
        let arena = vm::gc::Arena::new(&borrow.roots);
        let realm = vm::Realm::new(&mut owner, &arena, &borrow.atoms);
        let realm = arena.add(realm);
        let realm = unsafe { std::mem::transmute(borrow.roots.add_owned(realm)) };

        Realm {
            vm: toyjs.0.clone(),
            realm,
            interner: RefCell::new(Interner::new()),
        }
    }

    pub fn with<F, R>(&self, f: F) -> R
    where
        F: for<'js> FnOnce(Ctx<'js>) -> R,
    {
        let borrow = self.vm.lock();
        unsafe {
            let _frame = borrow.roots.frame();
            let context = Context {
                atoms: &borrow.atoms,
                realm: mem::transmute(*self.realm),
                root: mem::transmute(&borrow.roots),
                interner: &self.interner,
            };

            f(Ctx {
                context: &context,
                id: Id::new(),
            })
        }
    }
}

struct Context<'js> {
    pub realm: GcRealm<'js, 'js>,
    pub root: &'js Roots<'js>,
    pub atoms: &'js Atoms,
    pub interner: &'js RefCell<Interner>,
}

#[derive(Clone, Copy)]
pub struct Ctx<'js> {
    pub(crate) context: &'js Context<'js>,
    id: Id<'js>,
}

impl<'js> fmt::Debug for Ctx<'js> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ctx").finish_non_exhaustive()
    }
}

impl<'js> Ctx<'js> {
    pub(crate) fn root_value(self, v: vm::Value<'_, 'js>) -> vm::Value<'js, 'js> {
        unsafe {
            if let Some(obj) = v.into_object() {
                (*self.context).root.push(obj);
            } else if let Some(s) = v.into_string() {
                (*self.context).root.push(s);
            }
            vm::gc::rebind(v)
        }
    }

    pub(crate) fn wrap_error(
        self,
        r: std::result::Result<vm::Value<'_, 'js>, vm::Value<'_, 'js>>,
    ) -> Result<'js, Value<'js>> {
        match r {
            Ok(x) => Ok(Value::from_vm(self, self.root_value(x))),
            Err(e) => Err(Error::Value(Value::from_vm(self, self.root_value(e)))),
        }
    }

    pub fn eval<S: Into<StdString>>(self, source: S) -> Result<'js, Value<'js>> {
        let mut owner = unsafe { CellOwner::new(self.id) };
        let mut arena = Arena::new(&self.context.root);

        let source = common::source::Source::from_string(source.into());
        let mut interner = self.context.interner.borrow_mut();
        let lexer = Lexer::new(&source, &mut interner);
        let mut symbol_table = SymbolTable::new();
        let script = match Parser::parse_script(lexer, &mut symbol_table, Global) {
            Ok(x) => x,
            Err(e) => return Err(Error::Syntax(format!("{}", e.format(&source, &interner)))),
        };

        let bc = Compiler::compile_script(
            &script,
            &symbol_table,
            &mut interner,
            self.context.atoms,
            &arena,
            Global,
        );
        let bc = arena.add(bc);
        vm::root!(arena, bc);

        match unsafe {
            self.context
                .realm
                .eval(&mut arena, &mut owner, self.context.atoms, bc)
        } {
            Ok(x) => Ok(Value::from_vm(self, x)),
            Err(e) => Err(Error::Value(Value::from_vm(self, e))),
        }
    }

    pub fn to_string(self, v: Value<'js>) -> Result<'js, String<'js>> {
        let mut owner = unsafe { CellOwner::new(self.id) };
        let mut arena = Arena::new(&self.context.root);

        let r =
            self.context
                .realm
                .to_string(&mut owner, &mut arena, self.context.atoms, v.into_vm());

        match r {
            Ok(x) => Ok(String::from_vm(self, x)),
            Err(e) => Err(Error::from_vm(self, e)),
        }
    }
}
