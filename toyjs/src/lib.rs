use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use parser::{Parser, Result};
use runtime::{
    bytecode::Bytecode,
    environment::Environment,
    gc::{Gc, GcArena},
    object::Object,
    ExecutionContext, Stack,
};
use std::rc::Rc;

mod value;
pub use value::Value;

pub struct ToyJs {
    gc: GcArena,
    interner: Interner,
    stack: Stack,
    global: Gc<Object>,
}

impl ToyJs {
    pub fn new() -> Self {
        let arena = GcArena::new();
        let global = unsafe { arena.allocate(Object::new()) };
        ToyJs {
            gc: arena,
            interner: Interner::new(),
            stack: Stack::new(),
            global,
        }
    }

    pub fn exec(&mut self, script: &str) -> Result<Value> {
        let bc = self.compile(script)?;
        self.exec_bytecode(&bc)
    }

    pub fn compile(&mut self, script: &str) -> Result<Bytecode> {
        let alloc = Bump::new();
        let mut variables = ast::Variables::new_in(&alloc);
        unsafe {
            for (k, _) in self.global.iter() {
                let string_id = self.interner.intern(k);
                variables.define_global(string_id);
            }
        }
        let source = Source::from_string(script.to_string());
        let parser = Parser::from_source(&source, &mut self.interner, &alloc, &mut variables);
        let script = dbg!(parser.parse_script()?);
        Ok(Compiler::new(&alloc, &self.interner, &variables).compile_script(script))
    }

    pub fn exec_bytecode(&mut self, bc: &Bytecode) -> Result<Value> {
        println!("{}", bc);
        let env = Rc::new(Environment::new(bc.slots, None));
        let mut execution_ctx =
            ExecutionContext::new(&self.gc, self.global.clone(), env, bc, &mut self.stack);
        unsafe { Ok(Value::from_js_value(execution_ctx.run())) }
    }
}

impl Drop for ToyJs {
    fn drop(&mut self) {
        unsafe {
            self.gc.collect_all(&());
        }
    }
}
