use common::{result::ContextError, source::Source, string::String, structs::Interners};
use compiler::Compiler;
use dreck::{dreck, Arena, Owner};
use lexer::Lexer;
use parser::Parser;
use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};
use toyjs_vm::{exec::ExecutionFrame, object::Object, value::Value, Context, Vm};

pub enum Error {
    Parse(parser::Error),
    Compile(compiler::Error),
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Error::Parse(value)
    }
}

impl From<compiler::Error> for Error {
    fn from(value: compiler::Error) -> Self {
        Error::Compile(value)
    }
}

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().nth(1) {
        Ok(Box::new(File::open(x)?) as Box<dyn Read>)
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn run<'gc, 'own>(
    source: &Source,
    gc: &'gc Arena<'own>,
    owner: &'gc mut Owner<'own>,
) -> Result<Result<Value<'gc, 'own>, Value<'gc, 'own>>, Error> {
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script()?;
    let mut ast = parser.into_ast();
    let compiler = Compiler::new(&mut interners, &mut ast);
    let bc = compiler.compile_script(res.strict, res.stmt)?;
    let bc = gc.add(bc);

    let vm = Vm::new();
    let vm = gc.add(vm);

    let global = Object::new();
    let global = gc.add(global);

    let context = Context::new(global, vm);
    let context = gc.add(context);

    let mut exec = ExecutionFrame::new(context);

    let function = Object::entry_function(bc);
    let function = gc.add(function);

    Ok(exec.enter(function, owner))
}

fn main() -> Result<(), io::Error> {
    dreck!(owner, arena);

    let mut read = get_input()?;
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;

    let source = String::from_std_str(&buffer);
    let source = common::source::Source::new(source, Some("parse_script"));
    let before = Instant::now();
    let res = run(&source, &arena, &mut owner);
    let elapsed = before.elapsed();
    match res {
        Ok(res) => {
            println!("{:?}", res);
        }
        Err(Error::Parse(e)) => {
            eprintln!("{}", e.supply_context(&source))
        }
        Err(Error::Compile(e)) => {
            eprintln!("Compile error: {}", e)
        }
    }
    println!("compiled in {:.4} seconds", elapsed.as_secs_f64());
    Ok(())
}
