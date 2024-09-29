use std::{
    env,
    fs::File,
    io::{self, Read},
    rc::Rc,
    time::Instant,
};

use bc::ByteCode;
use common::{result::ContextError, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use toyjs_vm::{gc::OwnedRoot, Vm};

#[derive(Debug)]
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

fn compile(source: &Source) -> Result<bc::ByteCode, Error> {
    let mut ast = ast::Ast::new();
    let lexer = Lexer::new(source.source(), &mut ast);
    let res = Parser::parse_syntax(lexer, parser::parse_script)?;
    let compiler = Compiler::new(&mut ast);
    Ok(compiler.compile_script(res.strict, res.stmt)?)
}

fn run(bc: &ByteCode) -> Result<(), io::Error> {
    let _vm = unsafe { Vm::new().unwrap() };
    Ok(())
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut source = std::string::String::new();
    read.read_to_string(&mut source)?;
    let source = common::source::Source::new(source, Some("parse_script"));
    let before = Instant::now();
    let compile_res = compile(&source);
    let bc = match dbg!(compile_res) {
        Ok(bc) => bc,
        Err(Error::Parse(e)) => {
            eprintln!("parse error {}", e.supply_context(&source));
            return Ok(());
        }
        Err(Error::Compile(e)) => {
            eprintln!("compile_error {}", e);
            return Ok(());
        }
    };
    run(&bc)?;
    Ok(())
}
