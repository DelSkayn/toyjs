use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};

use common::{result::ContextError, source::Source, string::String, structs::Interners};
use lexer::Lexer;
use parser::Parser;
use toyjs_compiler::Compiler;

pub enum Error {
    Parse(parser::Error),
    Compile(toyjs_compiler::Error),
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Error::Parse(value)
    }
}

impl From<toyjs_compiler::Error> for Error {
    fn from(value: toyjs_compiler::Error) -> Self {
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
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script()?;
    let mut ast = parser.into_ast();
    let compiler = Compiler::new(&mut interners, &mut ast);
    Ok(compiler.compile_script(res.strict, res.stmt)?)
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;

    let source = String::from_std_str(&buffer);
    let source = common::source::Source::new(source, Some("parse_script"));
    let before = Instant::now();
    let res = compile(&source);
    let elapsed = before.elapsed();
    match res {
        Ok(bc) => {
            println!("{}", bc);
        }
        Err(Error::Parse(e)) => {
            eprintln!("parse error {}", e.supply_context(&source))
        }
        Err(Error::Compile(e)) => {
            eprintln!("compile_error {}", e)
        }
    }
    println!("compiled in {:.4} seconds", elapsed.as_secs_f64());
    Ok(())
}
