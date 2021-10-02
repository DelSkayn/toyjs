#![feature(allocator_api)]

use ast::SymbolTable;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use runtime::gc::GcArena;
use std::{
    alloc::Global,
    env,
    fs::File,
    io::{self, Read},
};

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().skip(1).next() {
        return Ok(Box::new(File::open(x)?) as Box<dyn Read>);
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut buffer = String::new();
    read.read_to_string(&mut buffer)?;
    let source = Source::from_string(buffer);
    let mut interner = Interner::new();
    let lexer = Lexer::new(&source, &mut interner);
    let mut variables = SymbolTable::new();
    let script = Parser::parse_script(lexer, &mut variables, Global).unwrap();
    let gc = GcArena::new();
    let compiler = Compiler::new(&variables, &interner, &gc, Global);
    println!("{}", compiler.compile_script(&script));
    Ok(())
}
