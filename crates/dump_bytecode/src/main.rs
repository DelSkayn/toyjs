#![feature(allocator_api)]

use ast::SymbolTable;
use common::{
    atom::{Atoms, Interner},
    source::Source,
};
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use std::{
    alloc::Global,
    env,
    fs::File,
    io::{self, Read},
};
use vm::{gc, rebind};

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().nth(1) {
        Ok(Box::new(File::open(x)?) as Box<dyn Read>)
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut buffer = String::new();
    read.read_to_string(&mut buffer)?;
    let source = Source::from_string(buffer);
    let atoms = Atoms::new();
    let mut interner = Interner::new(&atoms);
    let lexer = Lexer::new(&source, &mut interner);
    let mut variables = SymbolTable::new();
    let (script, symbol_table) = Parser::parse_script(lexer, &mut variables, Global).unwrap();

    let roots = gc::Roots::new();
    let gc = gc::Arena::new(&roots);

    let bytecode = Compiler::compile_script(&script, symbol_table, &atoms, &gc, Global);
    let bytecode = rebind!(&gc, bytecode);
    std::mem::drop(interner);

    println!("{}", atoms);
    println!("{}", bytecode);
    Ok(())
}
