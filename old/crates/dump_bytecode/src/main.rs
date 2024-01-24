#![feature(allocator_api)]

use ast::SymbolTable;
use common::{interner::Interner, source::Source};
use compiler::Compiler;
use dreck::{new_root, rebind, Root};
use lexer::Lexer;
use parser::Parser;
use std::{
    alloc::Global,
    env,
    fs::File,
    io::{self, Read},
};
use vm::atom::Atoms;

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().nth(1) {
        Ok(Box::new(File::open(x)?) as Box<dyn Read>)
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn main() -> Result<(), io::Error> {
    new_root!(owner, root);

    let mut read = get_input()?;
    let mut buffer = String::new();
    read.read_to_string(&mut buffer)?;
    let source = Source::from_string(buffer);
    let mut interner = Interner::new();
    let lexer = Lexer::new(&source, &mut interner);
    let mut variables = SymbolTable::new();
    let (script, symbol_table) = Parser::parse_script(lexer, &mut variables, Global).unwrap();

    let mut atoms = Atoms::new();

    let bytecode = Compiler::compile_script(
        &script,
        symbol_table,
        root,
        &mut atoms,
        &mut interner,
        Global,
    );
    let bytecode = rebind!(&root, bytecode);
    std::mem::drop(interner);

    //println!("{}", atoms);
    println!("{}", bytecode);
    Ok(())
}
