#![feature(allocator_api)]

use ast::SymbolTable;
use common::{atom::Atoms, source::Source};
use lexer::Lexer;
use std::{
    alloc::Global,
    env,
    fs::File,
    io::{self, Read},
};
use toyjs_parser::Parser;

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
    let lexer = Lexer::new(&source, &atoms);
    let mut variables = SymbolTable::new();
    match Parser::parse_script(lexer, &mut variables, Global) {
        Ok(x) => println!("{:#?}", x),
        Err(e) => {
            let formated_error = e.format(&source, &atoms);
            eprintln!("Error parsing script: {}", formated_error);
        }
    }
    Ok(())
}
