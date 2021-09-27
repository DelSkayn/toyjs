#![feature(allocator_api)]

use ast::SymbolTable;
use common::{interner::Interner, source::Source};
use lexer::Lexer;
use std::{
    alloc::Global,
    env,
    fs::File,
    io::{self, Read},
};
use toyjs_parser::Parser;

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
    match Parser::parse_script(lexer, &mut variables, Global) {
        Ok(x) => println!("{:#?}", x),
        Err(e) => {
            let formated_error = e.format(&source, &interner);
            eprintln!("Error parsing script: {}", formated_error);
        }
    }
    Ok(())
}
