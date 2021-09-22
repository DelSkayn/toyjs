use ast::Variables;
use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use parser::Parser;
use std::{
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
    let alloc = Bump::new();
    let mut variables = Variables::new_in(&alloc);
    let parser = Parser::from_source(&source, &mut interner, &alloc, &mut variables);
    match parser.parse_script() {
        Ok(x) => println!("{:#?}", x),
        Err(e) => {
            let formated_error = e.format(&source, &interner);
            eprintln!("Error parsing script: {}", formated_error);
        }
    }
    Ok(())
}
