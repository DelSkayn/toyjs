use common::{interner::Interner, source::Source};
use std::{
    env,
    fs::File,
    io::{self, Read},
};
use toyjs_lexer::Lexer;

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
    let mut interner = Interner::new();
    let mut tokens = Vec::new();
    let mut error = None;
    {
        let mut lexer = Lexer::new(&source, &mut interner);
        loop {
            match lexer.next() {
                Ok(Some(x)) => {
                    tokens.push(x);
                }
                Ok(None) => {
                    break;
                }
                Err(e) => {
                    error = Some(e);
                    break;
                }
            }
        }
    }
    for t in tokens.iter() {
        println!("{}\t{:?}", t.kind.format(&interner), t);
    }

    if let Some(e) = error {
        eprintln!("Error lexing: {:?}", e);
    }
    Ok(())
}
