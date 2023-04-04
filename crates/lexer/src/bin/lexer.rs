use common::string::String;
use std::{
    env,
    fs::File,
    io::{self, Read},
};
use token::t;
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
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;
    let source = String::from_std_str(&buffer);
    let mut lexer = Lexer::new(source.encoding());
    let mut tokens = Vec::new();
    for t in lexer.by_ref() {
        tokens.push(t);
    }
    for t in &tokens {
        let span = t.span;
        let kind = t.kind_and_data.kind();
        let data = t.kind_and_data.data_id();
        let kind_name = format!("{:?}", kind);
        print!(
            "{:>4}+{:<4}:: {:<20}",
            span.offset(),
            span.size(),
            kind_name
        );
        if kind == t!("ident") {
            println!(" = '{}'", lexer.data.strings[data.unwrap() as usize]);
        } else if kind == t!("num") {
            println!(" = {}", lexer.data.numbers[data.unwrap() as usize]);
        } else if kind == t!("big int") {
            println!(" = {}n", lexer.data.strings[data.unwrap() as usize]);
        } else {
            println!()
        }
    }
    Ok(())
}
