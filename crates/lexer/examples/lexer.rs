use common::{number::NumberId, string::String, structs::Interners};
use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};
use token::{t, TokenKind};
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
    let mut interners = Interners::default();

    let mut lexer = Lexer::new(source.encoding(), &mut interners);
    let mut tokens = Vec::new();
    let time = Instant::now();
    for t in lexer.by_ref() {
        tokens.push(t);
    }
    let elapsed = time.elapsed();
    for t in &tokens {
        let span = t.span.clone();
        let kind = t.kind_and_data.kind();
        let kind_name = format!("{:?}", kind);
        print!(
            "{:>4}+{:<4}:: {:<20}",
            span.offset(),
            span.size(),
            kind_name
        );
        print!("\t\t\t'{}'", source.encoding().slice(span));
        if kind == t!("ident") {
            let data = t.kind_and_data.data_id();
            println!(" = '{}'", lexer.data.strings.get(data.unwrap()).unwrap());
        } else if kind == t!("string") {
            let data = t.kind_and_data.data_id();
            println!(" = \"{}\"", lexer.data.strings.get(data.unwrap()).unwrap());
        } else if kind == t!("123") {
            let data = t.kind_and_data.data_id::<NumberId>();
            println!(" = {:e}", lexer.data.numbers[data.unwrap()].0);
        } else if kind == t!("123n") {
            let data = t.kind_and_data.data_id();
            println!(" = {}n", lexer.data.strings.get(data.unwrap()).unwrap());
        } else if let TokenKind::Template(_) = kind {
            let data = t.kind_and_data.data_id();
            println!(" = `{}`", lexer.data.strings.get(data.unwrap()).unwrap());
        } else {
            println!()
        }
    }
    eprintln!(
        "> lexed {} tokens in {:.4} seconds",
        tokens.len(),
        elapsed.as_secs_f64()
    );
    eprintln!(
        "> strings {}, numbers {}",
        lexer.data.strings.len(),
        lexer.data.numbers.len()
    );
    Ok(())
}
