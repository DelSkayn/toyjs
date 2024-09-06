use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};

use common::{number::Number, string::String};
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
    let mut ast = ast::Ast::new();

    let mut lexer = Lexer::new(source.encoding(), &mut ast);
    let mut tokens = Vec::new();
    let time = Instant::now();
    for t in lexer.by_ref() {
        tokens.push(t);
    }
    let elapsed = time.elapsed();
    for t in &tokens {
        let span = t.span;
        let kind = t.kind;
        let kind_name = format!("{:?}", kind);
        print!(
            "{:>4}+{:<4}:: {:<20}",
            span.offset(),
            span.size(),
            kind_name
        );
        print!("\t\t\t'{}'", source.encoding().slice(span));
        if kind == t!("ident") {
            let data = t.data.unwrap().entype::<String>();
            println!(" = {:?} = '{}'", data, lexer[data]);
        } else if kind == t!("string") {
            let data = t.data.unwrap().entype::<String>();
            println!(" = {:?}={:?}", data, lexer[data]);
        } else if kind == t!("123") {
            let data = t.data.unwrap().entype::<Number>();
            println!(" = {:e}", lexer[data].0);
        } else if kind == t!("123n") {
            let data = t.data.unwrap().entype::<String>();
            println!(" = {}n", lexer[data]);
        } else if let TokenKind::Template(_) = kind {
            let data = t.data.unwrap().entype::<String>();
            println!(" = `{}`", lexer[data]);
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
        lexer.ast.library().strings.len(),
        lexer.ast.library().numbers.len()
    );
    Ok(())
}
