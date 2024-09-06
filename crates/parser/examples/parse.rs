use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};

use ast::AstRender;
use common::{result::ContextResultExt, string::String};
use lexer::Lexer;
use toyjs_parser::{parse_script, Parser};

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
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut ast = ast::Ast::new();
    let lexer = Lexer::new(source.source(), &mut ast);
    let before = Instant::now();
    let res = Parser::parse_syntax(lexer, parse_script);
    let elapsed = before.elapsed();
    match res.supply_context(&source) {
        Ok(x) => {
            println!("{}", AstRender::new(&ast, x.stmt));
        }
        Err(e) => {
            eprintln!("{}", e)
        }
    }
    println!("parsed in {:.4} seconds", elapsed.as_secs_f64());
    Ok(())
}
