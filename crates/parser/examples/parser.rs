use ast::{RenderAst, RenderCtx};
use common::{result::ContextResultExt, string::String, structs::Interners};
use lexer::Lexer;
use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
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
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;

    let source = String::from_std_str(&buffer);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let before = Instant::now();
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script();
    let elapsed = before.elapsed();
    match res.supply_context(&source) {
        Ok(x) => {
            let ctx = RenderCtx::new(
                &parser.ast,
                &parser.lexer.data.strings,
                &parser.lexer.data.numbers,
            );
            println!("{}", x.display(ctx))
        }
        Err(e) => {
            eprintln!("{}", e)
        }
    }
    println!("parsed in {:.4} seconds", elapsed.as_secs_f64());
    Ok(())
}
