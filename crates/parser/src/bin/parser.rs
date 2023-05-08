use ast::{RenderAst, RenderCtx};
use common::string::String;
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
    let lexer = Lexer::new(source.source());
    let before = Instant::now();
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script();
    let elapsed = before.elapsed();
    println!("parsed in {:.4} seconds", elapsed.as_secs_f64());
    match res {
        Ok(x) => {
            let ctx = RenderCtx::new(
                &parser.ast,
                &parser.lexer.data.strings,
                &parser.lexer.data.numbers,
            );
            let out = std::io::stdout();
            let res = {
                let mut lock = out.lock();
                x.render(&ctx, &mut lock)
            };
            res.unwrap();
        }
        Err(e) => {
            eprintln!("{}", e.display(&source))
        }
    }
    Ok(())
}
