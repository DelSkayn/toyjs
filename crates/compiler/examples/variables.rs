use common::{result::ContextError, source::Source, string::String, structs::Interners};
use lexer::Lexer;
use parser::Parser;
use std::{
    env,
    fs::File,
    io::{self, Read},
    time::Instant,
};

use toyjs_compiler::variables;

pub enum Error {
    Parse(parser::Error),
    Compile(toyjs_compiler::Error),
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Error::Parse(value)
    }
}

impl From<toyjs_compiler::Error> for Error {
    fn from(value: toyjs_compiler::Error) -> Self {
        Error::Compile(value)
    }
}

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().nth(1) {
        Ok(Box::new(File::open(x)?) as Box<dyn Read>)
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn compile(source: &Source) -> Result<(), Error> {
    let before = Instant::now();
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script()?;
    let mut ast = parser.into_ast();
    let mut variables = variables::VariablesBuilder::new(&mut ast);
    variables
        .push_scope(variables::ScopeKind::Global { strict: res.strict })
        .unwrap();
    variables.resolve_variables(res.stmt)?;
    let root = variables.pop_scope()?;
    let vars = variables.build();
    let elapsed = before.elapsed();

    println!("{}", vars.render(root, &ast, &interners));
    println!("compiled in {:.4} seconds", elapsed.as_secs_f64());

    Ok(())
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut buffer = std::string::String::new();
    read.read_to_string(&mut buffer)?;

    let source = String::from_std_str(&buffer);
    let source = common::source::Source::new(source, Some("parse_script"));
    let res = compile(&source);
    match res {
        Ok(()) => {}
        Err(Error::Parse(e)) => {
            eprintln!("parse error {}", e.supply_context(&source))
        }
        Err(Error::Compile(e)) => {
            eprintln!("compile_error {}", e.supply_context(&source))
        }
    }
    Ok(())
}
