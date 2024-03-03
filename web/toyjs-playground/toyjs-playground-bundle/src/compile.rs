use std::{fmt::Write, string::String as StdString};

use ast::ListHead;
use common::{result::ContextError, source::Source, string::String, structs::Interners};
use compiler::{
    variables::{self, Kind, Variables},
    Compiler,
};
use lexer::Lexer;
use parser::Parser;

pub enum Error {
    Parse(parser::Error),
    Compile(compiler::Error),
}

impl From<parser::Error> for Error {
    fn from(value: parser::Error) -> Self {
        Error::Parse(value)
    }
}

impl From<compiler::Error> for Error {
    fn from(value: compiler::Error) -> Self {
        Error::Compile(value)
    }
}
fn compile_inner(source: &Source) -> Result<bc::ByteCode, Error> {
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script()?;
    let mut ast = parser.into_ast();
    let compiler = Compiler::new(&mut interners, &mut ast);
    Ok(compiler.compile_script(res.strict, res.stmt)?)
}

pub fn compile(text: &str) -> Result<StdString, StdString> {
    let source = String::from_std_str(text);
    let source = common::source::Source::new(source, Some("parse_script"));
    let bc = compile_inner(&source);
    match bc {
        Ok(bc) => Ok(format!("{}", bc)),
        Err(Error::Parse(e)) => Err(format!("parse error {}", e.supply_context(&source))),
        Err(Error::Compile(e)) => Err(format!("compile_error {}", e)),
    }
}

fn resolve_inner(source: &Source) -> Result<StdString, Error> {
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let resolve = parser.parse_script()?;
    let ast = parser.into_ast();
    let mut variables = Variables::new();
    let root_scope = variables.push_global_scope(false);

    let mut res = StdString::new();
    if let ListHead::Present(stmt) = resolve.stmt {
        variables::resolve_script(stmt, &ast, &mut variables, root_scope)?;
    }
    if let ListHead::Present(stmt) = resolve.stmt {
        writeln!(
            &mut res,
            "{}",
            variables.render(stmt, root_scope, &ast, &interners, source)
        )
        .unwrap();
    }

    let mut first = true;
    for v in variables.declared_vars(root_scope).iter().copied() {
        if variables.symbols[v].kind == Kind::Unresolved {
            if first {
                writeln!(res, "unresolved variables:",).unwrap();
                first = false;
            }
            let span = ast[variables.symbols[v].ast_node].span;
            let render = source.render_span(span, None).unwrap();
            writeln!(res, "{}", render.as_highlight()).unwrap();
        }
    }

    Ok(res)
}

pub fn resolve(text: &str) -> Result<StdString, StdString> {
    let source = String::from_std_str(text);
    let source = common::source::Source::new(source, Some("parse_script"));
    let res = resolve_inner(&source);
    match res {
        Ok(x) => Ok(x),
        Err(Error::Parse(e)) => Err(format!("parse error {}", e.supply_context(&source))),
        Err(Error::Compile(e)) => Err(format!("compile_error {}", e.supply_context(&source))),
    }
}
