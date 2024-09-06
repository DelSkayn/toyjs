use common::string::String;
use iai::black_box;
use lexer::Lexer;
use parser::{parse_script, Parser};
use toyjs_compiler::variables::{self, Variables};

pub fn bench(source: &str) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut ast = ast::Ast::new();
    let lexer = Lexer::new(source.source(), &mut ast);
    let res = Parser::parse_syntax(lexer, parse_script).unwrap();
    let mut variables = Variables::new();
    let root = variables.push_global_scope(false);
    if let Some(stmt) = res.stmt {
        variables::resolve_script(stmt, &ast, &mut variables, root).unwrap();
    }
    black_box(variables);
}

fn jquery_resolve() {
    bench(include_str!("../../../bench/jquery.js"));
}

fn jquery_minified_resolve() {
    bench(include_str!("../../../bench/jquery.min.js"));
}

fn axios_resolve() {
    bench(include_str!("../../../bench/axios.js"));
}

fn axios_minified_resolve() {
    bench(include_str!("../../../bench/axios.min.js"));
}

fn typescript_resolve() {
    bench(include_str!("../../../bench/typescript.js"));
}

fn typescript_minified_resolve() {
    bench(include_str!("../../../bench/typescript.min.js"));
}

iai::main!(
    jquery_resolve,
    jquery_minified_resolve,
    axios_resolve,
    axios_minified_resolve,
    typescript_resolve,
    typescript_minified_resolve
);
