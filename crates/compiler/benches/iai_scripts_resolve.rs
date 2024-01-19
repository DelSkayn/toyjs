use common::{string::String, structs::Interners};
use iai::black_box;
use lexer::Lexer;
use parser::Parser;
use toyjs_compiler::variables::{ScopeKind, VariablesResolver};

pub fn bench(source: &str) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script().expect("parsing failed");
    let mut ast = parser.into_ast();
    let mut variables = VariablesResolver::new(black_box(&mut ast));
    variables
        .push_scope(ScopeKind::Global { strict: res.strict })
        .unwrap();
    variables.resolve_variables(res.stmt).unwrap();
    variables.pop_scope().unwrap();
    black_box(variables.build());
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
