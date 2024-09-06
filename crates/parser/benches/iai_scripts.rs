use common::string::String;
use iai::black_box;
use lexer::Lexer;
use toyjs_parser::{parse_script, Parser};

pub fn bench_parser(source: &str) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut ast = ast::Ast::new();

    // Just to make sure that the parser generates the most instructions
    let lexer = Lexer::new(black_box(source.source()), &mut ast);
    black_box(Parser::parse_syntax(lexer, parse_script)).expect("parsing failed");
}

fn jquery_parser() {
    bench_parser(include_str!("../../../bench/jquery.js"));
}

fn jquery_minified_parser() {
    bench_parser(include_str!("../../../bench/jquery.min.js"));
}

fn axios_parser() {
    bench_parser(include_str!("../../../bench/axios.js"));
}

fn axios_minified_parser() {
    bench_parser(include_str!("../../../bench/axios.min.js"));
}

fn typescript_parser() {
    bench_parser(include_str!("../../../bench/typescript.js"));
}

fn typescript_minified_parser() {
    bench_parser(include_str!("../../../bench/typescript.min.js"));
}

iai::main!(
    jquery_parser,
    jquery_minified_parser,
    axios_parser,
    axios_minified_parser,
    typescript_parser,
    typescript_minified_parser
);
