use common::{string::String, structs::Interners};
use iai::black_box;
use lexer::Lexer;
use toyjs_parser::Parser;

pub fn bench_parser(source: &str) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut interners = Interners::default();

    // Just to make sure that the parser generates the most instructions
    let lexer = Lexer::new(black_box(source.source()), &mut interners);
    let mut parser = Parser::new(lexer);
    let _ = black_box(parser.parse_script()).expect("parsing failed");
}

fn jquery() {
    bench_parser(include_str!("jquery.js"));
}

fn jquery_minified() {
    bench_parser(include_str!("jquery.min.js"));
}

fn axios() {
    bench_parser(include_str!("axios.js"));
}

fn axios_minified() {
    bench_parser(include_str!("axios.min.js"));
}

fn typescript() {
    bench_parser(include_str!("typescript.js"));
}

fn typescript_minified() {
    bench_parser(include_str!("typescript.min.js"));
}

iai::main!(
    jquery,
    jquery_minified,
    axios,
    axios_minified,
    typescript,
    typescript_minified
);
