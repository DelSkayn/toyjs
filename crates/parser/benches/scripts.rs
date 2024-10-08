use common::string::String;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;
use toyjs_parser::{parse_script, Parser};

pub fn bench_parser(name: &str, source: &str, c: &mut Criterion) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));

    c.bench_function(name, |b| {
        b.iter(|| {
            let mut ast = ast::Ast::new();
            let lexer = Lexer::new(black_box(source.source()), &mut ast);
            let _ = black_box(Parser::parse_syntax(lexer, parse_script)).expect("parsing failed");
        })
    });
}

pub fn scripts(c: &mut Criterion) {
    bench_parser("jquery_parser", include_str!("../../../bench/jquery.js"), c);
    bench_parser(
        "jquery_minified_parser",
        include_str!("../../../bench/jquery.min.js"),
        c,
    );
    bench_parser("axios_parser", include_str!("../../../bench/axios.js"), c);
    bench_parser(
        "axios_minified_parser",
        include_str!("../../../bench/axios.min.js"),
        c,
    );
}

criterion_group!(benches, scripts);
criterion_main!(benches);
