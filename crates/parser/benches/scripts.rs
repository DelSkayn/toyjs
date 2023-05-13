use common::string::String;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;
use toyjs_parser::Parser;

pub fn bench_parser(name: &str, source: &str, c: &mut Criterion) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));

    c.bench_function(name, |b| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(source.source()));
            let mut parser = Parser::new(lexer);
            let _ = black_box(parser.parse_script()).expect("parsing failed");
        })
    });
}

pub fn scripts(c: &mut Criterion) {
    bench_parser("jquery", include_str!("jquery.js"), c);
    bench_parser("jquery_minified", include_str!("jquery.min.js"), c);
    bench_parser("axios", include_str!("axios.js"), c);
    bench_parser("axios_minified", include_str!("axios.min.js"), c);
}

criterion_group!(benches, scripts);
criterion_main!(benches);
