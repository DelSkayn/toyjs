use common::string::String;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;
use std::string::String as StdString;
use toyjs_parser::Parser;

pub fn get_scource(url: &str) -> StdString {
    attohttpc::get(url)
        .send()
        .expect("failed to retrieve source")
        .text()
        .expect("failed to read source")
}

pub fn bench_parser(name: &str, source: &str, c: &mut Criterion) {
    let source = get_scource(source);
    let source = String::from_std_str(&source);
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
    bench_parser("jquery", "https://code.jquery.com/jquery-3.6.4.js", c);
    bench_parser(
        "axios",
        "https://cdn.jsdelivr.net/npm/axios@1.1.2/dist/axios.js",
        c,
    );
    bench_parser(
        "axios_minified",
        "https://cdn.jsdelivr.net/npm/axios/dist/axios.min.js",
        c,
    );
    bench_parser(
        "jquery_minified",
        "https://code.jquery.com/jquery-3.6.4.min.js",
        c,
    );
}

criterion_group!(benches, scripts);
criterion_main!(benches);
