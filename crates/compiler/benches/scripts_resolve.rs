use ast::ListHead;
use common::{string::String, structs::Interners};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lexer::Lexer;
use parser::Parser;
use toyjs_compiler::variables::{self, Variables};

pub fn bench(name: &str, source: &str, c: &mut Criterion) {
    let source = String::from_std_str(source);
    let source = common::source::Source::new(source, Some("parse_script"));
    let mut interners = Interners::default();
    let lexer = Lexer::new(source.source(), &mut interners);
    let mut parser = Parser::new(lexer);
    let res = parser.parse_script().expect("parsing failed");
    let ast = parser.into_ast();

    c.bench_function(name, |b| {
        b.iter(|| {
            let mut variables = Variables::new();
            let root = variables.push_global_scope(false);
            if let ListHead::Present(stmt) = res.stmt {
                variables::resolve_script(stmt, black_box(&ast), &mut variables, root).unwrap();
            }
            black_box(variables);
        })
    });
}

pub fn scripts(c: &mut Criterion) {
    bench(
        "jquery_resolve",
        include_str!("../../../bench/jquery.js"),
        c,
    );
    bench(
        "jquery_minified_resolve",
        include_str!("../../../bench/jquery.min.js"),
        c,
    );
    bench("axios_resolve", include_str!("../../../bench/axios.js"), c);
    bench(
        "axios_minified_resolve",
        include_str!("../../../bench/axios.min.js"),
        c,
    );
}

criterion_group!(benches, scripts);
criterion_main!(benches);
