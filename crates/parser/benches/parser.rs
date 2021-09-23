#![allow(unused)]
use ast::SymbolTable;
use bumpalo::Bump;
use common::{interner::Interner, source::Source};
use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use toyjs_parser::Parser;

pub fn benchmark(c: &mut Criterion) {
    const EXPRESSION: &str = include_str!("./expression.js");

    let mut bump = Bump::new();
    c.bench_function("parse_expression", |b| {
        b.iter_batched(
            || {
                let source = Source::from_string(EXPRESSION.to_string());
                let interner = Interner::new();
                (source, interner)
            },
            |x| {
                bump.reset();
                let (source, mut interner) = x;
                let lexer = lexer::Lexer::new(&source, &mut interner);
                let mut symbol_table = SymbolTable::new_in(&bump);
                black_box(Parser::parse_script(lexer, &mut symbol_table, &bump).unwrap());
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
