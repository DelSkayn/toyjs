#![allow(unused)]
use common::{interner::Interner, source::Source};
use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use toyjs_lexer::Lexer;

pub fn benchmark(c: &mut Criterion) {
    const SCRIPT: &str = include_str!("./test.js");

    c.bench_function("lex_large_script", |b| {
        b.iter_batched(
            || {
                let source = Source::from_string(SCRIPT.to_string());
                let interner = Interner::new();
                (source, interner)
            },
            |x| {
                let (source, mut interner) = x;
                let mut lexer = Lexer::new(black_box(&source), &mut interner);
                while let Some(x) = lexer
                    .next()
                    .map_err(|e| {
                        use std::io::{stdout, Write};
                        source.format_span_block(stdout(), e.origin, None);
                        stdout().flush().unwrap();
                        e
                    })
                    .unwrap()
                {
                    black_box(x);
                }
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
