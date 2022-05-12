#![feature(allocator_api)]
use std::alloc::Global;

use ast::SymbolTable;
use common::{
    atom::{Atoms, Interner},
    source::Source,
};
use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use toyjs_parser::Parser;

pub fn benchmark(c: &mut Criterion) {
    const EXPRESSION: &str = include_str!("./expression.js");

    c.bench_function("parse_expression", |b| {
        b.iter_batched(
            || {
                let source = Source::from_string(EXPRESSION.to_string());
                let atoms = Atoms::new();
                (source, atoms)
            },
            |x| {
                let (source, atoms) = x;
                let mut interner = Interner::new(&atoms);
                let lexer = lexer::Lexer::new(&source, &mut interner);
                let mut symbol_table = SymbolTable::new();
                black_box(Parser::parse_script(lexer, &mut symbol_table, Global).unwrap());
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
