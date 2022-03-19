use criterion::{criterion_group, criterion_main, Criterion};
use toyjs::Context;

const FIBBO_SOURCE: &str = include_str!("./fibbo.js");

fn scripts(c: &mut Criterion) {
    let ctx = Context::new();
    ctx.with(|ctx| {
        let func = ctx.compile(FIBBO_SOURCE).unwrap();
        c.bench_function("fibbo", |b| {
            b.iter(|| {
                let v = func.call().unwrap();
                assert!(v.into_i32().unwrap() == 10946);
            })
        });
    });
}

criterion_group!(benches, scripts);
criterion_main!(benches);
