use criterion::{black_box, criterion_group, criterion_main, Criterion};
use toyjs::Context;

fn startup(c: &mut Criterion) {
    c.bench_function("startup", |b| {
        b.iter(|| {
            let ctx = Context::new();
            let v = black_box(ctx.with(|ctx| ctx.eval("null").unwrap().is_null()));
            assert!(v);
        })
    });
}

criterion_group!(benches, startup);
criterion_main!(benches);
