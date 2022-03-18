use criterion::{black_box, criterion_group, criterion_main, Criterion};
use toyjs::{Context, Value};

fn startup(c: &mut Criterion) {
    c.bench_function("startup", |b| {
        b.iter(|| {
            let ctx = Context::new();
            let v = black_box(ctx.with(|ctx| ctx.eval("null").unwrap().is_null()));
            assert!(v);
        })
    });
}

fn config() -> Criterion {
    Criterion::default()
}

criterion_group! {
    name = benches;
    config = config();
    targets = startup
}
criterion_main!(benches);
