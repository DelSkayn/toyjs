use criterion::{black_box, criterion_group, criterion_main, Criterion};
use toyjs::{Realm, ToyJs};

fn startup(c: &mut Criterion) {
    c.bench_function("startup", |b| {
        b.iter(|| {
            let toyjs = ToyJs::new();
            let ctx = Realm::new(&toyjs);
            black_box(ctx.with(|ctx| ctx.eval::<_, ()>("null").unwrap()));
        })
    });
}

criterion_group!(benches, startup);
criterion_main!(benches);
