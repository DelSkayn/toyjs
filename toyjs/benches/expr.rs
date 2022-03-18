use criterion::{black_box, criterion_group, criterion_main, Criterion};
use toyjs::{convert::FromJs, Context};

fn expression(c: &mut Criterion) {
    const EXPRESSION: &str = "((2 + 2) ** 3 / 100 - 5 ** 3 * -1000) ** 2 + 100 - 8;";

    let ctx = Context::new();
    c.bench_function("eval_expression", move |b| {
        ctx.with(|ctx| {
            b.iter(|| {
                let val = black_box(ctx.eval(EXPRESSION));
                let val = val.map(|x| f64::from_js(ctx, x));

                assert!(val.unwrap() == 15625160092.4096);
            })
        })
    });
}

fn config() -> Criterion {
    Criterion::default().sample_size(500)
}

criterion_group! {
    name = benches;
    config = config();
    targets = expression
}
criterion_main!(benches);
