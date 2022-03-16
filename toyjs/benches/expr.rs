use criterion::{criterion_group, criterion_main, Criterion};
use toyjs::ToyJs;

/*
fn expression(c: &mut Criterion) {
    const EXPRESSION: &str = "((2 + 2) ** 3 / 100 - 5 ** 3 * -1000) ** 2 + 100 - 8;";
    let mut toyjs = ToyJs::new();

    let toyjs_ref = &mut toyjs;
    c.bench_function("arithmentic_operations_full", move |b| {
        b.iter(|| toyjs_ref.exec(EXPRESSION).unwrap());
    });
    let bc = toyjs.compile(EXPRESSION).unwrap();
    let toyjs_ref = &mut toyjs;
    let bc_ref = &bc;
    c.bench_function("arithmentic_operations_exec", move |b| {
        b.iter(|| toyjs_ref.exec_bytecode(bc_ref).unwrap());
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
*/
