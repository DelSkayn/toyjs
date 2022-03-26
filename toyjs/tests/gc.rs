use toyjs::{Context, ToyJs};

const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");

#[test]
fn collect_all() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval::<(), _>(UPVALUE_SOURCE).unwrap();
    });
    toyjs.collect_full();
    toyjs.collect_full();
    ctx.with(|ctx| {
        ctx.eval::<(), _>(UPVALUE_SOURCE).unwrap();
    });
}
