use toyjs::Context;

const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");

#[test]
fn collect_all() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(UPVALUE_SOURCE).unwrap();
    });
    ctx.collect_all();
    ctx.with(|ctx| {
        ctx.eval(UPVALUE_SOURCE).unwrap();
    });
}
