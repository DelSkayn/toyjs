use toyjs::{Context, ToyJs};

const THROW_SOURCE: &str = include_str!("./throw.js");
const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");
const FLOW_SOURCE: &str = include_str!("./flow.js");
const CONSTRUCTOR_SOURCE: &str = include_str!("./flow.js");
const PARSE_INT_SOURCE: &str = include_str!("./parseInt.js");
const CALL_DEEP_SOURCE: &str = include_str!("./callDeep.js");

#[test]
fn script_upvalues() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(UPVALUE_SOURCE).unwrap();
    });
}

#[test]
fn script_throw() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(THROW_SOURCE).unwrap();
    });
}

#[test]
fn script_flow() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(FLOW_SOURCE).unwrap();
    });
}

#[test]
fn script_constructor() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(CONSTRUCTOR_SOURCE).unwrap();
    });
}

#[test]
fn script_parse_int() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(PARSE_INT_SOURCE).unwrap();
    });
}

#[test]
fn script_call_deep() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval(CALL_DEEP_SOURCE).unwrap();
    });
}
