use toyjs::Context;

const THROW_SOURCE: &str = include_str!("./throw.js");
const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");
const FLOW_SOURCE: &str = include_str!("./flow.js");
const CONSTRUCTOR_SOURCE: &str = include_str!("./flow.js");
const PARSE_INT_SOURCE: &str = include_str!("./parseInt.js");
const CALL_DEEP_SOURCE: &str = include_str!("./callDeep.js");

#[test]
fn script_upvalues() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(UPVALUE_SOURCE).unwrap();
    });
}

#[test]
fn script_throw() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(THROW_SOURCE).unwrap();
    });
}

#[test]
fn script_flow() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(FLOW_SOURCE).unwrap();
    });
}

#[test]
fn script_constructor() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(CONSTRUCTOR_SOURCE).unwrap();
    });
}

#[test]
fn script_parse_int() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(PARSE_INT_SOURCE).unwrap();
    });
}

#[test]
fn script_call_deep() {
    let ctx = Context::new();
    ctx.with(|ctx| {
        ctx.eval(CALL_DEEP_SOURCE).unwrap();
    });
}
