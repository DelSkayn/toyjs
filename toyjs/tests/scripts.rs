use toyjs::{Context, ToyJs};

const THROW_SOURCE: &str = include_str!("./throw.js");
const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");
const FLOW_SOURCE: &str = include_str!("./flow.js");
const CONSTRUCTOR_SOURCE: &str = include_str!("./flow.js");
const PARSE_INT_SOURCE: &str = include_str!("./parseInt.js");
const CALL_DEEP_SOURCE: &str = include_str!("./callDeep.js");
const VAR_FOR_SOURCE: &str = include_str!("./varFor.js");
const THIS_SOURCE: &str = include_str!("./this.js");

fn eval_script(s: &str) {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.eval::<(), _>(s).unwrap();
    });
}

#[test]
fn script_upvalues() {
    eval_script(UPVALUE_SOURCE);
}

#[test]
fn script_throw() {
    eval_script(THROW_SOURCE);
}

#[test]
fn script_flow() {
    eval_script(FLOW_SOURCE);
}

#[test]
fn script_constructor() {
    eval_script(CONSTRUCTOR_SOURCE);
}

#[test]
fn script_parse_int() {
    eval_script(PARSE_INT_SOURCE);
}

#[test]
fn script_call_deep() {
    eval_script(CALL_DEEP_SOURCE);
}

#[test]
fn script_var_for() {
    eval_script(VAR_FOR_SOURCE);
}

#[test]
fn script_this() {
    eval_script(THIS_SOURCE);
}
