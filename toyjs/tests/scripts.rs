use toyjs::{create_static_fn, Arguments, Ctx, Error, FromJs, Realm, Result, ToyJs, Value};

const THROW_SOURCE: &str = include_str!("./throw.js");
const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");
const FLOW_SOURCE: &str = include_str!("./flow.js");
const CONSTRUCTOR_SOURCE: &str = include_str!("./flow.js");
const PARSE_INT_SOURCE: &str = include_str!("./parseInt.js");
const CALL_DEEP_SOURCE: &str = include_str!("./callDeep.js");
const VAR_FOR_SOURCE: &str = include_str!("./varFor.js");
const THIS_SOURCE: &str = include_str!("./this.js");
const OBJECT_SOURCE: &str = include_str!("./object.js");
const RESOLVE_SYMBOLS_SOURCE: &str = include_str!("./resolve_symbols.js");
const BINDING_SOURCE: &str = include_str!("./binding.js");

pub fn assert<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    if let Some(x) = args.get(0) {
        if x.is_falsish() {
            if let Some(x) = args
                .get(1)
                .and_then(|x| toyjs::String::from_js(ctx, x).ok())
            {
                return Err(Error::Type(x.to_string()));
            } else {
                return Err(Error::Type("Assertion failed".to_string()));
            }
        }
    }
    Ok(Value::undefined(ctx))
}

fn eval_script(s: &str) {
    let toyjs = ToyJs::new();
    let ctx = Realm::new(&toyjs);
    ctx.with(|ctx| {
        ctx.global()
            .set("assert", create_static_fn!(ctx, assert))
            .unwrap();
        match ctx.eval::<_, ()>(s) {
            Ok(_) => {}
            Err(e) => panic!("error: {}", e),
        }
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
#[should_panic]
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

#[test]
fn script_object() {
    eval_script(OBJECT_SOURCE);
}

#[test]
fn script_resolve_symbols() {
    eval_script(RESOLVE_SYMBOLS_SOURCE);
}

#[test]
fn binding_symbols() {
    eval_script(BINDING_SOURCE);
}
