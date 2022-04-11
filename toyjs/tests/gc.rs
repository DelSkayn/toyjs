use toyjs::{
    convert::FromJs, create_static_fn, Arguments, Context, Ctx, Error, Result, ToyJs, Value,
};

const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");

pub fn assert<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    if let Some(x) = args.get(0) {
        if x.is_falseish() {
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

#[test]
fn collect_all() {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    ctx.with(|ctx| {
        ctx.global()
            .set("assert", create_static_fn!(ctx, assert))
            .unwrap();
        ctx.eval::<(), _>(UPVALUE_SOURCE).unwrap();
    });
    toyjs.collect_full();
    toyjs.collect_full();
    ctx.with(|ctx| {
        ctx.eval::<(), _>(UPVALUE_SOURCE).unwrap();
    });
}
