use toyjs::{create_static_fn, Arguments, Ctx, Error, FromJs, Realm, Result, ToyJs, Value};

const UPVALUE_SOURCE: &str = include_str!("./upvalue.js");

pub fn assert<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    println!("a");
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
    println!("b");
    Ok(Value::undefined(ctx))
}

pub fn dbg<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    let a = args.get(0).unwrap_or_else(|| Value::undefined(ctx));
    println!("dbg: {:?}", a);
    Ok(a)
}

#[test]
fn collect_all() {
    let toyjs = ToyJs::new();
    let ctx = Realm::new(&toyjs);
    println!("1");
    ctx.with(|ctx| {
        ctx.global()
            .set("assert", create_static_fn!(ctx, assert))
            .unwrap();
        ctx.global()
            .set("dbg", create_static_fn!(ctx, dbg))
            .unwrap();
        ctx.eval::<_, ()>(UPVALUE_SOURCE).unwrap();
    });
    println!("2");
    toyjs.collect_full();
    println!("3");
    toyjs.collect_full();
    println!("4");
    ctx.with(|ctx| {
        ctx.eval::<_, ()>(UPVALUE_SOURCE).unwrap();
    });
}
