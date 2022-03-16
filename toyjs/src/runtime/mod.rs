use crate::{ffi::Arguments, value::Value, Ctx};

pub fn console_log<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Value<'js> {
    if let Some(arg) = args.get(0) {
        print!("{}", ctx.coerce_string(arg));
    }
    println!();
    Value::undefined(ctx)
}

pub fn console_in<'js>(ctx: Ctx<'js>, _args: Arguments<'js>) -> Value<'js> {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(_) => ctx.create_string(buf).into(),
        Err(_) => Value::null(ctx),
    }
}

pub fn eval<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Value<'js> {
    if let Some(arg) = args.get(0) {
        ctx.eval(ctx.coerce_string(arg))
            .unwrap_or(Value::undefined(ctx))
    } else {
        Value::undefined(ctx)
    }
}

pub fn init<'js>(ctx: Ctx<'js>) {
    let global = ctx.global();
    let console = ctx.create_object();
    console.set("log", ctx.create_function(console_log));
    console.set("input", ctx.create_function(console_in));
    global.set("console", console);
    global.set("eval", ctx.create_function(eval));
}
