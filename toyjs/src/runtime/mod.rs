use crate::{convert::IntoJs, create_static_fn, ffi::Arguments, value::Value, Ctx};

pub fn assert<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    let mut idx = 0;
    while let Some(x) = args.get(idx) {
        if x.is_falseish() {
            return Err(Value::undefined(ctx));
        }
        idx += 1;
    }
    Ok(Value::undefined(ctx))
}

pub fn console_log<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    let mut idx = 0;
    while let Some(x) = args.get(idx) {
        if idx != 0 {
            print!(" ");
        }
        print!("{}", ctx.coerce_string(x));
        idx += 1;
    }
    println!();
    Ok(Value::undefined(ctx))
}

pub fn console_in<'js>(ctx: Ctx<'js>, _args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(_) => Ok(ctx.create_string(buf).into()),
        Err(_) => Ok(Value::null(ctx)),
    }
}

pub fn eval<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    ctx.eval(ctx.coerce_string(args.get(0).unwrap_or(Value::undefined(ctx))))
}

pub fn parse_int<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    let num = args.get(0);
    let radix = args.get(1);
    let str = ctx.coerce_string(num.unwrap_or(Value::undefined(ctx)));
    let mut radix = if let Some(radix) = radix {
        ctx.coerce_integer(radix)
    } else {
        0
    };

    let mut strip_prefix = true;
    if radix != 0 {
        strip_prefix = radix == 16;
        if radix < 2 || radix > 36 {
            return Ok(Value::nan(ctx));
        }
    } else {
        radix = 10
    }
    let mut radix = radix as u32;

    let mut trim = str.as_ref().trim();
    let neg = trim.starts_with("-");
    if neg || trim.starts_with("+") {
        trim = &trim[1..];
    }

    if strip_prefix {
        if trim.starts_with("0x") || trim.starts_with("0X") {
            trim = &trim[2..];
            radix = 16;
        }
    }

    let mut result: i32 = 0;
    let digits = trim.as_bytes();
    for (idx, d) in digits.into_iter().copied().enumerate() {
        let x = match (d as char).to_digit(radix) {
            Some(x) => x,
            None => {
                if idx == 0 {
                    return Ok(Value::nan(ctx));
                }
                break;
            }
        };
        result = match result.checked_mul(radix as i32) {
            Some(x) => x,
            None => break,
        };
        result = match result.checked_add(x as i32) {
            Some(x) => x,
            None => break,
        };
    }

    if neg {
        return Ok((-result).into_js(ctx));
    }
    Ok(result.into_js(ctx))
}

pub fn is_nan<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<Value<'js>, Value<'js>> {
    Ok(ctx
        .coerce_number(args.get(0).unwrap_or(Value::undefined(ctx)))
        .is_nan()
        .into_js(ctx))
}

pub fn init<'js>(ctx: Ctx<'js>) {
    let global = ctx.global();
    let console = ctx.create_object();

    console.set("log", create_static_fn!(ctx, console_log));
    console.set("input", create_static_fn!(ctx, console_in));

    global.set("console", console);
    global.set("eval", create_static_fn!(ctx, eval));
    global.set("undefined", Value::undefined(ctx));
    global.set("NaN", f64::NAN);
    global.set("parseInt", create_static_fn!(ctx, parse_int));
    global.set("isNaN", create_static_fn!(ctx, is_nan));
    global.set("assert", create_static_fn!(ctx, assert));
}
