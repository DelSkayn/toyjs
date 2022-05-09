use vm::object::PropertyFlag;

use crate::{convert::IntoJs, create_static_fn, ffi::Arguments, value::Value, Ctx, Result};

pub fn console_log<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    let mut idx = 0;
    while let Some(x) = args.get(idx) {
        if idx != 0 {
            print!(" ");
        }
        print!("{}", ctx.to_string(x)?);
        idx += 1;
    }
    println!();
    Ok(Value::undefined(ctx))
}

pub fn console_in<'js>(ctx: Ctx<'js>, _args: Arguments<'js>) -> Result<'js, Value<'js>> {
    let mut buf = String::new();
    match std::io::stdin().read_line(&mut buf) {
        Ok(_) => Ok(ctx.create_string(buf).into()),
        Err(_) => Ok(Value::null(ctx)),
    }
}

pub fn eval<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    if let Some(x) = args.get(0) {
        return ctx.eval(ctx.to_string(x)?.into_string());
    }
    return Ok(Value::undefined(ctx));
}

pub fn parse_int<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    let num = args.get(0);
    let radix = args.get(1);
    let str = ctx.to_string(num.unwrap_or_else(|| Value::undefined(ctx)))?;
    let mut radix = if let Some(radix) = radix {
        ctx.to_int32(radix)?
    } else {
        0
    };

    let mut strip_prefix = true;
    if radix == 0 {
        radix = 10;
    } else {
        strip_prefix = radix == 16;
        if !(2..36).contains(&radix) {
            return Ok(Value::nan(ctx));
        }
    }
    let mut radix = radix as u32;

    let mut trim = str.as_str().trim();
    let neg = trim.starts_with('-');
    if neg || trim.starts_with('+') {
        trim = &trim[1..];
    }

    if strip_prefix && (trim.starts_with("0x") || trim.starts_with("0X")) {
        trim = &trim[2..];
        radix = 16;
    }

    let mut result: i32 = 0;
    let digits = trim.as_bytes();
    for (idx, d) in digits.iter().copied().enumerate() {
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
        return (-result).into_js(ctx);
    }
    result.into_js(ctx)
}

pub fn is_nan<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    ctx.to_number(args.get(0).unwrap_or_else(|| Value::undefined(ctx)))?
        .into_float()
        .map_or(false, |x| x.is_nan())
        .into_js(ctx)
}

pub fn is_finite<'js>(ctx: Ctx<'js>, args: Arguments<'js>) -> Result<'js, Value<'js>> {
    let num = ctx.to_number(args.get(0).unwrap_or_else(|| Value::undefined(ctx)))?;
    num.into_float().map_or(true, f64::is_finite).into_js(ctx)
}

pub fn init(ctx: Ctx) {
    let global = ctx.global();
    let console = ctx.create_object();

    console
        .set("log", create_static_fn!(ctx, console_log))
        .unwrap();
    console
        .set("input", create_static_fn!(ctx, console_in))
        .unwrap();

    global.set("console", console).unwrap();
    global.set("eval", create_static_fn!(ctx, eval)).unwrap();
    global
        .raw_set_flags("undefined", Value::undefined(ctx), PropertyFlag::empty())
        .unwrap();
    global
        .raw_set_flags("NaN", f64::NAN, PropertyFlag::empty())
        .unwrap();
    global
        .raw_set_flags("Infinity", f64::INFINITY, PropertyFlag::empty())
        .unwrap();
    global
        .set("parseInt", create_static_fn!(ctx, parse_int))
        .unwrap();
    global.set("isNaN", create_static_fn!(ctx, is_nan)).unwrap();
    global
        .set("isFinite", create_static_fn!(ctx, is_finite))
        .unwrap();
}
