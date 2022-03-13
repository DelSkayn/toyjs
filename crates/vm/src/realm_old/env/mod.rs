use std::convert::TryFrom;

use crate::{gc::BoundGc, value::BoundValue};

use super::{Arguments, RealmCtx};

pub fn console_log<'a>(ctx: RealmCtx<'a>, args: Arguments<'a>) -> BoundValue<'a> {
    let mut first = true;
    for arg in args.iter() {
        if !first {
            print!(" ");
        }
        first = false;
        print!("{}", ctx.coerce_string(arg));
    }
    println!();
    BoundValue::undefined()
}

pub fn console_error<'a>(ctx: RealmCtx<'a>, args: Arguments<'a>) -> BoundValue<'a> {
    let mut first = true;
    for arg in args.iter() {
        if !first {
            eprint!(" ");
        }
        first = false;
        eprint!("{}", ctx.coerce_string(arg));
    }
    eprintln!();
    BoundValue::undefined()
}

pub unsafe fn init<'a>(ctx: RealmCtx<'a>) {
    let global = ctx.global();
    ctx.frame(|ctx| {
        let console = ctx.create_object();
        let log = ctx.create_function(console_log);
        let error = ctx.create_function(console_error);
        console.index_set(ctx.create_string("log"), log, ctx);
        console.index_set(ctx.create_string("error"), error, ctx);
        global.index_set(ctx.create_string("console"), console, ctx);
    })
}
