use std::cell::Cell;

use toyjs::{create_static_fn, Arguments, Ctx, Realm, Result, ToyJs, Value};

macro_rules! stack_ptr{
    () => ({
        let x: usize;
        unsafe{
            std::arch::asm!("mov {}, rsp", out(reg) x);
        }
        x
    })
}

thread_local! {static STACK_END: Cell<usize> = Cell::new(0)}

fn measure_stack<'js>(ctx: Ctx<'js>, _: Arguments<'js>) -> Result<'js, Value<'js>> {
    let ptr = stack_ptr!();

    STACK_END.with(|x| {
        x.set(ptr);
    });

    Ok(Value::undefined(ctx))
}

fn main() {
    let toyjs = ToyJs::new();
    let realm = Realm::new(&toyjs);

    let stack_start = stack_ptr!();
    realm.with(|ctx| {
        ctx.global()
            .set("measure_stack", create_static_fn!(ctx, measure_stack))
            .unwrap();
        ctx.eval::<_, ()>("measure_stack()").unwrap()
    });
    let used_full = stack_start - STACK_END.with(|x| x.get());

    let per_function = realm.with(|ctx| {
        ctx.eval::<_, ()>("measure_stack()").unwrap();
        let single = STACK_END.with(|x| x.get());
        ctx.eval::<_, ()>("(() => measure_stack() )()").unwrap();
        let double = STACK_END.with(|x| x.get());
        single - double
    });

    println!(
        "= stack usage\n\ttotal stack:\t{}\n\tsingle frame:\t{}\n\tother stack:\t{}",
        used_full,
        per_function,
        used_full - per_function
    );
}
