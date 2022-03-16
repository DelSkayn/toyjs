#![feature(allocator_api)]

use std::{
    env,
    fs::File,
    io::{self, Read},
};

use toyjs::Context;

fn get_input() -> Result<Box<dyn Read>, io::Error> {
    if let Some(x) = env::args().skip(1).next() {
        return Ok(Box::new(File::open(x)?) as Box<dyn Read>);
    } else {
        Ok(Box::new(io::stdin()) as Box<dyn Read>)
    }
}

fn main() -> Result<(), io::Error> {
    let mut read = get_input()?;
    let mut buffer = String::new();
    read.read_to_string(&mut buffer)?;
    let context = Context::new();
    context.with(|ctx| {
        println!("{:?}", ctx.eval(buffer).unwrap());
    });
    Ok(())
}
