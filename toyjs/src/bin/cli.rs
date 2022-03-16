use std::{
    env,
    io::{self, BufRead},
};

use toyjs::Context;

fn main() -> io::Result<()> {
    let ctx = Context::new();
    if let Some(x) = env::args().skip(1).next() {
        let source = std::fs::read_to_string(x)?;
        ctx.with(|ctx| println!("value: {:?}", ctx.eval(source).unwrap()));
        return Ok(());
    }

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut delims = Vec::new();
    let mut last_length = 0;
    'main: loop {
        if handle.read_line(&mut buffer)? == 0 {
            break;
        }
        for c in buffer[last_length..].chars() {
            match c {
                '{' => delims.push('{'),
                '(' => delims.push('('),
                '[' => delims.push('['),
                '}' => {
                    if let Some(x) = delims.pop() {
                        if x == '{' {
                            continue;
                        }
                    }
                    println!("mismatched deliminator");
                    delims.clear();
                    buffer.clear();
                    continue 'main;
                }
                ']' => {
                    if let Some(x) = delims.pop() {
                        if x == '[' {
                            continue;
                        }
                    }
                    println!("mismatched deliminator");
                    delims.clear();
                    buffer.clear();
                    continue 'main;
                }
                ')' => {
                    if let Some(x) = delims.pop() {
                        if x == '(' {
                            continue;
                        }
                    }
                    println!("mismatched deliminator");
                    delims.clear();
                    buffer.clear();
                    continue 'main;
                }
                _ => {}
            }
        }
        if delims.len() != 0 {
            last_length = buffer.len();
            continue 'main;
        }
        last_length = 0;
        ctx.with(|ctx| println!("value: {:?}", ctx.eval(&buffer).unwrap()));
        buffer.clear();
    }
    Ok(())
}
