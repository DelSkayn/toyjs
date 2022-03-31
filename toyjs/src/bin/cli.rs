use std::{env, io};

use toyjs::{Context, ToyJs, Value};

fn main() -> io::Result<()> {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    if let Some(x) = env::args().nth(1) {
        let source = std::fs::read_to_string(x)?;
        ctx.with(|ctx| match ctx.eval::<Value, _>(source) {
            Ok(x) => println!("value: {:?}", x),
            Err(e) => println!("\x1b[1:31m{}\x1b[0m", e),
        });

        return Ok(());
    }

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut delims = Vec::new();
    let mut last_length = 0;
    'main: loop {
        if stdin.read_line(&mut buffer)? == 0 {
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
        if !delims.is_empty() {
            last_length = buffer.len();
            continue 'main;
        }
        last_length = 0;
        ctx.with(|ctx| match ctx.eval::<Value, _>(&buffer) {
            Ok(x) => println!("> {:?}", x),
            Err(e) => {
                println!("\x1b[1;31m{}\x1b[0m", e);
            }
        });
        buffer.clear();
    }
    Ok(())
}
