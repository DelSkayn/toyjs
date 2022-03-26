use std::{env, io};

use toyjs::{Context, ToyJs, Value};

fn main() -> io::Result<()> {
    let toyjs = ToyJs::new();
    let ctx = Context::new(&toyjs);
    if let Some(x) = env::args().nth(1) {
        let source = std::fs::read_to_string(x)?;
        ctx.with(|ctx| println!("value: {:?}", ctx.eval(source).unwrap()));
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
        ctx.with(|ctx| match ctx.eval(&buffer) {
            Ok(x) => println!("> {:?}", x),
            Err(e) => {
                if let Some(e) = e.into_object() {
                    if e.is_error() {
                        let msg: Value = e.get("message");
                        let name: Value = e.get("name");

                        if let (Some(name), Some(msg)) = (name.into_string(), msg.into_string()) {
                            println!("uncaught {}: {}", name, msg);
                            return;
                        }
                    }
                }
                println!("uncaught error: {:?}", e)
            }
        });
        buffer.clear();
    }
    Ok(())
}
