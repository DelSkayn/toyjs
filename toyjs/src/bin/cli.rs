//use std::io::{self, BufRead};
use std::io;
//use toyjs::{Context, ToyJs};

fn main() -> io::Result<()> {
    /*
    let js = ToyJs::new();
    let ctx = Context::new(&js);
    //js.dump_bc(true);
    //js.dump_ssa(true);
    //js.dump_allocated_bytes(true);

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
        ctx.with(|ctx| {
            println!("value: {:?}", ctx.exec(buffer.clone(), true));
        });
        buffer.clear();
    }
    */
    Ok(())
}
