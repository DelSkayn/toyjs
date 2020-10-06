use std::io::{self, BufRead};
use toyjs::ToyJs;

fn main() -> io::Result<()> {
    let mut js = ToyJs::new();

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut delims = Vec::new();
    let mut last_length = 0;
    'main: loop {
        buffer.clear();
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
        println!("value: {:?}", js.exec(&buffer));
    }
    Ok(())
}
