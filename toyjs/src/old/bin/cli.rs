#![feature(allocator_api)]
#![allow(unused_imports)]

use std::alloc::Global;

use std::{
    env,
    io::{self, Write},
};

use ast::SymbolTable;
use common::{atom::Atoms, source::Source};
use lexer::Lexer;
use toyjs::ToyJs;

fn main() -> io::Result<()> {
    let toyjs = ToyJs::new();
    let realm = toyjs::Realm::new(&toyjs);

    if let Some(x) = env::args().nth(1) {
        let source = std::fs::read_to_string(x)?;

        realm.with(
            |ctx| match ctx.eval(source).and_then(|v| ctx.to_string(v)) {
                Ok(x) => println!("\x1b[1m{}\x1b[0m", x.as_str()),
                Err(e) => {
                    println!("\x1b[1;31m{}\x1b[0m", e);
                }
            },
        );

        return Ok(());
    }

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut delims = Vec::new();
    let mut last_length = 0;
    'main: loop {
        if delims.is_empty() {
            print!("> ");
        } else {
            print!("... ");
        }
        io::stdout().flush()?;
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

        realm.with(
            |ctx| match ctx.eval(&buffer).and_then(|v| ctx.to_string(v)) {
                Ok(x) => println!("\x1b[1m{}\x1b[0m", x.as_str()),
                Err(e) => {
                    println!("\x1b[1;31m{}\x1b[0m", e);
                }
            },
        );
        buffer.clear();
    }
    Ok(())
}
