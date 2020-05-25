use js::{compiler, lexer, parser, parser::ParseErrorKind, runtime};
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let l = lexer::Lexer::new(&line, None);
        let mut p = parser::Parser::new(l);
        let x = match p.parse_script() {
            Ok(x) => {
                println!("PARSE OKAY: {:#?}", x);
                x
            }
            Err(e) => {
                println!("PARSE ERROR: {}", e);
                continue;
            }
        };
        let mut compiler = compiler::Compiler::new();
        let x = match compiler.compile_script(x) {
            Ok(x) => {
                println!("COMPILE OK: \n{}", x);
                x
            }
            Err(e) => {
                println!("COMPILE ERROR: {:?}", e);
                continue;
            }
        };
        unsafe {
            let mut runtime = runtime::Runtime::new(&x.0);
            let res = runtime.run_unsafe();
            if let Some(x) = res {
                if x.is_float() {
                    println!("value: {:?}", x.into_float());
                }
                if x.is_int() {
                    println!("value: {:?}", x.into_int());
                }
            }
        }
    }
}
