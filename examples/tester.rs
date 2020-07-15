use js::{interner::Interner, lexer::Lexer};
use std::io::{self, BufRead};

fn main() {
    let mut interner = Interner::with_capacity(2048);
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let mut lexer = Lexer::new(line.as_bytes(), &mut interner);
        loop {
            match lexer.next() {
                Ok(None) => break,
                Ok(Some(x)) => println!("{:?}", x),
                Err(e) => println!("error: {:?}", e),
            }
        }
    }
}
/*
use js::{
    compiler, lexer, parser,
    parser::ParseErrorKind,
    runtime,
    source::{Source, Sourced},
};
use std::{
    io::{self, BufRead},
    mem,
};

fn main() {
    println!("{}", mem::size_of::<js::token::Token>());
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let s = Source::new(line, None);
        let l = lexer::Lexer::new(&s);
        let mut p = parser::Parser::new(l);
        let x = match p.parse_script() {
            Ok(x) => {
                println!("PARSE OKAY: {:#?}", x);
                x
            }
            Err(e) => {
                println!("PARSE ERROR: {}", s.wrap(e));
                continue;
            }
        };
        let compiler = compiler::Compiler::new();
        let x = match compiler.compile_script(&x) {
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
            let mut runtime = runtime::Runtime::new(&x);
            let res = runtime.run_unsafe();
            if let Some(x) = res {
                if x.is_float() {
                    println!("value: {:?}", x.into_float());
                }
                if x.is_int() {
                    println!("value: {:?}", x.into_int());
                }
                if x.is_bool() {
                    if x.into_bool() {
                        println!("value: true")
                    } else {
                        println!("value: false")
                    }
                }
                if x.is_null() {
                    println!("value: null")
                }
                if x.is_string() {
                    let val = x.into_string();
                    println!("value: \"{}\"", val.value());
                    val.drop();
                }
            }
        }
    }
}*/
