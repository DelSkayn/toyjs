use js::{
    compiler::Compiler, interner::Interner, lexer::Lexer, parser::Parser, runtime, source::Source,
};
use std::io::{self, Result, Write};

fn main() -> Result<()> {
    env_logger::init();
    let mut interner = Interner::with_capacity(2048);
    let stdin = io::stdin();

    let mut buffer = String::new();
    let mut pending_parrens = Vec::new();
    loop {
        let mut line = String::new();
        stdin.read_line(&mut line)?;
        for b in line.as_bytes() {
            match *b {
                x @ b'{' | x @ b'(' | x @ b'[' => pending_parrens.push(x),
                b'}' => {
                    if pending_parrens.pop() != Some(b'{') {
                        println!("mismatched paren!");
                        buffer.clear();
                        continue;
                    }
                }
                b')' => {
                    if pending_parrens.pop() != Some(b'(') {
                        println!("mismatched paren!");
                        buffer.clear();
                        continue;
                    }
                }
                b']' => {
                    if pending_parrens.pop() != Some(b'[') {
                        println!("mismatched paren!");
                        buffer.clear();
                        continue;
                    }
                }
                _ => {}
            }
        }
        buffer.push_str(&line);
        if pending_parrens.len() != 0 {
            print!("... ");
            io::stdout().flush()?;
            continue;
        }
        println!("script {}", buffer);

        let lexer = Lexer::new(buffer.as_bytes(), &mut interner);
        let mut parser = Parser::new(lexer);
        match parser.parse_script() {
            Ok(_) => {}
            Err(e) => {
                let s = Source::new(buffer.clone(), None);
                println!("PARSE ERROR: {}", s.wrap(e));
                continue;
            }
        };
        let ssa = parser.builder.build();
        println!("PARSE_OKAY:\n{:#?}", ssa);
        let bc = Compiler::compile(&ssa, &interner);
        println!("instructions: \n{}", bc);
        unsafe {
            let mut runtime = runtime::Runtime::new(&bc);
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
                if x.is_undefined() {
                    println!("value: undefined")
                }
                if x.is_string() {
                    let val = x.into_string();
                    println!("value: \"{}\"", val.value());
                    val.drop();
                }
            }
        }
        buffer.clear();
    }
}
