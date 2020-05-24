use js::{lexer, parser, parser::ParseErrorKind};
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let l = lexer::Lexer::new(&line, None);
        let mut p = parser::Parser::new(l);
        match p.parse_script() {
            Ok(x) => println!("OKAY: {:#?}", x),
            Err(e) => println!("ERROR: {}", e),
        }
    }
}
