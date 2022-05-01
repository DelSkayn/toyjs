#![feature(allocator_api)]
#![allow(unused_imports)]

use std::alloc::Global;

use std::{
    env,
    io::{self, Write},
};

use ast::SymbolTable;
use common::interner;
use common::source::Source;
use lexer::Lexer;
use vm::atom::Atoms;
use vm::Realm;

fn main() -> io::Result<()> {
    vm::new_cell_owner!(owner);
    let root = vm::gc::Roots::new();
    let mut arena = vm::gc::Arena::new(&root);
    let atoms = Atoms::new();
    let mut interner = interner::Interner::new();
    let realm = Realm::new(&mut owner, &arena, &atoms);
    let realm = arena.add(realm);
    vm::root!(arena, realm);

    if let Some(x) = env::args().nth(1) {
        let source = std::fs::read_to_string(x)?;
        let source = Source::from_string(source);

        let mut symbol_table = SymbolTable::new();

        let lexer = lexer::Lexer::new(&source, &mut interner);
        let script =
            parser::Parser::parse_script(lexer, &mut symbol_table, std::alloc::Global).unwrap();
        let bc = compiler::Compiler::compile_script(
            &script,
            &symbol_table,
            &mut interner,
            &atoms,
            &arena,
            Global,
        );
        let bc = arena.add(bc);
        vm::root!(arena, bc);

        unsafe {
            dbg!(realm.eval(&mut arena, &mut owner, &atoms, bc)).ok();
        }

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

        let source = Source::from_string(buffer.clone());

        let mut symbol_table = SymbolTable::new();

        let lexer = lexer::Lexer::new(&source, &mut interner);
        let script =
            parser::Parser::parse_script(lexer, &mut symbol_table, std::alloc::Global).unwrap();
        let bc = compiler::Compiler::compile_script(
            &script,
            &symbol_table,
            &mut interner,
            &atoms,
            &arena,
            Global,
        );
        let bc = arena.add(bc);
        vm::root!(arena, bc);

        arena.collect(&owner);

        unsafe {
            dbg!(realm.eval(&mut arena, &mut owner, &atoms, bc)).ok();
        }
        buffer.clear();
    }
    Ok(())
}
