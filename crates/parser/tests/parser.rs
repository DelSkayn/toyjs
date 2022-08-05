#![feature(allocator_api)]
use std::alloc::Global;

use ast::SymbolTable;
use common::{interner::Interner, source::Source};
use lexer::Lexer;
use toyjs_parser::Parser;

fn parse(string: &str) {
    let source = Source::from_string(string.to_string());
    let mut interner = Interner::new();
    let lexer = Lexer::new(&source, &mut interner);
    let mut variables = SymbolTable::new_in(Global);
    Parser::parse_script(lexer, &mut variables, Global).unwrap();
}

#[test]
fn basic_expression() {
    parse("1 + 1")
}
