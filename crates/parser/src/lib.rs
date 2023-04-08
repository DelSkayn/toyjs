use ast::{Ast, Root};
use common::span::Span;
use lexer::Lexer;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ast: Ast<Root>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ast: Ast::new(Root::Undetermined, Span::empty()),
        }
    }
}
