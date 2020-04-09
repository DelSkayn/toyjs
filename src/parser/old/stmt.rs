use super::{PResult, Parser};
use crate::{
    ast::*,
    lexer::Lexer,
    token::{Kw, TokenKind},
};
use std::collections::VecDeque;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        if self.is(tok!("let")) || self.is(tok!("const")) || self.is(tok!("var")) {
            return Ok(Stmt::Declaration {
                kind: self.parse_lexical_decl()?,
            });
        }
        // try
        if self.eat(tok!("try")) {
            return self.parse_try_stmt();
        }

        // {
        if self.is(tok!("{")) {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }

        if self.is(tok!("throw")) {
            return self.parse_throw_stmt();
        }

        // ,
        if self.eat(tok!(";")) {
            return Ok(Stmt::Empty);
        }
        to_do!(self)
        //self.parse_expr()
    }

    pub fn parse_try_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        let block = Box::new(self.parse_block_stmt()?);
        let catch = if self.eat(tok!("catch")) {
            let binding = self.parse_binding()?;
            let block = self.parse_block_stmt()?;
            Some(Box::new(Catch {
                param: binding,
                block,
            }))
        } else {
            None
        };
        let finally = if self.eat(tok!("finally")) {
            Some(Box::new(self.parse_block_stmt()?))
        } else {
            None
        };
        Ok(Stmt::Try {
            block,
            catch,
            finally,
        })
    }

    pub fn parse_throw_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        assert!(self.eat(tok!("throw")));
        Ok(Stmt::Throw {
            expr: self.parse_expr()?,
        })
    }

    pub fn parse_block_stmt(&mut self) -> PResult<'a, Block<'a>> {
        assert!(self.eat(tok!("{")));
        let mut stmts = Vec::new();
        while !self.eat(tok!("}")) {
            stmts.push(self.parse_stmt()?);
            eat!(self, ";");
        }
        Ok(Block { stmts })
    }

    pub fn parse_lexical_decl(&mut self) -> PResult<'a, DeclKind<'a>> {
        let mut kind = LexicalKind::Let;
        if self.eat(tok!("const")) {
            kind = LexicalKind::Const;
        } else if self.eat(tok!("var")) {
            kind = LexicalKind::Var;
        } else {
            assert!(self.eat(tok!("let")))
        }
        let mut bindings = Vec::new();
        loop {
            bindings.push(self.parse_binding()?);
            if !self.eat(tok!(",")) {
                break;
            }
        }
        Ok(DeclKind::Lexical { kind, bindings })
    }

    pub fn parse_binding(&mut self) -> PResult<'a, Binding<'a>> {
        if self.eat(tok!("{")) {
            return self.parse_object_binding();
        }
        if self.eat(tok!("[")) {
            return self.parse_array_binding();
        }
        if self.is_ident() {
            let ident = self.next().unwrap();
            let initializer = if self.eat(tok!("=")) {
                Some(self.parse_assigment_expr()?)
            } else {
                None
            };
            return Ok(Binding::Ident { ident, initializer });
        }
        to_do!(self);
    }

    pub fn parse_object_binding(&mut self) -> PResult<'a, Binding<'a>> {
        to_do!(self);
    }
    pub fn parse_array_binding(&mut self) -> PResult<'a, Binding<'a>> {
        to_do!(self);
    }
}
