use super::*;
use token::t;

use bumpalo::{boxed::Box, collections::Vec};

impl<'a, 'b> Parser<'a, 'b> {
    pub(crate) fn parse_stmt(&mut self) -> Result<ast::Stmt<'b>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => return Ok(ast::Stmt::Empty),
        };
        let res = match peek {
            t!("if") => self.parse_if(),
            t!("while") => self.parse_while(),
            t!("do") => self.parse_do_while(),
            t!("let") => self.parse_let_binding(),
            t!("var") => self.parse_var_binding(),
            t!("const") => self.parse_const_binding(),
            t!(";") => Ok(ast::Stmt::Empty),
            t!("{") => self.parse_block(),
            _ => Ok(ast::Stmt::Expr(self.parse_expr()?)),
        };
        self.eat(t!(";"))?;
        res
    }

    fn parse_if(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "if");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let stmt = self.parse_stmt()?;
        Ok(ast::Stmt::If(expr, Box::new_in(stmt, self.bump)))
    }

    fn parse_while(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let stmt = self.parse_stmt()?;
        Ok(ast::Stmt::While(expr, Box::new_in(stmt, self.bump)))
    }

    fn parse_do_while(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "do");
        let stmt = self.parse_stmt()?;
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        Ok(ast::Stmt::DoWhile(Box::new_in(stmt, self.bump), expr))
    }

    fn parse_let_binding(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "let");
        expect_bind!(self, let id = "ident");
        let expr = if self.eat(t!("="))? {
            Some(self.parse_single_expr()?)
        } else {
            None
        };
        let var = self.variables.define_local(id, false);
        Ok(ast::Stmt::Let(var, expr))
    }

    fn parse_var_binding(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "var");
        expect_bind!(self, let id = "ident");
        let expr = if self.eat(t!("="))? {
            Some(self.parse_single_expr()?)
        } else {
            None
        };
        let var = self.variables.define_global(id);
        Ok(ast::Stmt::Var(var, expr))
    }

    fn parse_const_binding(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "var");
        expect_bind!(self, let id = "ident");
        expect!(self, "=" => "constant needs to be initialized");
        let expr = self.parse_single_expr()?;
        let var = self.variables.define_local(id, true);
        Ok(ast::Stmt::Const(var, expr))
    }

    fn parse_block(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "{");
        let scope = self.variables.push_scope();
        let mut stmts = Vec::new_in(self.bump);
        while !self.eat(t!("}"))? {
            stmts.push(self.parse_stmt()?)
        }
        self.variables.pop();
        Ok(ast::Stmt::Block(scope, stmts))
    }
}
