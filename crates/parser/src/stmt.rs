use super::*;
use token::t;

use bumpalo::boxed::Box;

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
            t!("return") => self.parse_return(),
            t!(";") => Ok(ast::Stmt::Empty),
            t!("{") => self.parse_block(),
            t!("function") => self.parse_function(),
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
        let if_stmt = self.parse_stmt()?;
        let if_stmt = Box::new_in(if_stmt, self.bump);
        let else_stmt = if self.eat(t!("else"))? {
            let else_stmt = self.parse_stmt()?;
            Some(Box::new_in(else_stmt, self.bump))
        } else {
            None
        };
        Ok(ast::Stmt::If(expr, if_stmt, else_stmt))
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

    fn parse_function(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "function");
        expect_bind!(self, let name = "ident");
        let scope = self.variables.push_function();
        let params = self.parse_params()?;
        expect!(self, "{");
        let stmts = self.alter_state::<_, _, Result<_>>(
            |s| s.r#return = true,
            |this| {
                let mut stmts = Vec::new_in(this.bump);
                while !this.eat(t!("}"))? {
                    stmts.push(this.parse_stmt()?);
                }
                Ok(stmts)
            },
        )?;
        self.variables.push_scope();
        let var = self.variables.define_global(name);
        Ok(ast::Stmt::Function(scope, var, params, stmts))
    }

    fn parse_params(&mut self) -> Result<ast::Params<'b>> {
        expect!(self, "(");
        let mut stmt = Vec::new_in(self.bump);
        let mut rest = None;
        loop {
            let peek = if let Some(x) = self.peek_kind()? {
                x
            } else {
                break;
            };
            match peek {
                t!("...") => {
                    self.next()?;
                    expect_bind!(self, let arg = "ident");
                    let arg_var = self.variables.define_argument(arg);
                    rest = Some(ast::Rest::BindingIdent(arg_var));
                    break;
                }
                t!("ident", arg) => {
                    self.next()?;
                    let arg_var = self.variables.define_argument(arg);
                    stmt.push(arg_var);
                }
                t!(")") => {
                    break;
                }
                _ => unexpected!(self, "...", ",", "ident"),
            }
        }
        expect!(self, ")");
        Ok(ast::Params(stmt, rest))
    }

    fn parse_return(&mut self) -> Result<ast::Stmt<'b>> {
        expect!(self, "return");
        let expr = self.parse_expr().ok();
        Ok(ast::Stmt::Return(expr))
    }
}
