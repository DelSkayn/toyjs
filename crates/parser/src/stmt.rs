use super::*;
use ast::symbol_table::{DeclType, ScopeKind};
use token::t;

impl<'a, A: Allocator + Clone> Parser<'a, A> {
    pub(crate) fn parse_stmt(&mut self) -> Result<ast::Stmt<A>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => return Ok(ast::Stmt::Empty),
        };
        let res = match peek {
            t!("if") => self.parse_if(),
            t!("while") => self.parse_while(),
            t!("do") => self.parse_do_while(),
            t!("for") => self.parse_for(),
            t!("let") => self.parse_let_binding(),
            t!("var") => self.parse_var_binding(),
            t!("const") => self.parse_const_binding(),
            t!("return") => self.parse_return(),
            t!("throw") => self.parse_throw(),
            t!(";") => {
                self.next()?;
                Ok(ast::Stmt::Empty)
            }
            t!("{") => self.parse_block(),
            t!("function") => self.parse_function(),
            t!("try") => self.parse_try(),
            _ => Ok(ast::Stmt::Expr(self.parse_expr()?)),
        };
        self.eat(t!(";"))?;
        res
    }

    fn parse_if(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "if");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let if_stmt = self.parse_stmt()?;
        let if_stmt = Box::new_in(if_stmt, self.alloc.clone());
        let else_stmt = if self.eat(t!("else"))? {
            let else_stmt = self.parse_stmt()?;
            Some(Box::new_in(else_stmt, self.alloc.clone()))
        } else {
            None
        };
        Ok(ast::Stmt::If(expr, if_stmt, else_stmt))
    }

    fn parse_while(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let stmt = self.parse_stmt()?;
        Ok(ast::Stmt::While(
            expr,
            Box::new_in(stmt, self.alloc.clone()),
        ))
    }

    fn parse_do_while(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "do");
        let stmt = self.parse_stmt()?;
        expect!(self, "while");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        Ok(ast::Stmt::DoWhile(
            Box::new_in(stmt, self.alloc.clone()),
            expr,
        ))
    }

    pub(crate) fn parse_for(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "for");
        expect!(self, "(");
        let _scope = self.symbol_table.push_scope(ScopeKind::Lexical);
        let decl = match self.peek_kind()? {
            None => unexpected!(self, "let", "var", "const", "ident"),
            Some(t!("let")) => {
                ast::ForDecl::Stmt(Box::new_in(self.parse_let_binding()?, self.alloc.clone()))
            }
            Some(t!("const")) => {
                ast::ForDecl::Stmt(Box::new_in(self.parse_const_binding()?, self.alloc.clone()))
            }
            _ => ast::ForDecl::Expr(self.parse_single_expr()?),
        };
        expect!(self, ";");
        let cond = self.parse_single_expr()?;
        expect!(self, ";");
        let post = self.parse_single_expr()?;
        expect!(self, ")");

        let stmt = self.parse_stmt()?;
        self.symbol_table.pop_scope();

        Ok(ast::Stmt::For(
            decl,
            cond,
            post,
            Box::new_in(stmt, self.alloc.clone()),
        ))
    }

    fn parse_let_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "let");
        expect_bind!(self, let id = "ident");
        let expr = if self.eat(t!("="))? {
            Some(self.parse_single_expr()?)
        } else {
            None
        };
        let var = self.symbol_table.define(id, DeclType::Let).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        Ok(ast::Stmt::Let(var, expr))
    }

    fn parse_var_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "var");
        expect_bind!(self, let id = "ident");
        let expr = if self.eat(t!("="))? {
            Some(self.parse_single_expr()?)
        } else {
            None
        };
        let var = self.symbol_table.define(id, DeclType::Var).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        Ok(ast::Stmt::Var(var, expr))
    }

    fn parse_const_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "const");
        expect_bind!(self, let id = "ident");
        expect!(self, "=" => "constant needs to be initialized");
        let expr = self.parse_single_expr()?;
        let var = self.symbol_table.define(id, DeclType::Const).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        Ok(ast::Stmt::Const(var, expr))
    }

    fn parse_block(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "{");
        let scope = self.symbol_table.push_scope(ScopeKind::Lexical);
        let mut stmts = Vec::new_in(self.alloc.clone());
        while !self.eat(t!("}"))? {
            stmts.push(self.parse_stmt()?)
        }
        self.symbol_table.pop_scope();
        Ok(ast::Stmt::Block(scope, stmts))
    }

    fn parse_function(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "function");
        expect_bind!(self, let name = "ident");
        let var = self.symbol_table.define(name, DeclType::Var).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        let scope = self.symbol_table.push_scope(ScopeKind::Function);
        let params = self.parse_params()?;
        expect!(self, "{");
        let stmts = self.alter_state::<_, _, Result<_>>(
            |s| s.r#return = true,
            |this| {
                let mut stmts = Vec::new_in(this.alloc.clone());
                while !this.eat(t!("}"))? {
                    stmts.push(this.parse_stmt()?);
                }
                Ok(stmts)
            },
        )?;
        self.symbol_table.pop_scope();
        Ok(ast::Stmt::Function(scope, var, params, stmts))
    }

    pub(crate) fn parse_params(&mut self) -> Result<ast::Params<A>> {
        expect!(self, "(");
        let mut stmt = Vec::new_in(self.alloc.clone());
        let mut rest = None;
        while let Some(peek) = self.peek_kind()? {
            match peek {
                t!("...") => {
                    self.next()?;
                    expect_bind!(self, let arg = "ident");
                    let arg_var =
                        self.symbol_table
                            .define(arg, DeclType::Argument)
                            .ok_or(Error {
                                kind: ErrorKind::RedeclaredVariable,
                                origin: self.last_span,
                            })?;
                    rest = Some(ast::Rest::BindingIdent(arg_var));
                    break;
                }
                t!("ident", arg) => {
                    self.next()?;
                    let arg_var =
                        self.symbol_table
                            .define(arg, DeclType::Argument)
                            .ok_or(Error {
                                kind: ErrorKind::RedeclaredVariable,
                                origin: self.last_span,
                            })?;
                    stmt.push(arg_var);
                    if self.eat(t!(","))? {
                        continue;
                    } else {
                        break;
                    }
                }
                t!(")") => {
                    break;
                }
                _ => unexpected!(self, "...", ")", "ident"),
            }
        }
        expect!(self, ")");
        Ok(ast::Params(stmt, rest))
    }

    fn parse_return(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "return");
        let expr = self.parse_expr().ok();
        Ok(ast::Stmt::Return(expr))
    }

    fn parse_throw(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "throw");
        let expr = self.parse_single_expr()?;
        Ok(ast::Stmt::Throw(expr))
    }

    fn parse_try(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "try");
        let r#try = Box::new_in(self.parse_block()?, self.alloc.clone());
        let catch = if self.peek_kind()? == Some(t!("catch")) {
            self.next()?;
            let scope = self.symbol_table.push_scope(ScopeKind::Lexical);
            let binding = if self.peek_kind()? == Some(t!("(")) {
                self.next()?;
                expect_bind!(self, let binding = "ident");
                expect!(self, ")");
                self.symbol_table.define(binding, DeclType::Let)
            } else {
                None
            };
            expect!(self, "{");
            let mut stmts = Vec::new_in(self.alloc.clone());
            while !self.eat(t!("}"))? {
                stmts.push(self.parse_stmt()?)
            }
            self.symbol_table.pop_scope();
            let stmt = Box::new_in(ast::Stmt::Block(scope, stmts), self.alloc.clone());
            Some(ast::Catch { binding, stmt })
        } else {
            None
        };
        let finally = if self.peek_kind()? == Some(t!("finally")) {
            self.next()?;
            Some(Box::new_in(self.parse_block()?, self.alloc.clone()))
        } else {
            None
        };

        Ok(ast::Stmt::Try(r#try, catch, finally))
    }
}
