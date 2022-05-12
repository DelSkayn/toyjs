use super::{Allocator, Error, ErrorKind, Parser, Result};
use ast::symbol_table::{DeclType, ScopeKind};
use token::t;

impl<'a, 'b, A: Allocator + Clone> Parser<'a, 'b, A> {
    pub(crate) fn parse_stmt(&mut self) -> Result<ast::Stmt<A>> {
        let peek = match self.peek_kind()? {
            Some(x) => x,
            None => return Ok(ast::Stmt::Empty),
        };
        let res = match peek {
            t!("if") => self.parse_if(),
            t!("while") => self.parse_while(),
            t!("switch") => self.parse_switch(),
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
            t!("break") => {
                if !self.state.r#break {
                    unexpected!(self => "`break` is disallowed in this scope.");
                }
                self.next()?;
                Ok(ast::Stmt::Break)
            }
            t!("continue") => {
                if !self.state.r#continue {
                    unexpected!(self => "`continue` is disallowed in this scope.");
                }
                self.next()?;
                Ok(ast::Stmt::Continue)
            }
            t!("{") => self.parse_block(),
            t!("function") => self.parse_function(),
            t!("try") => self.parse_try(),
            _ => {
                let expr = self.parse_expr()?;
                match self.peek_lt()?.map(|x| x.kind) {
                    Some(t!(";")) | Some(t!("\n")) | Some(t!("}")) | None => {}
                    _ => {
                        unexpected!(self, "\n", ";" => "expected expression to end");
                    }
                }
                Ok(ast::Stmt::Expr(expr))
            }
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
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Parser::parse_stmt,
        )?;
        Ok(ast::Stmt::While(
            expr,
            Box::new_in(stmt, self.alloc.clone()),
        ))
    }

    fn parse_switch(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "switch");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        expect!(self, "{");
        let mut clauses = Vec::new_in(self.alloc.clone());
        let mut r#default = None;
        self.alter_state(
            |x| {
                x.r#break = true;
            },
            |this| loop {
                if this.eat(t!("case"))? {
                    let expr = this.parse_single_expr()?;
                    expect!(this, ":");
                    let mut stmts = Vec::new_in(this.alloc.clone());
                    loop {
                        let peek = this.peek_kind()?;
                        match peek {
                            Some(t!("case")) | Some(t!("default")) | Some(t!("}")) => break,
                            _ => {
                                stmts.push(this.parse_stmt()?);
                            }
                        }
                    }
                    clauses.push(ast::Case { expr, stmts });
                } else if this.eat(t!("default"))? {
                    expect!(this, ":");
                    let mut peek = this.peek_kind()?;
                    let mut stmts = Vec::new_in(this.alloc.clone());
                    while peek.is_some() && peek.unwrap() != t!("}") {
                        stmts.push(this.parse_stmt()?);
                        peek = this.peek_kind()?;
                    }
                    r#default = Some(stmts);
                    return Ok(());
                } else {
                    return Ok(());
                }
            },
        )?;
        if r#default.is_some() && self.peek_kind()? == Some(t!("case")) {
            unexpected!(self => "`default` must be the last case");
        }
        expect!(self, "}" => "switch block not closed");
        Ok(ast::Stmt::Switch(expr, clauses, r#default))
    }

    fn parse_do_while(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "do");
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;
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
        let decl = if self.eat(t!(";"))? {
            None
        } else {
            let decl = match self.peek_kind()? {
                None => unexpected!(self, "let", "var", "const", "ident"),
                Some(t!("let")) => {
                    ast::ForDecl::Stmt(Box::new_in(self.parse_let_binding()?, self.alloc.clone()))
                }
                Some(t!("const")) => {
                    ast::ForDecl::Stmt(Box::new_in(self.parse_const_binding()?, self.alloc.clone()))
                }
                Some(t!("var")) => {
                    ast::ForDecl::Stmt(Box::new_in(self.parse_var_binding()?, self.alloc.clone()))
                }
                _ => ast::ForDecl::Expr(self.parse_single_expr()?),
            };
            if self.peek_kind()? == Some(t!("in")) {
                return self.parse_for_in_of(false, decl);
            }
            if self.peek_kind()? == Some(t!("of")) {
                return self.parse_for_in_of(true, decl);
            }
            expect!(self, ";");
            Some(decl)
        };
        let cond = if self.eat(t!(";"))? {
            None
        } else {
            let res = self.parse_expr()?;
            expect!(self, ";");
            Some(res)
        };
        let post = if self.eat(t!(")"))? {
            None
        } else {
            let res = self.parse_expr()?;
            expect!(self, ")");
            Some(res)
        };
        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;
        self.symbol_table.pop_scope();

        Ok(ast::Stmt::For(ast::For::CStyle(
            decl,
            cond,
            post,
            Box::new_in(stmt, self.alloc.clone()),
        )))
    }

    fn parse_for_in_of(&mut self, is_of: bool, decl: ast::ForDecl<A>) -> Result<ast::Stmt<A>> {
        let binding = match decl {
            ast::ForDecl::Stmt(stmt) => match *stmt {
                ast::Stmt::Let(binding, None) => binding,
                ast::Stmt::Var(x) if x.len() == 1 => {
                    if x[0].1.is_some() {
                        unexpected!(self => "a for-in/of loop declaration can't have an initializer")
                    }
                    x[0].0
                }
                ast::Stmt::Let(_, Some(_)) => {
                    unexpected!(self => "a for-in/of loop declaration can't have an initializer")
                }
                _ => unexpected!(self => "invalid for-in/of left-hand side"),
            },
            ast::ForDecl::Expr(ast::Expr::Prime(ast::PrimeExpr::Variable(x))) => x,
            _ => unexpected!(self => "invalid for-in/of left-hand side"),
        };

        if is_of {
            expect!(self, "of");
        } else {
            expect!(self, "in");
        }

        let e = self.parse_expr()?;

        expect!(self, ")");

        let stmt = self.alter_state(
            |x| {
                x.r#continue = true;
                x.r#break = true;
            },
            Self::parse_stmt,
        )?;

        if is_of {
            Ok(ast::Stmt::For(ast::For::ForOf(
                binding,
                e,
                Box::new_in(stmt, self.alloc.clone()),
            )))
        } else {
            Ok(ast::Stmt::For(ast::For::ForIn(
                binding,
                e,
                Box::new_in(stmt, self.alloc.clone()),
            )))
        }
    }

    fn parse_let_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "let");
        expect_bind!(self, let id = "ident");
        let var = self.symbol_table.define(id, DeclType::Let).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        let expr = if self.eat(t!("="))? {
            Some(self.parse_single_expr()?)
        } else {
            None
        };
        Ok(ast::Stmt::Let(var, expr))
    }

    fn parse_var_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "var");
        let mut bindings = Vec::new_in(self.alloc.clone());
        loop {
            expect_bind!(self, let id = "ident");
            let var = self.symbol_table.define(id, DeclType::Var).ok_or(Error {
                kind: ErrorKind::RedeclaredVariable,
                origin: self.last_span,
            })?;
            let expr = if self.eat(t!("="))? {
                Some(self.parse_single_expr()?)
            } else {
                None
            };
            bindings.push((var, expr));
            if !self.eat(t!(","))? {
                break;
            }
        }
        Ok(ast::Stmt::Var(bindings))
    }

    fn parse_const_binding(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "const");
        expect_bind!(self, let id = "ident");
        expect!(self, "=" => "constant needs to be initialized");
        let var = self.symbol_table.define(id, DeclType::Const).ok_or(Error {
            kind: ErrorKind::RedeclaredVariable,
            origin: self.last_span,
        })?;
        let expr = self.parse_single_expr()?;
        Ok(ast::Stmt::Const(var, expr))
    }

    fn parse_block(&mut self) -> Result<ast::Stmt<A>> {
        expect!(self, "{");
        let scope = self.symbol_table.push_scope(ScopeKind::Lexical);
        let mut stmts = Vec::new_in(self.alloc.clone());
        while !self.eat(t!("}"))? {
            stmts.push(self.parse_stmt()?);
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
        let params = self.parse_params(false)?;
        expect!(self, "{");
        let stmts = self.alter_state::<_, _, Result<_>>(
            |s| {
                s.r#return = true;
                s.r#continue = false;
                s.r#break = false;
            },
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

    pub(crate) fn parse_params(&mut self, openbrace_eaten: bool) -> Result<ast::Params<A>> {
        if !openbrace_eaten {
            expect!(self, "(");
        }
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
                    if !self.eat(t!(","))? {
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
                stmts.push(self.parse_stmt()?);
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
