use super::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        trace_log!("statement");

        if let Some(x) = self.parse_break_stmt()? {
            return Ok(x);
        }
        if let Some(x) = self.parse_do_while_stmt()? {
            return Ok(x);
        }
        if let Some(x) = self.parse_while_stmt()? {
            return Ok(x);
        }
        if is!(self, "for") {
            return self.parse_for_stmt().map(Stmt::For);
        }
        if let Some(x) = self.parse_return_stmt()? {
            return Ok(x);
        }
        if is!(self, "{") {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }
        if is!(self, "var", "let", "const") {
            let decl = self.alter_state(|x| x._in = true, |this| this.parse_decl())?;
            return Ok(Stmt::Declaration {
                kind: DeclKind::Lexical(decl),
            });
        }
        if is!(self, "class") {
            return Ok(Stmt::Class(self.parse_class(true)?));
        }
        if eat!(self, ";") {
            return Ok(Stmt::Empty);
        }
        if eat!(self, "debugger") {
            eat!(self, ";");
            return Ok(Stmt::Debugger);
        }
        if eat!(self, "throw") {
            no_lt!(self);
            let expr = self.parse_expr_with_in()?;
            return Ok(Stmt::Throw { expr });
        }
        if let Some(x) = self.parse_if_stmt()? {
            return Ok(x);
        }
        if is!(self, "switch") {
            return self.parse_switch_stmt();
        }
        if eat!(self, "with") {
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            let stmt = Box::new(self.parse_stmt()?);
            return Ok(Stmt::With { expr, stmt });
        }
        if let Some(x) = self.parse_try_stmt()? {
            return Ok(x);
        }
        Ok(Stmt::Expr {
            expr: self.parse_expr()?,
        })
    }

    pub fn parse_break_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if self.state._break {
            if eat!(self, "break") {
                if !self.is_lt() {
                    if is!(self, "ident") {
                        return Ok(Some(Stmt::Break {
                            label: Some(self.next().unwrap()),
                        }));
                    }
                }
                return Ok(Some(Stmt::Break { label: None }));
            }
        } else {
            if is!(self, "break") {
                unexpected!(self)
            }
        }
        Ok(None)
    }

    pub fn parse_do_while_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if eat!(self, "do") {
            let body = Box::new(self.alter_state(
                |x| {
                    x._break = true;
                    x._continue = true;
                },
                |this| this.parse_stmt(),
            )?);
            expect!(self, "while");
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            return Ok(Some(Stmt::DoWhile { body, expr }));
        }
        Ok(None)
    }

    pub fn parse_while_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if eat!(self, "while") {
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            let body = Box::new(self.alter_state(
                |x| {
                    x._break = true;
                    x._continue = true;
                },
                |this| this.parse_stmt(),
            )?);
            return Ok(Some(Stmt::While { body, expr }));
        }
        Ok(None)
    }

    pub fn parse_return_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if is!(self, "return") {
            if self.state._return {
                self.next();
                let expr = if is_lt!(self) || is!(self, ";") {
                    None
                } else {
                    Some(self.parse_expr_with_in()?)
                };
                return Ok(Some(Stmt::Return { expr }));
            } else {
                unexpected!(self => "return not allowed in this context")
            }
        }
        Ok(None)
    }

    pub fn parse_if_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if eat!(self, "if") {
            expect!(self, "(");
            let expr = self.parse_expr_with_in()?;
            expect!(self, ")");
            let body = Box::new(self.parse_stmt()?);
            let else_body = if eat!(self, "else") {
                Some(Box::new(self.parse_stmt()?))
            } else {
                None
            };
            return Ok(Some(Stmt::If {
                expr,
                body,
                else_body,
            }));
        }
        Ok(None)
    }

    pub fn parse_try_stmt(&mut self) -> PResult<'a, Option<Stmt<'a>>> {
        if eat!(self, "try") {
            let block = self.parse_block_stmt()?;
            let catch = if eat!(self, "catch") {
                let bindings = if eat!(self, "(") {
                    let binding = self.parse_binding()?;
                    expect!(self, ")");
                    Some(binding)
                } else {
                    None
                };
                let block = self.parse_block_stmt()?;
                Some(Catch {
                    param: bindings,
                    block,
                })
            } else {
                None
            };
            let finally = if eat!(self, "finally") {
                Some(self.parse_block_stmt()?)
            } else {
                None
            };
            return Ok(Some(Stmt::Try {
                block,
                catch,
                finally,
            }));
        }
        Ok(None)
    }

    pub fn parse_block_stmt(&mut self) -> PResult<'a, Block<'a>> {
        trace_log!("block statement");
        expect!(self, "{");
        let mut stmts = Vec::new();
        while !eat!(self, "}") {
            stmts.push(self.parse_stmt()?);
            eat!(self, ";");
        }
        Ok(Block { stmts })
    }

    pub fn parse_switch_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        expect!(self, "switch");
        expect!(self, "(");
        let expr = self.parse_expr_with_in()?;
        expect!(self, ")");
        expect!(self, "{");
        let mut clauses = Vec::new();
        let mut def = None;
        loop {
            if eat!(self, "case") {
                let expr = self.parse_expr_with_in()?;
                expect!(self, ":");
                let mut stmts = Vec::new();
                self.alter_state(
                    |x| x._break = true,
                    |this| {
                        while !is!(this, "case", "default", "}") {
                            stmts.push(this.parse_stmt()?);
                        }
                        Ok(())
                    },
                )?;
                clauses.push(Clause { expr, stmts });
                continue;
            }
            if eat!(self, "default") {
                expect!(self, ":");
                let mut statements = Vec::new();
                self.alter_state(
                    |x| x._break = true,
                    |this| {
                        while !is!(this, "case", "default", "}") {
                            statements.push(this.parse_stmt()?);
                        }
                        Ok(())
                    },
                )?;
                def = Some(statements);
            }
            if eat!(self, "}") {
                break;
            }
            unexpected!(self, "case", "default", "}")
        }
        return Ok(Stmt::Switch {
            expr,
            clauses,
            default: def,
        });
    }

    pub fn parse_for_stmt(&mut self) -> PResult<'a, For<'a>> {
        trace_log!("for statement");
        expect!(self, "for");
        expect!(self, "(");

        let kind = if eat!(self, ";") {
            let condition = if !is!(self, ";") {
                Some(self.parse_expr_with_in()?)
            } else {
                None
            };
            expect!(self, ";");
            let iteration = if !is!(self, ")") {
                Some(self.parse_expr_with_in()?)
            } else {
                None
            };
            ForKind::Empty {
                condition,
                iteration,
            }
        } else if is!(self, "var", "let", "const") {
            let decl = self.alter_state(|x| x._in = false, |this| this.parse_decl())?;
            if eat!(self, "in") {
                let expr = self.parse_expr()?;
                ForKind::DeclIn { decl, expr }
            } else if eat!(self, "of") {
                let expr = self.parse_assignment_expr()?;
                ForKind::DeclOf { decl, expr }
            } else if eat!(self, ";") {
                let condition = if !is!(self, ";") {
                    Some(self.parse_expr_with_in()?)
                } else {
                    None
                };
                expect!(self, ";");
                let iteration = if !is!(self, ")") {
                    Some(self.parse_expr_with_in()?)
                } else {
                    None
                };
                ForKind::DeclCLike {
                    decl,
                    condition,
                    iteration,
                }
            } else {
                unexpected!(self, ";", "of", "in");
            }
        } else {
            let expr = self.alter_state(|x| x._in = false, |this| this.parse_expr())?;
            if eat!(self, ";") {
                let condition = if !is!(self, ";") {
                    Some(self.parse_expr_with_in()?)
                } else {
                    None
                };
                expect!(self, ";");
                let iteration = if !is!(self, ")") {
                    Some(self.parse_expr_with_in()?)
                } else {
                    None
                };
                ForKind::ExprCLike {
                    expr,
                    condition,
                    iteration,
                }
            } else {
                if expr.exprs.len() != 1 {
                    unexpected!(self,"expression" => "'in' and 'of' loops require an expression");
                }
                let lhs = expr.exprs.into_iter().next().unwrap();
                if !lhs.is_assign_lhs() {
                    unexpected!(self,"left hand side expression" => "'in' and 'of' loops require a single left hand side expression")
                };
                if eat!(self, "in") {
                    let expr = self.parse_expr_with_in()?;
                    ForKind::ExprIn { lhs, expr }
                } else if eat!(self, "of") {
                    let expr = self.parse_expr_with_in()?;
                    ForKind::ExprOf { lhs, expr }
                } else {
                    unexpected!(self, ";", "in", "of");
                }
            }
        };
        expect!(self, ")");
        let stmt = Box::new(self.alter_state(
            |x| {
                x._break = true;
                x._continue = true;
            },
            |this| this.parse_stmt(),
        )?);
        Ok(For { kind, stmt })
    }
}
