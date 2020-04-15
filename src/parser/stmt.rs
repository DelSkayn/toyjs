use super::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        trace_log!("statement");

        if self.breakable {
            if eat!(self, "break") {
                if !self.is_lt() {
                    if is!(self, "ident") {
                        return Ok(Stmt::Break {
                            label: Some(self.next().unwrap()),
                        });
                    }
                }
                return Ok(Stmt::Break { label: None });
            }
        } else {
            if is!(self, "break") {
                unexpected!(self)
            }
        }

        if eat!(self, "do") {
            let old = self.breakable;
            self.breakable = true;
            let body = Box::new(self.parse_stmt()?);
            self.breakable = old;
            expect!(self, "while");
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            return Ok(Stmt::DoWhile { body, expr });
        }
        if eat!(self, "while") {
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            let old = self.breakable;
            self.breakable = true;
            let body = Box::new(self.parse_stmt()?);
            self.breakable = old;
            return Ok(Stmt::DoWhile { body, expr });
        }
        if is!(self, "for") {
            return self.parse_for_stmt();
        }
        if eat!(self, "return") {
            let expr = if is_lt!(self) || is!(self, ";") {
                Some(self.parse_expr()?)
            } else {
                None
            };
            return Ok(Stmt::Return { expr });
        }
        if is!(self, "{") {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }
        if is!(self, "var", "let", "const") {
            return Ok(Stmt::Declaration {
                kind: self.parse_decl()?,
            });
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
            let expr = self.parse_expr()?;
            return Ok(Stmt::Throw { expr });
        }
        if eat!(self, "if") {
            expect!(self, "(");
            let expr = self.parse_expr()?;
            expect!(self, ")");
            let body = Box::new(self.parse_stmt()?);
            let else_body = if eat!(self, "else") {
                Some(Box::new(self.parse_stmt()?))
            } else {
                None
            };
            return Ok(Stmt::If {
                expr,
                body,
                else_body,
            });
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
            return Ok(Stmt::Try {
                block,
                catch,
                finally,
            });
        }
        Ok(Stmt::Expr {
            expr: self.parse_expr()?,
        })
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

    pub fn parse_params(&mut self) -> PResult<'a, Parameters<'a>> {
        trace_log!("parameters");
        expect!(self, "(");
        if eat!(self, ")") {
            return Ok(Parameters {
                params: Vec::new(),
                rest: None,
            });
        }
        let mut params = Vec::new();
        let mut rest = None;
        loop {
            if eat!(self, "...") {
                rest = Some(self.parse_binding()?);
            }
            let binding = self.parse_binding()?;
            let initializer = if eat!(self, "=") {
                Some(self.parse_assignment_expr()?)
            } else {
                None
            };
            params.push((binding, initializer));
            if !eat!(self, ",") {
                break;
            }
        }
        expect!(self, ")");
        Ok(Parameters { params, rest })
    }

    pub fn parse_switch_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        expect!(self, "switch");
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        expect!(self, "{");
        let mut clauses = Vec::new();
        let mut def = None;
        loop {
            if eat!(self, "case") {
                let expr = self.parse_expr()?;
                expect!(self, ":");
                let mut stmts = Vec::new();

                let old = self.breakable;
                self.breakable = true;
                while !is!(self, "case", "default", "}") {
                    stmts.push(self.parse_stmt()?);
                }
                self.breakable = old;

                clauses.push(Clause { expr, stmts });
                continue;
            }
            if eat!(self, "default") {
                expect!(self, ":");
                let mut statements = Vec::new();

                let old = self.breakable;
                self.breakable = true;
                while !is!(self, "case", "default", "}") {
                    statements.push(self.parse_stmt()?);
                }
                self.breakable = old;

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

    pub fn parse_for_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        trace_log!("for statement");
        expect!(self, "for");
        expect!(self, "(");
        let start = if is!(self, "var", "let", "const") {
            Some(ForDecl::Decl(self.parse_decl()?))
        } else if is!(self, ";") {
            None
        } else {
            let expr = self.parse_expr()?;
            if expr.exprs.len() == 1 {
                match expr.exprs.into_iter().next().unwrap() {
                    AssignExpr::Lhs { expr } => Some(ForDecl::LhsExpr(expr)),
                    x => Some(ForDecl::Expr(Expr { exprs: vec![x] })),
                }
            } else {
                Some(ForDecl::Expr(self.parse_expr()?))
            }
        };

        let iter_kind = if is!(self, "in") {
            if let Some(ForDecl::Expr(_)) = start {
                unexpected!(self, ";" => "general expressions are only allowed in C like for loops");
            }
            self.next();
            ForKind::In {
                expr: self.parse_expr()?,
            }
        } else if is!(self, "of") {
            if let Some(ForDecl::Expr(_)) = start {
                unexpected!(self, ";" => "general expressions are only allowed in C like for loops");
            }
            self.next();
            ForKind::Of {
                expr: self.parse_assignment_expr()?,
            }
        } else if eat!(self, ";") {
            let cmp = if !is!(self, ";") {
                Some(self.parse_expr()?)
            } else {
                None
            };
            expect!(self, ";");
            let incr = if !is!(self, ")") {
                Some(self.parse_expr()?)
            } else {
                None
            };
            ForKind::CLike { cmp, incr }
        } else {
            unexpected!(self, "in", "of", ";")
        };
        expect!(self, ")");
        let old = self.breakable;
        self.breakable = true;
        let stmt = Box::new(self.parse_stmt()?);
        self.breakable = old;
        Ok(Stmt::For {
            start,
            kind: iter_kind,
            stmt,
        })
    }
}
