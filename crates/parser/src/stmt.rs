use ast::{
    CaseItem, CatchStmt, CstyleDecl, ForLoopHead, IdentOrPattern, InOfDecl, List, ListHead, ListId,
    NodeId, Stmt, VariableDecl, VariableKind,
};
use token::t;

use crate::{
    alter_state, error::ErrorKind, expect, function::FunctionKind, peek_expect, unexpected, Parser,
    Result,
};

impl<'a> Parser<'a> {
    /// Parses ECMA spec `Declaration`, `Statement` and other `...Statement` productions
    pub fn parse_stmt(&mut self) -> Result<NodeId<Stmt>> {
        let Some(token) = self.peek() else {
            return Ok(self.ast.push_node(Stmt::Empty));
        };
        let expr = match token.kind() {
            t!("{") => {
                let list = self.parse_block_stmt()?;
                self.ast.push_node(Stmt::Block { list })
            }
            t!("var") => self.parse_variable_decl(VariableKind::Var)?,
            t!("let") => self.parse_variable_decl(VariableKind::Let)?,
            t!("const") => self.parse_variable_decl(VariableKind::Const)?,
            t!("if") => self.parse_if_stmt()?,
            t!("while") => self.parse_while_stmt()?,
            t!("do") => self.parse_do_while_stmt()?,
            t!("for") => self.parse_for_stmt()?,
            t!("switch") => self.parse_switch_stmt()?,
            t!("return") => self.parse_return_stmt()?,
            t!("break") => self.parse_cntrl_flow_stmt(true)?,
            t!("continue") => self.parse_cntrl_flow_stmt(false)?,
            t!("try") => self.parse_try_stmt()?,
            t!("throw") => self.parse_throw_stmt()?,
            t!("function") => {
                self.next();
                let func = self.parse_function(FunctionKind::Stmt)?;
                self.ast.push_node(Stmt::Function { func })
            }
            t!("debugger") => {
                self.next();
                self.semicolon()?;
                self.ast.push_node(Stmt::Debugger)
            }
            t!("with") => self.parse_with_stmt()?,
            t!(";") => {
                self.next();
                self.ast.push_node(Stmt::Empty)
            }
            _ => {
                alter_state!(self,r#in = true => {
                    let expr = self.parse_expr()?;
                });
                self.semicolon()?;
                self.ast.push_node(Stmt::Expr { expr })
            }
        };

        Ok(expr)
    }

    pub fn parse_block_stmt(&mut self) -> Result<ListHead<Stmt>> {
        expect!(self, "{");
        let mut head = ListHead::Empty;
        let mut prev = None;

        loop {
            let token = peek_expect!(self,"}" => "expected statement block to close");
            if let t!("}") = token.kind() {
                self.next();
                return Ok(head);
            }
            let stmt = self.parse_stmt()?;

            let new = self.ast.append_list(stmt, prev);
            prev = Some(new);
            head = head.or(ListHead::Present(new));
        }
    }

    pub fn parse_if_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "if");
        expect!(self, "(");
        alter_state!(self,r#in = true => {
            let cond = self.parse_expr()?;
        });
        expect!(self, ")");
        let body = self.parse_stmt()?;
        let r#else = if self.eat(t!("else")) {
            Some(self.parse_stmt()?)
        } else {
            None
        };

        Ok(self.ast.push_node(Stmt::If { cond, body, r#else }))
    }

    pub fn parse_while_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "while");
        expect!(self, "(");
        alter_state!(self,r#in = true => {
        let cond = self.parse_expr()?;
        });
        expect!(self, ")");

        alter_state!(self,r#break = true, r#continue = true => {
            let body = self.parse_stmt()?;
        });

        Ok(self.ast.push_node(Stmt::While { cond, body }))
    }

    pub fn parse_do_while_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "do");

        alter_state!(self,r#break = true, r#continue = true => {
            let body = self.parse_stmt()?;
        });

        expect!(self, "while");
        expect!(self, "(");
        let cond = self.parse_expr()?;
        expect!(self, ")");
        Ok(self.ast.push_node(Stmt::DoWhile { cond, body }))
    }

    pub fn parse_c_style_decl(
        &mut self,
        decl: NodeId<IdentOrPattern>,
    ) -> Result<ListId<VariableDecl>> {
        let initializer = self
            .eat(t!("="))
            .then(|| self.parse_assignment_expr())
            .transpose()?;
        let decl = self.ast.push_node(VariableDecl { decl, initializer });

        let head = self.ast.append_list(decl, None);
        let mut prev = head;
        while self.eat(t!(",")) {
            let decl = self.parse_ident_or_pattern()?;
            let initializer = self
                .eat(t!("="))
                .then(|| self.parse_assignment_expr())
                .transpose()?;
            let decl = self.ast.push_node(VariableDecl { decl, initializer });
            prev = self.ast.append_list(decl, Some(prev));
        }
        Ok(head)
    }

    pub fn parse_c_style_for(&mut self, decl: CstyleDecl) -> Result<NodeId<Stmt>> {
        expect!(self, ";");
        let cond = if let t!(";") = peek_expect!(self, ";").kind() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        expect!(self, ";");
        let post = if let t!(")") = peek_expect!(self, ")").kind() {
            None
        } else {
            Some(self.parse_expr()?)
        };
        expect!(self, ")");
        let head = self.ast.push_node(ForLoopHead::CStyle { decl, cond, post });
        alter_state!(self, r#break = true, r#continue = true => {
            let body = self.parse_stmt()?;
        });
        Ok(self.ast.push_node(Stmt::For { head, body }))
    }

    pub fn parse_for_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "for");
        //TODO await loop
        expect!(self, "(");

        let next = peek_expect!(self);
        let decl = match next.kind() {
            t!("let") | t!("var") | t!("const") => {
                let kind = match next.kind() {
                    t!("let") => VariableKind::Let,
                    t!("var") => VariableKind::Var,
                    t!("const") => VariableKind::Const,
                    _ => unreachable!(),
                };
                self.next();
                let binding = self.parse_ident_or_pattern()?;
                if let t!("=") | t!(",") = peek_expect!(self).kind() {
                    let decl = self.parse_c_style_decl(binding)?;
                    return self.parse_c_style_for(CstyleDecl::Decl { kind, decl });
                } else {
                    InOfDecl::Decl { kind, binding }
                }
            }
            t!(";") => return self.parse_c_style_for(CstyleDecl::Empty),
            _ => {
                let r#in = self.state.r#in;
                self.state.r#in = false;
                let expr = self.parse_assignment_expr()?;

                if self.eat(t!(",")) {
                    let next = Some(self.parse_expr()?);
                    let expr = self.ast.push_list(List { item: expr, next });
                    self.state.r#in = r#in;
                    return self.parse_c_style_for(CstyleDecl::Expr(expr));
                }
                self.state.r#in = r#in;
                InOfDecl::Expr(expr)
            }
        };

        let next = peek_expect!(self);
        let head = match next.kind() {
            t!(";") => {
                let decl = match decl {
                    InOfDecl::Expr(expr) => CstyleDecl::Expr(self.ast.append_list(expr, None)),
                    InOfDecl::Decl { kind, binding } => {
                        let decl = self.ast.push_node(VariableDecl {
                            decl: binding,
                            initializer: None,
                        });
                        let decl = self.ast.append_list(decl, None);
                        CstyleDecl::Decl { kind, decl }
                    }
                };
                return self.parse_c_style_for(decl);
            }
            t!("in") => {
                self.next();
                let expr = self.parse_expr()?;
                ForLoopHead::In { decl, expr }
            }
            t!("of") => {
                self.next();
                let expr = self.parse_assignment_expr()?;
                ForLoopHead::Of { decl, expr }
            }
            //TODO 'of'
            x => unexpected!(self, x, ";", "in"),
        };
        let head = self.ast.push_node(head);

        expect!(self, ")");
        alter_state!(self, r#break = true, r#continue = true => {
            let body = self.parse_stmt()?;
        });
        Ok(self.ast.push_node(Stmt::For { head, body }))
    }

    pub fn parse_switch_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "switch");
        expect!(self, "(");
        let cond = self.parse_expr()?;
        expect!(self, ")");
        expect!(self, "{");
        let mut head = ListHead::Empty;
        let mut prev = None;
        let mut default = None;
        loop {
            let case = match peek_expect!(self, "case", "default", "}").kind() {
                t!("case") => {
                    self.next();
                    Some(self.parse_expr()?)
                }
                t!("default") => {
                    if default.is_some() {
                        unexpected!(self, t!("default") => "multiple default cases is not allowed")
                    }
                    self.next();
                    None
                }
                t!("}") => {
                    self.next();
                    return Ok(self.ast.push_node(Stmt::Switch {
                        cond,
                        cases: head,
                        default,
                    }));
                }
                x => unexpected!(self, x, "case", "default", "}"),
            };
            expect!(self, ":");

            alter_state!(self,r#break = true => {
                let mut stmt_head = ListHead::Empty;
                let mut stmt_prev = None;
                loop {
                    match peek_expect!(self).kind() {
                        t!("case") | t!("default") | t!("}") => break,
                        _ => {
                            let stmt = self.parse_stmt()?;
                            stmt_prev = Some(self.ast.append_list(stmt, stmt_prev));
                            stmt_head = stmt_head.or(stmt_prev.into())
                        }
                    }
                }
            });

            if let Some(expr) = case {
                let node = self.ast.push_node(CaseItem {
                    expr,
                    stmts: stmt_head,
                });
                prev = Some(self.ast.append_list(node, prev));
                head = head.or(prev.into())
            } else {
                // Default case
                default = Some(stmt_head)
            }
        }
    }

    pub fn parse_return_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "return");
        debug_assert!(self.peek.is_none());
        if self.eat_semicolon() {
            Ok(self.ast.push_node(Stmt::Return { expr: None }))
        } else {
            let expr = Some(self.parse_expr()?);
            self.semicolon()?;
            Ok(self.ast.push_node(Stmt::Return { expr }))
        }
    }

    pub fn parse_cntrl_flow_stmt(&mut self, is_break: bool) -> Result<NodeId<Stmt>> {
        if is_break {
            expect!(self, "break");
        } else {
            expect!(self, "continue");
        }

        if is_break && !self.state.r#break || !is_break && !self.state.r#continue {
            return Err(crate::Error::new(
                ErrorKind::DisallowedToken {
                    found: if is_break {
                        t!("break")
                    } else {
                        t!("continue")
                    },
                    message: None,
                },
                self.last_span().clone(),
            ));
        }

        let label = if self.eat_semicolon() {
            None
        } else {
            let token = peek_expect!(self);
            if let t!("ident") = token.kind() {
                Some(token.data_id().unwrap())
            } else {
                unexpected!(self, token.kind(), "ident");
            }
        };

        let node = if is_break {
            Stmt::Break { label }
        } else {
            Stmt::Continue { label }
        };
        Ok(self.ast.push_node(node))
    }

    pub fn parse_try_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "try");
        let block = self.parse_block_stmt()?;
        let catch = self
            .eat(t!("catch"))
            .then(|| {
                let binding = self
                    .eat(t!("("))
                    .then(|| {
                        let res = self.parse_ident_or_pattern()?;
                        expect!(self, ")");
                        Ok(res)
                    })
                    .transpose()?;
                let block = self.parse_block_stmt()?;
                Ok(self.ast.push_node(CatchStmt { binding, block }))
            })
            .transpose()?;

        let finally = self
            .eat(t!("finally"))
            .then(|| self.parse_block_stmt())
            .transpose()?;

        Ok(self.ast.push_node(Stmt::Try {
            block,
            catch,
            finally,
        }))
    }

    pub fn parse_throw_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "throw");
        peek_expect!(self);
        if self.ate_line_terminator {
            unexpected!(self,t!("\n") => "line terminator not allowed here")
        }
        let expr = self.parse_expr()?;
        self.semicolon()?;
        Ok(self.ast.push_node(Stmt::Throw { expr }))
    }

    pub fn parse_with_stmt(&mut self) -> Result<NodeId<Stmt>> {
        expect!(self, "with");
        if self.state.strict {
            todo!("disallow with in strict mode")
        }
        expect!(self, "(");
        let expr = self.parse_expr()?;
        expect!(self, ")");
        let stmt = self.parse_stmt()?;
        Ok(self.ast.push_node(Stmt::With { expr, stmt }))
    }

    pub fn parse_variable_decl(&mut self, kind: VariableKind) -> Result<NodeId<Stmt>> {
        match kind {
            VariableKind::Let => expect!(self, "let"),
            VariableKind::Const => expect!(self, "const"),
            VariableKind::Var => expect!(self, "var"),
        };

        let decl = self.parse_ident_or_pattern()?;
        let initializer = self
            .eat(t!("="))
            .then(|| self.parse_assignment_expr())
            .transpose()?;
        let decl = self.ast.push_node(VariableDecl { decl, initializer });

        let head = self.ast.append_list(decl, None);
        let mut prev = head;
        while self.eat(t!(",")) {
            let decl = self.parse_ident_or_pattern()?;
            let initializer = self
                .eat(t!("="))
                .then(|| self.parse_assignment_expr())
                .transpose()?;
            let decl = self.ast.push_node(VariableDecl { decl, initializer });
            prev = self.ast.append_list(decl, Some(prev));
        }

        let res = self.ast.push_node(Stmt::VariableDecl { kind, decl: head });

        Ok(res)
    }
}
