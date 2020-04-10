use super::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        trace_log!("statement");
        if is!(self, "{") {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }
        if is!(self, "var") {
            return Ok(Stmt::Declaration {
                kind: self.parse_decl()?,
            });
        }
        if eat!(self, ";") {
            return Ok(Stmt::Empty);
        }
        if eat!(self, "throw") {
            no_lt!(self);
            let mut exprs = Vec::new();
            loop {
                exprs.push(self.parse_assignment_expr()?);
                if !eat!(self, ",") {
                    break;
                }
            }
            return Ok(Stmt::Throw { exprs });
        }
        if eat!(self, "try") {
            let block = self.parse_block_stmt()?;
            let catch = if eat!(self, "catch") {
                let bindings = if eat!(self, "(") {
                    Some(self.parse_binding()?)
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
        self.parse_assignment_expr()?;
        to_do!(self)
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

    pub fn parse_decl(&mut self) -> PResult<'a, DeclKind<'a>> {
        trace_log!("declaration");
        if eat!(self, "var") {
            let kind = LexicalKind::Var;
            let mut decl = Vec::new();
            loop {
                let binding = self.parse_binding()?;
                let initializer = match binding {
                    Binding::ArrayPattern | Binding::ObjectPattern => {
                        expect!(self, "=");
                        Some(self.parse_assignment_expr()?)
                    }
                    _ => {
                        if eat!(self, "=") {
                            Some(self.parse_assignment_expr()?)
                        } else {
                            None
                        }
                    }
                };
                decl.push(LexicalDecl {
                    binding,
                    initializer,
                });
                if !eat!(self, ",") {
                    break;
                }
            }
            return Ok(DeclKind::Lexical { kind, decl });
        }
        to_do!(self)
    }

    pub fn parse_binding(&mut self) -> PResult<'a, Binding<'a>> {
        trace_log!("binding");
        if is!(self, "[") {
            let _res = self.parse_array_binding();
        }
        if is!(self, "{") {
            let _res = self.parse_object_binding();
        }
        if is!(self, "ident") {
            let ident = self.next().unwrap();
            return Ok(Binding::Ident { ident });
        }
        unexpected!(self, "[", "{", "ident")
    }

    pub fn parse_array_binding(&mut self) -> PResult<'a, Binding<'a>> {
        to_do!(self)
    }
    pub fn parse_object_binding(&mut self) -> PResult<'a, Binding<'a>> {
        to_do!(self)
    }

    pub fn parse_params(&mut self) -> PResult<'a, Parameters<'a>> {
        trace_log!("parse: parameters");
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
        Ok(Parameters { params, rest })
    }
}
