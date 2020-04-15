use super::*;
use crate::token::{LitToken, NumberKind, TokenKind};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> PResult<'a, Expr<'a>> {
        trace_log!("expression");
        let mut res = vec![self.parse_assignment_expr()?];
        while eat!(self, ",") {
            res.push(self.parse_assignment_expr()?);
        }
        Ok(Expr { exprs: res })
    }

    pub fn parse_assignment_expr(&mut self) -> PResult<'a, AssignExpr<'a>> {
        trace_log!("assignment expression");
        let lhs = self.parse_lhs_expr()?;
        if eat!(self, "=") {
            let expr = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::Assign { lhs, expr });
        }
        if let Some(TokenKind::BinOpAssign(x)) = self.peek().map(|e| e.kind) {
            self.next();
            let expr = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::AssignOp { lhs, op: x, expr });
        }
        if let Some(TokenKind::BinOp(x)) = self.peek().map(|e| e.kind) {
            self.next();
            let expr = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::BinOp { lhs, op: x, expr });
        }
        if eat!(self, "?") {
            let left = Box::new(self.parse_assignment_expr()?);
            expect!(self, ":");
            let right = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::ShortCircuit { left, right });
        }
        if eat!(self, "==") {
            let right = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::Equal {
                strict: false,
                not: false,
                left: Box::new(AssignExpr::Lhs { expr: lhs }),
                right,
            });
        }
        if eat!(self, "!=") {
            let right = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::Equal {
                strict: false,
                not: true,
                left: Box::new(AssignExpr::Lhs { expr: lhs }),
                right,
            });
        }
        if eat!(self, "===") {
            let right = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::Equal {
                strict: true,
                not: false,
                left: Box::new(AssignExpr::Lhs { expr: lhs }),
                right,
            });
        }
        if eat!(self, "!==") {
            let right = Box::new(self.parse_assignment_expr()?);
            return Ok(AssignExpr::Equal {
                strict: true,
                not: true,
                left: Box::new(AssignExpr::Lhs { expr: lhs }),
                right,
            });
        }
        Ok(AssignExpr::Lhs { expr: lhs })
    }

    pub fn parse_lhs_expr(&mut self) -> PResult<'a, LhsExpr<'a>> {
        trace_log!("lhs expression");
        let mut root = match self.parse_ll_lhs_expr()? {
            Some(x) => x,
            None => LhsExpr::Prime(self.parse_primary_expr()?),
        };

        loop {
            trace!("lhs loop");
            if eat!(self, "[") {
                let idx = self.parse_expr()?;
                expect!(self, "]");
                root = LhsExpr::Index {
                    on: Box::new(root),
                    idx,
                };
            } else if eat!(self, ".") {
                if !is!(self, "ident") {
                    unexpected!(self, "ident");
                }
                root = LhsExpr::Dot {
                    on: Box::new(root),
                    ident: self.next().unwrap(),
                };
            } else if is!(self, "(") {
                let args = self.parse_arguments()?;
                root = LhsExpr::Call {
                    on: Box::new(root),
                    args,
                };
            } else {
                break;
            }
        }
        Ok(root)
    }

    pub fn parse_ll_lhs_expr(&mut self) -> PResult<'a, Option<LhsExpr<'a>>> {
        if eat!(self, "super") {
            if eat!(self, "[") {
                let idx = self.parse_expr()?;
                expect!(self, "]");
                return Ok(Some(LhsExpr::SuperIndex { idx }));
            }
            if eat!(self, ".") {
                if !is!(self, "ident") {
                    unexpected!(self, "ident");
                }
                return Ok(Some(LhsExpr::SuperDot {
                    ident: self.next().unwrap(),
                }));
            }
            if is!(self, "(") {
                let args = self.parse_arguments()?;
                return Ok(Some(LhsExpr::SuperCall { args }));
            }
            unexpected!(self, "[", ".", "(")
        } else if eat!(self, "new") {
            //TODO parse "new new bla(x)" correctly
            if eat!(self, ".") {
                expect!(self, "target");
                return Ok(Some(LhsExpr::NewTarget));
            }
            let target = Box::new(self.parse_lhs_expr()?);
            return Ok(Some(LhsExpr::New { target }));
        } else if eat!(self, "import") {
            if is!(self, ".") {
                expect!(self, ".");
                expect!(self, "meta");
                return Ok(Some(LhsExpr::ImportMeta));
            }
            if eat!(self, "(") {
                let expr = Box::new(self.parse_assignment_expr()?);
                expect!(self, ")");
                return Ok(Some(LhsExpr::ImportCall { expr }));
            }
        } else if eat!(self, "typeof") {
            let target = Box::new(self.parse_lhs_expr()?);
            return Ok(Some(LhsExpr::Typeof { target }));
        } else if eat!(self, "void") {
            let target = Box::new(self.parse_lhs_expr()?);
            return Ok(Some(LhsExpr::Void { target }));
        } else if eat!(self, "delete") {
            let target = Box::new(self.parse_lhs_expr()?);
            return Ok(Some(LhsExpr::Delete { target }));
        }
        Ok(None)
    }

    pub fn parse_arguments(&mut self) -> PResult<'a, Arguments<'a>> {
        trace_log!("arguments");
        expect!(self, "(");
        let mut args = Vec::new();
        let mut f = true;
        while !eat!(self, ")") {
            if f {
                f = false;
            } else {
                expect!(self, ",");
            }
            args.push(self.parse_assignment_expr()?);
        }
        Ok(Arguments { args })
    }

    pub fn parse_primary_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        trace_log!("primary expression");
        if eat!(self, "this") {
            return Ok(PrimeExpr::This);
        }
        if eat!(self, "null") {
            return Ok(PrimeExpr::Null);
        }
        if eat!(self, "false") {
            return Ok(PrimeExpr::Boolean(false));
        }
        if eat!(self, "true") {
            return Ok(PrimeExpr::Boolean(true));
        }
        if eat!(self, "string") {
            return Ok(PrimeExpr::String(self.cur_string()));
        }
        if is!(self, "[") {
            // Array literal
            return self.parse_array_expr();
        }
        if is!(self, "{") {
            // Object literal
            return self.parse_object_expr();
        }
        if eat!(self, "(") {
            if eat!(self, ")") {
                return Ok(PrimeExpr::ParamList {
                    expr: None,
                    rest: None,
                });
            }
            if eat!(self, "...") {
                return Ok(PrimeExpr::ParamList {
                    expr: None,
                    rest: Some(self.parse_binding()?),
                });
            }
            let expr = Some(self.parse_expr()?);
            let rest = if eat!(self, ",") && eat!(self, "...") {
                Some(self.parse_binding()?)
            } else {
                None
            };
            expect!(self, ")");
            return Ok(PrimeExpr::ParamList { expr, rest });
        }
        if eat!(self, "function") {
            let generator = eat!(self, "*");
            let binding = if is!(self, "ident") {
                Some(self.next().unwrap())
            } else {
                None
            };
            let params = self.parse_params()?;
            let block = self.parse_block_stmt()?;
            if generator {
                return Ok(PrimeExpr::Generator {
                    binding,
                    params,
                    block,
                });
            }
            return Ok(PrimeExpr::Function {
                binding,
                params,
                block,
            });
        }
        if eat!(self, "class") {
            // class expression
            to_do!(self)
        }
        if eat!(self, "/") {
            // regular expression
            to_do!(self)
        }
        if is!(self, "number") {
            let number = self.peek().unwrap();
            let (big, kind) = match number.kind {
                TokenKind::Lit(LitToken::Number { big, kind }) => (big, kind),
                _ => unreachable!(),
            };
            if big {
                to_do!(self);
            }
            let num = match kind {
                NumberKind::Float => Number::Float(match self.cur_string().parse() {
                    Ok(x) => x,
                    _ => to_do!(self),
                }),
                _ => Number::Integer(match self.cur_string().parse() {
                    Ok(x) => x,
                    e => {
                        error!("{:?}", e);
                        error!("{}", self.cur_string());
                        to_do!(self)
                    }
                }),
            };
            self.next();
            return Ok(PrimeExpr::Number(num));
        }
        if is!(self, "ident") {
            return Ok(PrimeExpr::Ident {
                token: self.next().unwrap(),
            });
        }
        to_do!(self)
    }

    pub fn parse_object_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        trace_log!("object expression");
        expect!(self, "{");
        let mut properties = Vec::new();
        while !eat!(self, "}") {
            if properties.len() != 0 {
                expect!(self, ",");
            }
            if eat!(self, "[") {
                let idx = self.parse_expr()?;
                expect!(self, "]");
                expect!(self, ":");
                let expr = self.parse_assignment_expr()?;
                properties.push(Property::Computed { idx, expr });
                continue;
            }
            if is!(self, "ident") {
                let token = self.next().unwrap();
                if eat!(self, ":") {
                    properties.push(Property::IdentAssign {
                        idx: token,
                        expr: self.parse_assignment_expr()?,
                    });
                } else {
                    properties.push(Property::Ident {
                        token: self.next().unwrap(),
                    });
                }
                continue;
            }
            if eat!(self, "}") {
                break;
            }
            to_do!(self)
        }
        Ok(PrimeExpr::ObjectLiteral { properties })
    }

    pub fn parse_array_expr(&mut self) -> PResult<'a, PrimeExpr<'a>> {
        trace_log!("array expression");

        expect!(self, "[");
        let mut elems = Vec::new();
        let mut had_comma = false;
        while !eat!(self, "]") {
            if eat!(self, ",") {
                elems.push(ArrayElement::Elision);
            } else if eat!(self, "...") {
                elems.push(ArrayElement::Spread {
                    expr: self.parse_assignment_expr()?,
                });
                had_comma = eat!(self, ",");
            } else {
                elems.push(ArrayElement::Expr {
                    expr: self.parse_assignment_expr()?,
                });
                had_comma = eat!(self, ",");
            }
        }
        if had_comma {
            elems.push(ArrayElement::Elision);
        }
        Ok(PrimeExpr::ArrayLiteral { elems })
    }
}
