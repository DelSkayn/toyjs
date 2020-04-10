use super::*;
use crate::token::{LitToken, NumberKind, TokenKind};

impl<'a> Parser<'a> {
    pub fn parse_assignment_expr(&mut self) -> PResult<'a, AssignExpr<'a>> {
        trace_log!("assignment expression");
        if eat!(self, "new") {
            return Ok(AssignExpr::New {
                expr: Box::new(self.parse_assignment_expr()?),
            });
        }

        let left = self.parse_primary_expr()?;
        if eat!(self, ".") {
            let right = self.parse_assignment_expr()?;
            match right {
                AssignExpr::Prime {
                    kind: PrimeExpr::Ident { token },
                } => {
                    return Ok(AssignExpr::In {
                        left: Box::new(AssignExpr::Prime { kind: left }),
                        right: token,
                    });
                }
                AssignExpr::In { left: _, right: _ } => to_do!(self),
                x => {
                    dbg!(left);
                    dbg!(x);
                    to_do!(self);
                }
            }
        }
        if is!(self, "(") {
            let args = self.parse_arguments()?;
            return Ok(AssignExpr::Call {
                expr: Box::new(AssignExpr::Prime { kind: left }),
                args,
            });
        }

        return Ok(AssignExpr::Prime { kind: left });
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
        if eat!(self, "[") {
            // Array literal
            to_do!(self)
        }
        if eat!(self, "{") {
            // Object literal
            to_do!(self)
        }
        if eat!(self, "(") {
            // Array param list
            // or cover parenthesized expresion
            to_do!(self)
        }
        if eat!(self, "function") {
            let binding = if is!(self, "ident") {
                Some(self.next().unwrap())
            } else {
                None
            };
            let params = self.parse_params()?;
            let block = self.parse_block_stmt()?;
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
            let number = self.next().unwrap();
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
                    _ => to_do!(self),
                }),
            };
            return Ok(PrimeExpr::Number(num));
        }
        if is!(self, "ident") {
            return Ok(PrimeExpr::Ident {
                token: self.next().unwrap(),
            });
        }
        to_do!(self)
    }
}
