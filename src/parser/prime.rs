use super::error::NumberParseErrorKind;
use super::*;
use crate::token::{LitToken, NumberKind, TokenKind};
use std::num::ParseIntError;

impl<'a> Parser<'a> {
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
                NumberKind::Integer => self
                    .cur_string()
                    .parse()
                    .map(Number::Integer)
                    .or_else(|_| self.cur_string().parse().map(Number::Float))
                    .or_else(|e| {
                        syntax_error!(
                            self,
                            ParseErrorKind::NumberParseError {
                                kind: NumberParseErrorKind::Float(e)
                            }
                        )
                    })?,
                NumberKind::Hex => u32::from_str_radix(&self.cur_string()[2..], 16)
                    .map(Number::Integer)
                    .or_else(|e| {
                        syntax_error!(
                            self,
                            ParseErrorKind::NumberParseError {
                                kind: NumberParseErrorKind::Integer(e)
                            }
                        )
                    })?,
                NumberKind::Octal => u32::from_str_radix(&self.cur_string()[2..], 8)
                    .map(Number::Integer)
                    .or_else(|e| {
                        syntax_error!(
                            self,
                            ParseErrorKind::NumberParseError {
                                kind: NumberParseErrorKind::Integer(e)
                            }
                        )
                    })?,
                NumberKind::Binary => u32::from_str_radix(&self.cur_string()[2..], 2)
                    .map(Number::Integer)
                    .or_else(|e| {
                        syntax_error!(
                            self,
                            ParseErrorKind::NumberParseError {
                                kind: NumberParseErrorKind::Integer(e)
                            }
                        )
                    })?,
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

    pub fn parse_arguments(&mut self) -> PResult<'a, Arguments<'a>> {
        expect!(self, "(");
        let mut args = Vec::new();
        let mut rest = None;
        while !eat!(self, ")") {
            if eat!(self, "...") {
                rest = Some(Box::new(self.parse_assignment_expr()?));
                expect!(self, ")");
                break;
            }
            args.push(self.parse_assignment_expr()?);
            if !eat!(self, ",") {
                if !is!(self, ")") {
                    unexpected!(self,")" => "maybe missing a comma?");
                }
            }
        }
        Ok(Arguments { args, rest })
    }
}
