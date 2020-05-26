use super::error::NumberParseErrorKind;
use super::*;
use crate::token::{LitToken, NumberKind, TokenKind};
use std::num::ParseIntError;

impl<'a> Parser<'a> {
    pub fn parse_primary_expr(&mut self) -> PResult<'a, PrimeExpr> {
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
        if is!(self, "string") {
            return Ok(PrimeExpr::Literal(self.parse_literal()?));
        }
        if is!(self, "[") {
            // Array literal
            return self.parse_array_expr();
        }
        if is!(self, "{") {
            // Object literal
            return self.parse_object_expr();
        }
        if is!(self, "class") {
            return Ok(PrimeExpr::Class(Box::new(self.parse_class(false)?)));
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
            let binding = self.next_ident();
            let params = self.parse_params()?;
            let block = self.alter_state(|x| x._return = true, |this| this.parse_block_stmt())?;
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
        if eat!(self, "/") {
            // regular expression
            to_do!(self)
        }
        if is!(self, "number") {
            return Ok(PrimeExpr::Literal(self.parse_literal()?));
        }
        if let Some(x) = self.next_ident() {
            return Ok(PrimeExpr::Ident(x));
        }
        unexpected!(self)
    }

    pub fn parse_object_expr(&mut self) -> PResult<'a, PrimeExpr> {
        trace_log!("object expression");
        expect!(self, "{");
        let mut properties = Vec::new();
        while !eat!(self, "}") {
            if properties.len() != 0 {
                expect!(self, ",");
            }
            if eat!(self, "}") {
                break;
            }
            if eat!(self, "...") {
                properties.push(Property::Rest {
                    expr: self.parse_assignment_expr()?,
                });
                continue;
            }
            let name = self.parse_property_name()?;
            if eat!(self, ":") {
                let expr = self.parse_assignment_expr()?;
                properties.push(Property::Prop { name, expr });
                continue;
            }
            // Check for method definitions
            if is!(self, "(") {
                let params = self.parse_params()?;
                let block =
                    self.alter_state(|x| x._return = true, |this| this.parse_block_stmt())?;
                properties.push(Property::Method(Method {
                    ty: MethodType::Normal,
                    is_static: false,
                    name,
                    block,
                    params,
                }));
                continue;
            }
            if is!(self, ",") || is!(self, "}") {
                match name {
                    PropertyName::Ident(x) => {
                        properties.push(Property::Ident(x));
                        continue;
                    }
                    _ => unexpected!(self,":","method" => "expected method or initializer"),
                }
            }
            let ty = if let PropertyName::Ident(x) = name {
                if x.0 == "get" {
                    MethodType::Getter
                } else if x.0 == "set" {
                    MethodType::Setter
                } else {
                    unexpected!(self)
                }
            } else {
                unexpected!(self)
            };
            let name = self.parse_property_name()?;
            let params = if ty == MethodType::Getter {
                expect!(self, "(");
                expect!(self,")" => "getters dont have parameters");
                Parameters::default()
            } else {
                self.parse_params()?
            };
            let block = self.parse_block_stmt()?;
            properties.push(Property::Method(Method {
                name,
                params,
                is_static: false,
                ty,
                block,
            }))
        }
        Ok(PrimeExpr::ObjectLiteral { properties })
    }

    pub fn parse_array_expr(&mut self) -> PResult<'a, PrimeExpr> {
        trace_log!("array expression");

        expect!(self, "[");
        let mut elems = Vec::new();
        let mut rest = None;
        while !eat!(self, "]") {
            if elems.len() != 0 {
                expect!(self, ",");
            }
            if eat!(self, "]") {
                break;
            }
            if is!(self, ",") {
                elems.push(ArrayElement::Elision);
                continue;
            }
            if eat!(self, "...") {
                rest = Some(Box::new(self.parse_assignment_expr()?));
                expect!(self,"]" => "spread element must be the last element");
                break;
            }
            elems.push(ArrayElement::Expr {
                expr: Box::new(self.parse_assignment_expr()?),
            });
        }
        Ok(PrimeExpr::ArrayLiteral { elems, rest })
    }

    pub fn parse_arguments(&mut self) -> PResult<'a, Arguments> {
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
