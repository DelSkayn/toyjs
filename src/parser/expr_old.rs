use super::error::NumberParseErrorKind;
use super::*;
use crate::token::{BinOpToken, LitToken, NumberKind, TokenKind, UnaryOpToken};
use std::num::ParseIntError;

impl<'a> Parser<'a> {
    pub fn infix_binding_power(kind: TokenKind) -> (u8, u8) {
        match kind {
            tok!("+") | tok!("-") => (1, 2),
            tok!("+") | tok!("/") => (3, 4),
            _ => panic!("invalid token kind"),
        }
    }

    pub fn parse_expr(&mut self, prod_in: bool) -> PResult<'a, Expr<'a>> {
        let mut exprs = vec![self.parse_assignment_expr(prod_in)?];
        while eat!(self, ",") {
            exprs.push(self.parse_assignment_expr(prod_in)?);
        }
        Ok(Expr { exprs })
    }

    pub fn parse_assignment_expr(&mut self, prod_in: bool) -> PResult<'a, AssignExpr<'a>> {
        let mut start = self.parse_assignment_expr_prefix(prod_in)?;
        loop {
            if eat!(self, "++") {
                start = AssignExpr::Post {
                    incr: true,
                    expr: Box::new(start),
                };
                continue;
            }
            if eat!(self, "--") {
                start = AssignExpr::Post {
                    incr: false,
                    expr: Box::new(start),
                };
                continue;
            }
            if is!(self, "=") {
                if let AssignExpr::LhsExpr(lhs) = start {
                    self.next();
                    let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                    return Ok(AssignExpr::Assign { lhs, expr });
                }
                unexpected!( => "cannot assign to rhs expression");
            }
            if let Some(TokenKind::BinOpAssign(op)) = self.peek_kind() {
                if let AssignExpr::LhsExpr(lhs) = start {
                    self.next();
                    let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                    return Ok(AssignExpr::AssignOp { lhs, op, expr });
                }
                unexpected!( => "cannot assign to rhs expression");
            }
            if let Some(TokenKind::BinOp(op)) = self.peek_kind() {
                self.next();
                let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                return Ok(AssignExpr::BinOp {
                    lhs: Box::new(start),
                    op,
                    expr,
                });
            }
            if let Some(TokenKind::Relation(rel)) = self.peek_kind() {
                self.next();
                let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                return Ok(AssignExpr::Relation {
                    lhs: Box::new(start),
                    rel,
                    expr,
                });
            }
            if eat!(self, "instanceof") {
                let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                return Ok(AssignExpr::Instanceof {
                    lhs: Box::new(start),
                    expr,
                });
            }
            if prod_in && eat!(self, "in") {
                let expr = Box::new(self.parse_assignment_expr(prod_in)?);
                return Ok(AssignExpr::In {
                    lhs: Box::new(start),
                    expr,
                });
            }
            break;
        }
        return Ok(start);
    }

    pub fn parse_assignment_expr_prefix(&mut self, prod_in: bool) -> PResult<'a, AssignExpr<'a>> {
        if eat!(self, "delete") {
            let inner = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Delete(inner));
        }
        if eat!(self, "void") {
            let inner = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Void(inner));
        }
        if eat!(self, "typeof") {
            let inner = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Typeof(inner));
        }
        if eat!(self, "-") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::Negative,
                expr,
            });
        }
        if eat!(self, "+") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::Positive,
                expr,
            });
        }
        if eat!(self, "~") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::BitwiseNot,
                expr,
            });
        }
        if eat!(self, "!") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::Not,
                expr,
            });
        }
        if eat!(self, "++") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::AddOne,
                expr,
            });
        }
        if eat!(self, "--") {
            let expr = Box::new(self.parse_assignment_expr(prod_in)?);
            return Ok(AssignExpr::Unary {
                op: UnaryOpToken::SubractOne,
                expr,
            });
        }
        Ok(AssignExpr::LhsExpr(self.parse_lhs_expr()?))
    }
    pub fn parse_lhs_expr(&mut self) -> PResult<'a, LhsExpr<'a>> {
        let mut start = self.parse_lhs_expr_ll()?;
        loop {
            if eat!(self, "[") {
                let expr = self.parse_expr(true)?;
                expect!(self, "]");
                start = LhsExpr::Index {
                    lhs: Box::new(start),
                    expr,
                };
                continue;
            }
            if eat!(self, ".") {
                let ident = expect!(self, "ident");
                start = LhsExpr::Dot {
                    lhs: Box::new(start),
                    ident,
                };
                continue;
            }
            if is!(self, "(") {
                let args = self.parse_arguments()?;
                start = LhsExpr::Call {
                    lhs: Box::new(start),
                    args,
                };
                continue;
            }
            break;
        }
        return Ok(start);
    }

    pub fn parse_lhs_expr_ll(&mut self) -> PResult<'a, LhsExpr<'a>> {
        trace_log!("lhs expression");
        if eat!(self, "new") {
            if eat!(self, ".") {
                expect!(self, "meta");
                return Ok(LhsExpr::NewTarget);
            }
            return Ok(LhsExpr::New(Box::new(self.parse_lhs_expr()?)));
        }
        if eat!(self, "import") {
            if eat!(self, ".") {
                expect!(self, "target");
                return Ok(LhsExpr::ImportMeta);
            }
            expect!(self, "(");
            let res = Ok(LhsExpr::Import(Box::new(self.parse_assignment_expr(true)?)));
            expect!(self, ")");
            return res;
        }
        if eat!(self, "super") {
            if eat!(self, ".") {
                let token = expect!(self, "ident");
                return Ok(LhsExpr::SuperDot(token));
            }
            if eat!(self, "[") {
                let expr = self.parse_expr(true)?;
                expect!(self, "]");
                return Ok(LhsExpr::SuperIdx(expr));
            }
        }
        return Ok(LhsExpr::Prime(self.parse_primary_expr()?));
    }
}
