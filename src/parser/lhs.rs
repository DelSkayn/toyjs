use super::error::NumberParseErrorKind;
use super::*;
use crate::token::{BinOpToken, LitToken, NumberKind, TokenKind, UnaryOpToken};
use std::num::ParseIntError;

pub enum TLhsExpr<'a> {
    New(Box<TLhsExpr<'a>>),
    Import(AssignExpr<'a>),
    SuperCall(Arguments<'a>),
    SuperIdx(Expr<'a>),
    SuperDot(Token<'a>),
    ImportMeta,
    NewTarget,
}

pub enum TAssignExpr<'a> {
    Delete(Box<TAssignExpr<'a>>),
    Void(Box<TAssignExpr<'a>>),
    Typeof(Box<TAssignExpr<'a>>),
    Positive(Box<TAssignExpr<'a>>),
    Negative(Box<TAssignExpr<'a>>),
    BinaryNot(Box<TAssignExpr<'a>>),
    Not(Box<TAssignExpr<'a>>),
    Unary {
        op: UnaryOpToken,
        expr: Box<TAssignExpr<'a>>,
    },
    Post {
        incr: bool,
        expr: Box<TAssignExpr<'a>>,
    },
    LhsExpr(TLhsExpr<'a>),
    Assign {
        lhs: TLhsExpr<'a>,
        expr: Box<TAssignExpr<'a>>,
    },
    AssignOp {
        lhs: TLhsExpr<'a>,
        op: BinOpToken,
        expr: Box<TAssignExpr<'a>>,
    },
    BinOp {
        lhs: Box<TAssignExpr<'a>>,
        op: BinOpToken,
        expr: Box<TAssignExpr<'a>>,
    },
    Instanceof {
        lhs: Box<TAssignExpr<'a>>,
        expr: Box<TAssignExpr<'a>>,
    },
    In {
        lhs: Box<TAssignExpr<'a>>,
        expr: Box<TAssignExpr<'a>>,
    },
}

impl<'a> Parser<'a> {
    pub fn t_parse_assignment_expr(&mut self) -> PResult<'a, TAssignExpr<'a>> {
        let mut start = self.parse_assignment_expr_ll()?;
        loop {
            if eat!(self, "++") {
                start = TAssignExpr::Post {
                    incr: true,
                    expr: Box::new(start),
                };
                continue;
            }
            if eat!(self, "--") {
                start = TAssignExpr::Post {
                    incr: false,
                    expr: Box::new(start),
                };
                continue;
            }
            if is!(self, "=") {
                if let TAssignExpr::LhsExpr(lhs) = start {
                    self.next();
                    let expr = Box::new(self.t_parse_assignment_expr()?);
                    return Ok(TAssignExpr::Assign { lhs, expr });
                }
                unexpected!( => "cannot assign to rhs expression");
            }
            if let Some(TokenKind::BinOpAssign(op)) = self.peek_kind() {
                if let TAssignExpr::LhsExpr(lhs) = start {
                    self.next();
                    let expr = Box::new(self.t_parse_assignment_expr()?);
                    return Ok(TAssignExpr::AssignOp { lhs, op, expr });
                }
                unexpected!( => "cannot assign to rhs expression");
            }
            if let Some(TokenKind::BinOp(op)) = self.peek_kind() {
                self.next();
                let expr = Box::new(self.t_parse_assignment_expr()?);
                return Ok(TAssignExpr::BinOp {
                    lhs: Box::new(start),
                    op,
                    expr,
                });
            }
            if eat!(self, "instanceof") {
                let expr = Box::new(self.t_parse_assignment_expr()?);
                return Ok(TAssignExpr::Instanceof {
                    lhs: Box::new(start),
                    expr,
                });
            }
            if eat!(self, "in") {
                let expr = Box::new(self.t_parse_assignment_expr()?);
                return Ok(TAssignExpr::In {
                    lhs: Box::new(start),
                    expr,
                });
            }
            break;
        }
        return Ok(start);
    }

    pub fn parse_assignment_expr_ll(&mut self) -> PResult<'a, TAssignExpr<'a>> {
        if eat!(self, "delete") {
            let inner = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Delete(inner));
        }
        if eat!(self, "void") {
            let inner = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Void(inner));
        }
        if eat!(self, "typeof") {
            let inner = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Typeof(inner));
        }
        if eat!(self, "-") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::Negative,
                expr,
            });
        }
        if eat!(self, "+") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::Positive,
                expr,
            });
        }
        if eat!(self, "~") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::BitwiseNot,
                expr,
            });
        }
        if eat!(self, "!") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::Not,
                expr,
            });
        }
        if eat!(self, "++") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::AddOne,
                expr,
            });
        }
        if eat!(self, "--") {
            let expr = Box::new(self.t_parse_assignment_expr()?);
            return Ok(TAssignExpr::Unary {
                op: UnaryOpToken::SubractOne,
                expr,
            });
        }
        Ok(TAssignExpr::LhsExpr(self.parse_lhs_expr()?))
    }

    pub fn parse_lhs_expr(&mut self) -> PResult<'a, TLhsExpr<'a>> {
        trace_log!("lhs expression");
        if eat!(self, "new") {
            if eat!(self, ".") {
                expect!(self, "meta");
                return Ok(TLhsExpr::NewTarget);
            }
            return Ok(TLhsExpr::New(Box::new(self.parse_lhs_expr()?)));
        }
        if eat!(self, "import") {
            if eat!(self, ".") {
                expect!(self, "target");
                return Ok(TLhsExpr::ImportMeta);
            }
            expect!(self, "(");
            let res = Ok(TLhsExpr::Import(self.parse_assignment_expr()?));
            expect!(self, ")");
            return res;
        }
        if eat!(self, "super") {
            if eat!(self, ".") {
                let token = expect!(self, "ident");
                return Ok(TLhsExpr::SuperDot(token));
            }
            if eat!(self, "[") {
                let expr = self.parse_expr()?;
                expect!(self, "]");
                return Ok(TLhsExpr::SuperIdx(expr));
            }
        }
        to_do!(self);
    }
}
