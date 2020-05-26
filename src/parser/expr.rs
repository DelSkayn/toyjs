use super::error::NumberParseErrorKind;
use super::*;
use crate::ast::AssignExpr as Op;
use crate::token::{BinOpToken, LitToken, NumberKind, TokenKind, UnaryOpToken};
use std::num::ParseIntError;

pub enum Operand<'a> {
    Ident(Token<'a>),
    Super,
    Import,
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        tok!("=")
        | tok!("*=")
        | tok!("/=")
        | tok!("%=")
        | tok!("+=")
        | tok!("-=")
        | tok!("<<=")
        | tok!(">>=")
        | tok!(">>>=")
        | tok!("&=")
        | tok!("^=")
        | tok!("|=")
        | tok!("**=") => Some((1, 2)),
        tok!("?") => Some((4, 3)),
        tok!("??") => Some((6, 5)),
        tok!("||") => Some((7, 8)),
        tok!("&&") => Some((9, 10)),
        tok!("|") => Some((11, 12)),
        tok!("^") => Some((13, 14)),
        tok!("&") => Some((15, 16)),
        tok!("==") | tok!("!=") | tok!("===") | tok!("!==") => Some((17, 18)),
        tok!("<") | tok!(">") | tok!("<=") | tok!(">=") | tok!("instanceof") | tok!("in") => {
            Some((19, 20))
        }
        tok!("<<") | tok!(">>") | tok!(">>>") => Some((21, 22)),
        tok!("+") | tok!("-") => Some((23, 24)),
        tok!("*") | tok!("%") | tok!("/") => Some((25, 26)),
        tok!("**") => Some((27, 28)),
        tok!("?.") => Some((36, 35)),
        tok!(".") => Some((38, 37)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        tok!("++") | tok!("--") => Some((31, ())),
        tok!("[") | tok!("(") => Some((38, ())),
        _ => None,
    }
}

fn prefix_binding_power(kind: TokenKind) -> Option<((), u8)> {
    match kind {
        tok!("delete")
        | tok!("void")
        | tok!("typeof")
        | tok!("+")
        | tok!("-")
        | tok!("~")
        | tok!("!") => Some(((), 29)),
        tok!("++") | tok!("--") => Some(((), 31)),
        tok!("new") => Some(((), 33)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    pub fn parse_lhs_expr(&mut self) -> PResult<'a, AssignExpr> {
        to_do!(self)
    }

    pub fn parse_expr_with_in(&mut self) -> PResult<'a, Expr> {
        let old = self.state._in;
        self.state._in = true;
        let res = self.parse_expr();
        self.state._in = old;
        res
    }

    pub fn parse_expr(&mut self) -> PResult<'a, Expr> {
        let mut exprs = vec![self.parse_assignment_expr()?];
        while eat!(self, ",") {
            exprs.push(self.parse_assignment_expr()?);
        }
        Ok(Expr { exprs })
    }

    pub fn parse_assignment_expr(&mut self) -> PResult<'a, Op> {
        self.parse_ops(0)
    }

    pub fn parse_ops(&mut self, min_bp: u8) -> PResult<'a, Op> {
        trace_log!("assign expr");
        let mut lhs =
            if let Some(((), r_bp)) = self.peek_kind().and_then(|x| prefix_binding_power(x)) {
                let op = PrefixOp::from_token(self.next().unwrap()).unwrap();
                match op {
                    PrefixOp::New => {
                        if eat!(self, ".") {
                            expect!(self, "meta");
                            Op::NewTarget
                        } else {
                            let expr = Box::new(self.parse_ops(r_bp)?);
                            Op::Prefix {
                                op: PrefixOp::New,
                                expr,
                            }
                        }
                    }
                    op => {
                        let expr = Box::new(self.parse_ops(r_bp)?);
                        Op::Prefix { op, expr }
                    }
                }
            } else {
                let exp = self.parse_primary_expr()?;
                Op::Prime(exp)
            };

        loop {
            let op = match self.peek_kind() {
                Some(x) => x,
                None => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                if eat!(self, "[") {
                    let expr = self.parse_expr()?;
                    expect!(self, "]");
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: PostfixOp::Index(expr),
                    };
                } else if is!(self, "(") {
                    let args = self.parse_arguments()?;
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: PostfixOp::Call(args),
                    };
                } else {
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: if eat!(self, "++") {
                            PostfixOp::Increment
                        } else if eat!(self, "--") {
                            PostfixOp::Decrement
                        } else {
                            panic!(
                                "encountered postfix operator with binding power but no production"
                            )
                        },
                    };
                }
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                if eat!(self, "?") {
                    let mhs = self.parse_ops(0)?;
                    expect!(self, ":");
                    let rhs = self.parse_ops(r_bp)?;
                    lhs = Op::Bin {
                        lhs: Box::new(lhs),
                        op: BinOp::Tenary(Box::new(mhs)),
                        rhs: Box::new(rhs),
                    };
                } else if eat!(self, "[") {
                } else {
                    let op = BinOp::from_token(self.peek().unwrap()).unwrap();
                    match op {
                        BinOp::Assign(_) => {
                            if !lhs.is_assign_lhs() {
                                unexpected!(self => "left-hand side is not assignable")
                            }
                        }
                        _ => {}
                    }
                    self.next();
                    let rhs = self.parse_ops(r_bp)?;
                    lhs = Op::Bin {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    }
                }
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    pub fn parse_operand(&mut self) -> PResult<'a, Op> {
        if eat!(self, "super") {
            return Ok(Op::Super);
        }
        if eat!(self, "import") {
            if eat!(self, ".") {
                expect!(self, "meta");
                return Ok(Op::ImportMeta);
            }
            return Ok(Op::Import);
        }
        Ok(Op::Prime(self.parse_primary_expr()?))
    }
}
